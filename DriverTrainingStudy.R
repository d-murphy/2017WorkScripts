### Driver Training study
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(readxl)

DriverHist <- data.frame(`EmpID`=character(0),Month = numeric(0), Year = numeric(0))


Driver <- EmpDir %>% select(`Personnel No.`, CC , `Is Driver Final?`)
temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & Calc_Year > 2015) %>%
                   select(`Driver Employee ID`)
Driver$`Is Driver Final?` <- ifelse(Driver$`Personnel No.` %in% temp, "Yes", Driver$`Is Driver Final?` )
Driver <- Driver %>% filter(`Is Driver Final?` == "Yes") %>% select(`Personnel No.`, CC)
colnames(Driver) <- c("EmpID", "CC")

for(i in 2015:2017){

  if(i %in% c(2015,2016)){
  for(j in 1:12){

    Driver$Month <- j
    Driver$Year <- i
    DriverHist <- bind_rows(DriverHist, Driver)
  }
  }else{
  for(j in 1:10){
      
    Driver$Month <- j
    Driver$Year <- i
    DriverHist <- bind_rows(DriverHist, Driver)
  }
  }
}

MVActJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & Calc_Year > 2014) %>%
                                 group_by(`Driver Employee ID`, Calc_Month, Calc_Year) %>%
                                 summarise(MVAct = n())
colnames(MVActJoin) <- c("EmpID", "Month","Year", "MVAct")


DriverHist <- left_join(DriverHist, MVActJoin, by = c("EmpID" = "EmpID", "Month" = "Month","Year" = "Year"))

MVActJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                                 Calc_Year > 2014 &
                                 Calc_CrashResp == "PS Vehicle") %>%
  group_by(`Driver Employee ID`, Calc_Month, Calc_Year) %>%
  summarise(MVAct = n())
colnames(MVActJoin) <- c("EmpID", "Month","Year", "AtFaultMVAct")

DriverHist <- left_join(DriverHist, MVActJoin, by = c("EmpID" = "EmpID", "Month" = "Month","Year" = "Year"))

DriverHist$MonthSince2014 <- DriverHist$Month + (DriverHist$Year - 2014)*12

### Next 35 lines to secure miles per driver

### Turned off - safer to do averages by # of people

# DriverCcCount <- EmpDir %>% 
#                       filter(`Is Driver Final?` == "Yes") %>%
#                       group_by(CC) %>%
#                       summarise(CCdriverCount = n())
# 
# CCMileage <- data.frame(CC=numeric(0),Miles = numeric(0), Year = numeric(0), Month = numeric(0))
# 
# for(i in 2016:2017){
# 
#   if(i==2016){
#     
#   for(j in 1:12){
#     
#     filename <- if(j<10){paste0(i,"0",j)}else{paste0(i,j)}
#     new <- read_excel(paste0("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/",filename,".xlsx"))
#     
#     new <- new %>% mutate(Miles = `Company Mileage` + `Personal Mileage`) %>%
#                    select(`Cost Center`, Miles, Year, Month)
#     colnames(new) <- c("CC", "Miles", "Year", "Month")
#     new$CC <- as.numeric(substring(new$`CC`,0,4))
#     
#     CCMileage <- bind_rows(CCMileage, new)
#     }
#   }else{
#     for(j in 1:7){
#       
#       filename <- if(j<10){paste0(i,"0",j)}else{paste0(i,j)}
#       new <- read_excel(paste0("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/",filename,".xlsx"))
#       
#       new <- new %>% mutate(Miles = `Company Mileage` + `Personal Mileage`) %>%
#                      select(`Cost Center`, Miles, Year, Month)
#         colnames(new) <- c("CC", "Miles", "Year", "Month")
#         new$CC <- as.numeric(substring(new$`CC`,0,4))
#       
#         CCMileage <- bind_rows(CCMileage, new)
#     }
#   }  
#     
# }
# 
# CCMileage <- left_join(CCMileage, DriverCcCount, by = "CC")
# 
# CCMileage <- CCMileage %>% mutate(MilesPerDriver = Miles / CCdriverCount)


### Join Mileage Per Driver to Driver History

# DriverHist <- left_join(DriverHist, CCMileage, by = c("CC" = "CC", "Year" = "Year", "Month" = "Month"))
# 
# remove(CCMileage, DriverCcCount, filename, new)


### Load Training again

SafeTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SAFETraining.xlsx")
SafeTraining$`Personnel No.` <- as.character(SafeTraining$`Personnel No.`)
SafeTraining <- SafeTraining %>% select(`Personnel No.`, `Date of SAFE Driver Training`)

DDTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/DDTraining.xlsx")
DDTraining$`Personnel No.` <- as.character(DDTraining$`Personnel No.`)
DDTraining <- DDTraining %>% select(`Personnel No.`, `Date of Defensive Driver Training`)


SmithTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SmithTraining.xlsx")
SmithTraining$`Personnel No.` <- as.character(SmithTraining$`Personnel No.`)
SmithTraining <- SmithTraining %>% select(`Personnel No.`, `Date of Smith Training`)


SmithTraining$MonthOfSmithSince2014 <- month(SmithTraining$`Date of Smith Training`) + 
                       12 * (year(SmithTraining$`Date of Smith Training`) - 2014)
DDTraining$MonthOfDDSince2014 <- month(DDTraining$`Date of Defensive Driver Training`) + 
                       12 * (year(DDTraining$`Date of Defensive Driver Training`) - 2014)
SafeTraining$MonthOfSafeSince2014 <- month(SafeTraining$`Date of SAFE Driver Training`) + 
                       12 * (year(SafeTraining$`Date of SAFE Driver Training`) - 2014)

SmithTraining$`Date of Smith Training` <- NULL
DDTraining$`Date of Defensive Driver Training` <- NULL
SafeTraining$`Date of SAFE Driver Training` <- NULL


DriverHist <- left_join(DriverHist, SmithTraining, by = c("EmpID" = "Personnel No."))
DriverHist <- left_join(DriverHist, DDTraining, by = c("EmpID" = "Personnel No."))
DriverHist <- left_join(DriverHist, SafeTraining, by = c("EmpID" = "Personnel No."))
# 
# DriverHist$MonthsSinceSmith <- ymd(paste0(DriverHist$Year,"-",DriverHist$Month,"-3))

DriverHist$MonthSinceSmith <- DriverHist$MonthSince2014 - DriverHist$MonthOfSmithSince2014
DriverHist$MonthSinceSafe <- DriverHist$MonthSince2014 - DriverHist$MonthOfSafeSince2014
DriverHist$MonthSinceDD <- DriverHist$MonthSince2014 - DriverHist$MonthOfDDSince2014



DriverHist$SmithTrained <- ifelse(is.na(DriverHist$MthsSinceSmith),0,
                                  ifelse(DriverHist$MthsSinceSmith>=0,1,-1))
DriverHist$DDTrained <- ifelse(is.na(DriverHist$MthsSinceDD),0,
                               ifelse(DriverHist$MthsSinceDD>=0, 1,-1))
DriverHist$SafeTrained <- ifelse(is.na(DriverHist$MthsSinceSafe),0,
                                 ifelse(DriverHist$MthsSinceSafe>=0, 1,-1))


GraphMonthlyRate <- function(col, str){

    Results <- DriverHist %>% group_by_(col) %>% 
    summarise(MVAct = sum(MVAct, na.rm = TRUE),
              NumberAtMth = n()) %>% 
    mutate(MonthlyRate = MVAct / NumberAtMth,
           BeforeOrAfter = ifelse(col<=0,"Before Training", "After Training"))
  

    Results %>% filter(NumberAtMth > 150 & NumberAtMth < 5000) %>%
      ggplot(aes_string(x = col, y = "MonthlyRate")) + geom_line(size = 1) +
    geom_vline(xintercept = 0, col="dark blue", size = 1)+
    theme_hc() +
    scale_fill_pander() + 
    labs(y = "# of MVAs / # of Employees", x = "Months Relative to Class") + 
    ggtitle(str)
  
}

GraphMonthlyRate("MonthSinceSmith", "Months Before or After Smith")
GraphMonthlyRate("MonthSinceDD", "Months Before or After Defensive Driver")
GraphMonthlyRate("MonthSinceSafe", "Months Before or After CDT Safe Driver")


DriverHist$HadAccident <- ifelse(is.na(DriverHist$MVAct),"No MVA", "Had MVA")

GraphMonthlyNumWithMVA <- function(col, str){
  
  DriverHist %>% 
    ggplot(aes_string(x = col, fill = "HadAccident")) + geom_bar() +
    theme_hc() +
    scale_fill_tableau() + 
    labs(y = "Count of Employees", x = "Months Relative to Class", fill = "") + 
    ggtitle("")
  
}

GraphMonthlyNumWithMVA("MonthSinceSmith", "Months Before and After Smith")
GraphMonthlyNumWithMVA("MonthSinceDD", "Months Before and After Defensive Driving")
GraphMonthlyNumWithMVA("MonthSinceSafe", "Months Before and After CDT Safe Driver")


DriverHist %>% mutate(AfterDD = if_else(is.na(MonthSinceDD),0,
                              if_else(MonthSinceDD==0, 1,
                              if_else(MonthSinceDD>0,2,-1)))) %>% 
               group_by(AfterDD) %>% 
               summarise(MVATotal = sum(MVAct, na.rm=TRUE), 
                         DriverMonths = n()) %>% 
               mutate(MvaPerDriverMonths = MVATotal / DriverMonths)

DriverHist %>% mutate(AfterSafe = if_else(is.na(MonthSinceSafe),0,
                                        if_else(MonthSinceSafe==0, 1,
                                        if_else(MonthSinceSafe>0,2,-1)))) %>% 
              group_by(AfterSafe) %>% 
              summarise(MVATotal = sum(MVAct, na.rm=TRUE), 
                        DriverMonths = n()) %>% 
              mutate(MvaPerDriverMonths = MVATotal / DriverMonths)









DistOfSmithRates <- DriverHist %>% group_by(MonthSinceSmith) %>% summarise(ct = n(), 
                                                                           MVATot = sum(MVAct, na.rm = TRUE)) %>%
                                   mutate(Rate = MVATot / ct)

DistOfSmithRates$BeforeTraining <- ifelse(DistOfSmithRates$MonthSinceSmith<=0,"Yes","No")
                                                           
ggplot(DistOfSmithRates %>% filter(ct > 100 & ct < 5000), aes(x = Rate, fill = BeforeTraining)) + 
            geom_histogram() +
            theme_hc() +
            scale_fill_pander() + 
            labs(x = "", y = "Count", fill = "Before Training") + 
            ggtitle("Distribution of MVAs per Person in Months Before / After Smith Training")


DistOfDDRates <- DriverHist %>% group_by(MonthSinceDD) %>% summarise(ct = n(), 
                                                                           MVATot = sum(MVAct, na.rm = TRUE)) %>%
  mutate(Rate = MVATot / ct)

DistOfDDRates$BeforeTraining <- ifelse(DistOfDDRates$MonthSinceDD<=0,"Yes","No")

ggplot(DistOfDDRates %>% filter(ct > 100 & ct < 5000), aes(x = Rate, fill = BeforeTraining)) + 
  geom_histogram() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "Before Training") + 
  ggtitle("Distribution of MVAs per Person in Months Before / After DD Training")


DistOfSafeRates <- DriverHist %>% group_by(MonthSinceSafe) %>% summarise(ct = n(), 
                                                                     MVATot = sum(MVAct, na.rm = TRUE)) %>%
  mutate(Rate = MVATot / ct)

DistOfSafeRates$BeforeTraining <- ifelse(DistOfSafeRates$MonthSinceSafe<=0,"Yes","No")

ggplot(DistOfSafeRates %>% filter(ct > 100 & ct < 5000), aes(x = Rate, fill = BeforeTraining)) + 
  geom_histogram() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "Before Training") + 
  ggtitle("Distribution of MVAs per Person in Months Before / After CDT-Safety Training")



###  This looks at counts of MVAs relative to Training


Events$HadSmith <- ifelse(is.na(Events$`EmpDir_Date of Smith Training`), "No Training",
                          ifelse(Events$`EmpDir_Date of Smith Training` < Events$Date, 
                                 "Before MVA", "After MVA"))
Events$HadDD <- ifelse(is.na(Events$`EmpDir_Date of Defensive Driver Training`), "No Training",
                       ifelse(Events$`EmpDir_Date of Defensive Driver Training` < Events$Date, 
                              "Before MVA", "After MVA"))
Events$HadSafeD <- ifelse(is.na(Events$`EmpDir_Date of SAFE Driver Training`), "No Training",
                          ifelse(Events$`EmpDir_Date of SAFE Driver Training` < Events$Date, 
                                 "Before MVA", "After MVA"))

Events$ADEnrolled <- ifelse(is.na(Events$`EmpDir_Hazard Perception Evaluation Status`) &
                            is.na(Events$`EmpDir_Hazard Perception Targeted Training Status`) &
                            is.na(Events$`EmpDir_PSEG LI Driver Training Y2 Status`) &
                            is.na(Events$`EmpDir_PSEG LI Driver Training Y3 Status`), "Not Enrolled", "AD Enrolled")



ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")), 
       aes(Calc_Year, fill = HadDD)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "") +
  guides(fill = FALSE)

ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")), 
       aes(Calc_Year, fill = HadSafeD)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "")+
  guides(fill = FALSE)

ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")), 
       aes(Calc_Year, fill = HadSmith)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "") 

ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")), 
       aes(Calc_Year, fill = ADEnrolled)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "", y = "Count", fill = "") 



Events$Calc_Weekday <- wday(Events$Date)

ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") ), 
       aes(factor(Calc_Weekday, levels = c(1,2,3,4,5,6,7)), fill = factor(1))) + 
  geom_bar(width = .9) + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7), drop = FALSE,
                   labels = c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(x = "", y = "Count", fill = "") + 
  theme_hc() +
  scale_fill_pander() +
  guides(fill = FALSE)



ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & 
                  OrgStruct_Division %in% c("Meter Srvs",
                                            "T&D Oh / Ug",
                                            "T&D Ops",
                                            "T&D Proj & Const",
                                            "T&D Services",
                                            "T&D Substation & Tel")), 
       aes(factor(OrgStruct_Division), fill = Calc_CrashResp)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "Department", y = "Count", fill = "") + 
  facet_wrap( ~ Calc_CrashType, ncol = 4) + 
  scale_x_discrete(breaks = c("Meter Srvs",
                              "T&D Oh / Ug",
                              "T&D Ops",
                              "T&D Proj & Const",
                              "T&D Services",
                              "T&D Substation & Tel"), 
                   labels = c("Meter Srvs",
                              "Oh/Ug",
                              "T&D Ops",
                              "Proj & Const",
                              "T&D Services",
                              "SPT")) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))


ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & 
                EmpDir_Location %in% c("Brentwood", 
                                       "Greenlawn",
                                       "Hewlett",
                                       "Hicksville",
                                       "Patchogue",
                                       "Riverhead",
                                       "Roslyn")), 
       aes(factor(EmpDir_Location), fill = Calc_CrashResp)) + geom_bar() +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "Department", y = "Count", fill = "") + 
  facet_wrap( ~ Calc_CrashType, ncol = 4) + 
  # scale_x_discrete(breaks = c("Meter Srvs",
  #                             "T&D Oh / Ug",
  #                             "T&D Ops",
  #                             "T&D Proj & Const",
  #                             "T&D Services",
  #                             "T&D Substation & Tel"), 
  #                  labels = c("Meter Srvs",
  #                             "Oh/Ug",
  #                             "T&D Ops",
  #                             "Proj & Const",
  #                             "T&D Services",
  #                             "SPT")) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))


ggplot(Events %>%
         filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & 
                  is.na(Fleet_Description) == FALSE & 
                  EmpDir_Location %in% c("Brentwood", 
                                         "Greenlawn",
                                         "Hewlett",
                                         "Hicksville",
                                         "Patchogue",
                                         "Riverhead",
                                         "Roslyn")), 
       aes(factor(Calc_CrashType), fill = Fleet_Class)) + geom_bar(position = "fill") +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "Fleet Desc", y = "Count", fill = "") + 
  facet_wrap(Calc_CrashResp ~ EmpDir_Location, ncol = 7)  +
theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + coord_flip()




DriverHist$SmithClass <- ifelse(DriverHist$MonthSinceSmith < -2 & DriverHist$MonthSinceSmith > -13 , -1, 
                                ifelse(DriverHist$MonthSinceSmith > 0 & DriverHist$MonthSinceSmith < 13 , 1, 0 ))

DriverHist$SafeClass <- ifelse(DriverHist$MonthSinceSafe < 0 & DriverHist$MonthSinceSafe > -13 , -1, 
                               ifelse(DriverHist$MonthSinceSafe > 0 & DriverHist$MonthSinceSafe < 13 , 1, 0 ))

DriverHist$DDClass <- ifelse(DriverHist$MonthSinceDD < 0 & DriverHist$MonthSinceDD > -13 , -1, 
                             ifelse(DriverHist$MonthSinceDD > 0 & DriverHist$MonthSinceDD < 13 , 1, 0 ))

SmithNames <- c("-1" = "Before Smith", "1" = "After Smith")
DDNames <- c("-1" = "Before DD", "1" = "After DD")
SafeNames <- c("-1" = "Before Safe", "1" = "After Safe")



 DriverHist %>% group_by(SmithClass) %>% summarise(ct = n(), MVAct = sum(MVAct, na.rm=TRUE)) %>%
  mutate(Rate = MVAct / ct) %>% filter(SmithClass != 0) %>%
  ggplot(aes(factor(SmithClass), Rate, fill = "1")) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = SmithNames) +
  theme_hc() +
  scale_fill_pander() + 
  labs(x = "") + guides(fill = FALSE)
 
 DriverHist %>% group_by(SmithClass) %>% summarise(ct = n(), MVAct = sum(MVAct, na.rm=TRUE)) %>%
   mutate(Rate = MVAct / ct) %>% filter(SmithClass != 0) %>%
   ggplot(aes(factor(SmithClass), Rate, fill = "1")) + geom_bar(stat = "identity") +
   scale_x_discrete(labels = SmithNames) +
   theme_hc() +
   scale_fill_pander() + 
   labs(x = "") + guides(fill = FALSE)
 
 DriverHist %>% group_by(DDClass) %>% summarise(ct = n(), MVAct = sum(MVAct, na.rm=TRUE)) %>%
   mutate(Rate = MVAct / ct) %>% filter(DDClass != 0) %>%
   ggplot(aes(factor(DDClass), Rate, fill = "1")) + geom_bar(stat = "identity") +
   scale_x_discrete(labels = DDNames) +
   theme_hc() +
   scale_fill_pander() + 
   labs(x = "") + guides(fill = FALSE)
 
 DriverHist %>% group_by(SafeClass) %>% summarise(ct = n(), MVAct = sum(MVAct, na.rm=TRUE)) %>%
   mutate(Rate = MVAct / ct) %>% filter(SafeClass != 0) %>%
   ggplot(aes(factor(SafeClass), Rate, fill = "1")) + geom_bar(stat = "identity") +
   scale_x_discrete(labels = SafeNames) +
   theme_hc() +
   scale_fill_pander() + 
   labs(x = "") + guides(fill = FALSE) 
  



