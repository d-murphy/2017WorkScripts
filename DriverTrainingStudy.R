### Driver Training study
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

DriverHist <- data.frame(`EmpID`=character(0),Month = numeric(0), Year = numeric(0))


Driver <- EmpDir %>% select(`Personnel No.`, CC , `Is Driver Final?`)
temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & Calc_Year > 2015) %>%
                   select(`Driver Employee ID`)
Driver$`Is Driver Final?` <- ifelse(Driver$`Personnel No.` %in% temp, "Yes", Driver$`Is Driver Final?` )
Driver <- Driver %>% filter(`Is Driver Final?` == "Yes") %>% select(`Personnel No.`, CC)
colnames(Driver) <- c("EmpID", "CC")

for(i in 2016:2017){

  if(i==2016){
  for(j in 1:12){

    Driver$Month <- j
    Driver$Year <- i
    DriverHist <- bind_rows(DriverHist, Driver)
  }
  }else{
  for(j in 1:7){
      
    Driver$Month <- j
    Driver$Year <- i
    DriverHist <- bind_rows(DriverHist, Driver)
  }
  }
}

MVActJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & Calc_Year > 2015) %>%
                                 group_by(`Driver Employee ID`, Calc_Month, Calc_Year) %>%
                                 summarise(MVAct = n())
colnames(MVActJoin) <- c("EmpID", "Month","Year", "MVAct")


DriverHist <- left_join(DriverHist, MVActJoin, by = c("EmpID" = "EmpID", "Month" = "Month","Year" = "Year"))

MVActJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                                 Calc_Year > 2015 &
                                 Calc_CrashResp == "PS Vehicle") %>%
  group_by(`Driver Employee ID`, Calc_Month, Calc_Year) %>%
  summarise(MVAct = n())
colnames(MVActJoin) <- c("EmpID", "Month","Year", "AtFaultMVAct")

DriverHist <- left_join(DriverHist, MVActJoin, by = c("EmpID" = "EmpID", "Month" = "Month","Year" = "Year"))

### Next 35 lines to secure miles per driver

DriverCcCount <- EmpDir %>% 
                      filter(`Is Driver Final?` == "Yes") %>%
                      group_by(CC) %>%
                      summarise(CCdriverCount = n())

CCMileage <- data.frame(CC=numeric(0),Miles = numeric(0), Year = numeric(0), Month = numeric(0))

for(i in 2016:2017){

  if(i==2016){
    
  for(j in 1:12){
    
    filename <- if(j<10){paste0(i,"0",j)}else{paste0(i,j)}
    new <- read_excel(paste0("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/",filename,".xlsx"))
    
    new <- new %>% mutate(Miles = `Company Mileage` + `Personal Mileage`) %>%
                   select(`Cost Center`, Miles, Year, Month)
    colnames(new) <- c("CC", "Miles", "Year", "Month")
    new$CC <- as.numeric(substring(new$`CC`,0,4))
    
    CCMileage <- bind_rows(CCMileage, new)
    }
  }else{
    for(j in 1:7){
      
      filename <- if(j<10){paste0(i,"0",j)}else{paste0(i,j)}
      new <- read_excel(paste0("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/",filename,".xlsx"))
      
      new <- new %>% mutate(Miles = `Company Mileage` + `Personal Mileage`) %>%
                     select(`Cost Center`, Miles, Year, Month)
        colnames(new) <- c("CC", "Miles", "Year", "Month")
        new$CC <- as.numeric(substring(new$`CC`,0,4))
      
        CCMileage <- bind_rows(CCMileage, new)
    }
  }  
    
}

CCMileage <- left_join(CCMileage, DriverCcCount, by = "CC")

CCMileage <- CCMileage %>% mutate(MilesPerDriver = Miles / CCdriverCount)


### Join Mileage Per Driver to Driver History

DriverHist <- left_join(DriverHist, CCMileage, by = c("CC" = "CC", "Year" = "Year", "Month" = "Month"))

remove(CCMileage, DriverCcCount, filename, new)


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

### add months since training 



# DriverHist$MthsSinceSmith <- NULL
# 
# DriverHist <- left_join(DriverHist, SmithTraining, by = c("EmpID" = "Personnel No."))
# DriverHist <- left_join(DriverHist, DDTraining, by = c("EmpID" = "Personnel No."))
# DriverHist <- left_join(DriverHist, SafeTraining, by = c("EmpID" = "Personnel No."))
# 
# DriverHist$MonthsSinceSmith <- ymd(paste0(DriverHist$Year,"-",DriverHist$Month,"-3))



DriverHist$MthsSinceSmith <- NA

for(i in 1:dim(DriverHist)[1]){

  if(DriverHist$EmpID[i] %in% SmithTraining$`Personnel No.`){
  
    # ymd(`Date of Smith Training`) <
    #   ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-28")) &
    
  temp <- SmithTraining %>% filter(`Personnel No.` == DriverHist$EmpID[i])
  if(dim(temp)[1]>1){
    temp <- temp %>% arrange(desc(`Date of Smith Training`))
  }

  if(dim(temp)[1]>0){

  DriverHist$MthsSinceSmith[i] <- round((ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-15")) -
                                        ymd(temp$`Date of Smith Training`[1])) / 30 )
    }
  }
}

DriverHist$MthsSinceDD <- NA

for(i in 1:dim(DriverHist)[1]){
  
  if(DriverHist$EmpID[i] %in% DDTraining$`Personnel No.`){
  
  temp <- DDTraining %>% filter(`Personnel No.` == DriverHist$EmpID[i])
  
  if(dim(temp)[1]>1){
    temp <- temp %>% arrange(desc(`Date of Defensive Driver Training`))
  }
  
  if(dim(temp)[1]>0){
    
    DriverHist$MthsSinceDD[i] <- round((ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-15")) -
                                             ymd(temp$`Date of Defensive Driver Training`[1])) / 30 )
    }
  }
}

DriverHist$MthsSinceSafe <- NA

for(i in 1:dim(DriverHist)[1]){
  
  
  if(DriverHist$EmpID[i] %in% SafeTraining$`Personnel No.`){
  
    temp <- SafeTraining %>% filter(`Personnel No.` == DriverHist$EmpID[i])
    
    if(dim(temp)[1]>1){
      temp <- temp %>% arrange(desc(`Date of SAFE Driver Training`))
    }
  
   if(dim(temp)[1]>0){
    
      DriverHist$MthsSinceSafe[i] <- round((ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-15")) -
                                             ymd(temp$`Date of SAFE Driver Training`[1])) / 30 )
    }
  }
}


# DriverHist$MthsSinceMVA <- NA
# 
# MVADriverIDs <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>%
#                                     select(`Driver Employee ID`)
# 
# for(i in 1:dim(DriverHist)[1]){
# 
# 
#   if(DriverHist$EmpID[i] %in% MVADriverIDs$`Driver Employee ID`) {
# 
#  
#     
#     temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
#                               `Driver Employee ID` == DriverHist$EmpID[i] &
#                               Date < ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-01")))  %>%
#                        select(Calc_Year, Calc_Month, Calc_Day, Date)
# 
#   
#     if(dim(temp)[1]>1){
#       temp <- temp %>% arrange(desc(Date))
#     }
# 
#     if(dim(temp)[1]>0){
# 
#       DriverHist$MthsSinceMVA[i] <- round((ymd(paste0(DriverHist$Year[i],"-",DriverHist$Month[i],"-01")) -
#                                               ymd(paste0(temp$Calc_Year[1], "-", temp$Calc_Month[1],"-", temp$Calc_Day[1]))) / 30 )
#     }
#   }
# }




# Check Any Training

# DriverHist$SmithTrained <- ifelse(is.na(DriverHist$MthsSinceSmith),0,1)
# DriverHist$DDTrained <- ifelse(is.na(DriverHist$MthsSinceDD),0,1)
# DriverHist$SafeTrained <- ifelse(is.na(DriverHist$MthsSinceSafe),0,1)

# Check Training with 6 Months

# DriverHist$SmithTrained <- ifelse(is.na(DriverHist$MthsSinceSmith),0,
#                                   ifelse(DriverHist$MthsSinceSmith>=0&DriverHist$MthsSinceSmith<6, 1,0))
# DriverHist$DDTrained <- ifelse(is.na(DriverHist$MthsSinceDD),0,
#                                ifelse(DriverHist$MthsSinceDD>=0&DriverHist$MthsSinceDD<6, 1,0))
# DriverHist$SafeTrained <- ifelse(is.na(DriverHist$MthsSinceSafe),0,
#                                  ifelse(DriverHist$MthsSinceSafe>=0&DriverHist$MthsSinceSafe<6, 1,0))


# Check Training within 2 Months

# DriverHist$SmithTrained <- ifelse(is.na(DriverHist$MthsSinceSmith),0,
#                                   ifelse(DriverHist$MthsSinceSmith>=0&DriverHist$MthsSinceSmith<=2, 1,0))
# DriverHist$DDTrained <- ifelse(is.na(DriverHist$MthsSinceDD),0,
#                                ifelse(DriverHist$MthsSinceDD>=0&DriverHist$MthsSinceDD<=2, 1,0))
# DriverHist$SafeTrained <- ifelse(is.na(DriverHist$MthsSinceSafe),0,
#                                  ifelse(DriverHist$MthsSinceSafe>=0&DriverHist$MthsSinceSafe<=2, 1,0))

# Check if before or after training

DriverHist$SmithTrained <- ifelse(is.na(DriverHist$MthsSinceSmith),0,
                                  ifelse(DriverHist$MthsSinceSmith>=0,1,-1))
DriverHist$DDTrained <- ifelse(is.na(DriverHist$MthsSinceDD),0,
                               ifelse(DriverHist$MthsSinceDD>=0, 1,-1))
DriverHist$SafeTrained <- ifelse(is.na(DriverHist$MthsSinceSafe),0,
                                 ifelse(DriverHist$MthsSinceSafe>=0, 1,-1))



Results <- DriverHist %>% group_by(CC,SmithTrained) %>%
              summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), TotalMVAs = sum(MVAct, na.rm = TRUE)) %>%
              mutate(Rate = TotalMVAs * 1000000 / TotalMiles)

Results <- left_join(Results, CCLut, by = c("CC" = "OrgStruct_CC"))

Results <- Results %>% group_by(OrgStruct_Division, SmithTrained) %>%
             summarise(DivTotalMiles = sum(TotalMiles), DivTotalMVAs = sum(TotalMVAs))

Results <- Results %>% mutate(Rate = DivTotalMVAs * 1000000 / DivTotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/SmithTrainedRates.csv", row.names = FALSE)

Results <- DriverHist %>% group_by(SmithTrained) %>% 
                          summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
                                    TotalMVAs = sum(MVAct, na.rm = TRUE),
                                    TotalAtFaultMVAS = sum(AtFaultMVAct, na.rm = TRUE)) %>%
                          mutate(Rate = TotalMVAs * 1000000 / TotalMiles,
                                 AtFaultMvaRate = TotalAtFaultMVAS * 1000000 / TotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/BinarySmithTrainedRates.csv", row.names = FALSE)

GraphMonthlyRate <- function(col){

    Results <- DriverHist %>% group_by_(col) %>% 
    summarise(MVAct = sum(MVAct, na.rm = TRUE),
              NumberAtMth = n()) %>% 
    mutate(MonthlyRate = MVAct / NumberAtMth)
  

    Results %>% ggplot(aes_string(x = col, y = "MonthlyRate")) + geom_line() +
    theme_hc() +
    scale_fill_pander() + 
    labs(y = "# of MVAs / # of Employees", x = "Months Relative to Class") + 
    ggtitle(col)
  
}

GraphMonthlyRate("MthsSinceSmith")
GraphMonthlyRate("MthsSinceDD")
GraphMonthlyRate("MthsSinceSafe")

GraphMonthlyNum <- function(col){
  
  Results <- DriverHist %>% group_by_(col) %>% 
    summarise(MVAct = sum(MVAct, na.rm = TRUE),
              NumberAtMth = n()) %>% 
    mutate(MonthlyRate = MVAct / NumberAtMth)
  
  
  Results %>% ggplot(aes_string(x = col, y = "NumberAtMth")) + geom_bar(stat="identity") +
    theme_hc() +
    scale_fill_pander() + 
    labs(y = "Count of Employees", x = "Months Relative to Class") + 
    ggtitle(col)
  
}

GraphMonthlyNum("MthsSinceSmith")
GraphMonthlyNum("MthsSinceDD")
GraphMonthlyNum("MthsSinceSafe")

DriverHist$HadAccident <- ifelse(is.na(DriverHist$MVAct),"No MVA", "Had MVA")

GraphMonthlyNumWithMVA <- function(col){
  
  DriverHist %>% ggplot(aes_string(x = col, fill = "HadAccident")) + geom_bar() +
    theme_hc() +
    scale_fill_pander() + 
    labs(y = "Count of Employees", x = "Months Relative to Class") + 
    ggtitle(col)
  
}

GraphMonthlyNumWithMVA("MthsSinceSmith")
GraphMonthlyNumWithMVA("MthsSinceDD")
GraphMonthlyNumWithMVA("MthsSinceSafe")









  
Results <- DriverHist %>% group_by(SmithTrained) %>% 
  summarise(MVAct = sum(MVAct, na.rm = TRUE),
            NumberAtMth = n()) %>% 
  mutate(MonthlyRate = MVAct / NumberAtMth)
  
Results <- DriverHist %>% group_by(DDTrained) %>% 
  summarise(MVAct = sum(MVAct, na.rm = TRUE),
            NumberAtMth = n()) %>% 
  mutate(MonthlyRate = MVAct / NumberAtMth)

Results <- DriverHist %>% group_by(SafeTrained) %>% 
  summarise(MVAct = sum(MVAct, na.rm = TRUE),
            NumberAtMth = n()) %>% 
  mutate(MonthlyRate = MVAct / NumberAtMth)


  
  
  group_by(col) %>% 
    summarise(MVAct = sum(MVAct, na.rm = TRUE),
              NumberAtMth = n()) %>% 
    mutate(MonthlyRate = MVAct / NumberAtMth)
  
  



Results <- DriverHist %>% group_by(CC,DDTrained) %>%
  summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), TotalMVAs = sum(MVAct, na.rm = TRUE)) %>%
  mutate(Rate = TotalMVAs * 1000000 / TotalMiles)

Results <- left_join(Results, CCLut, by = c("CC" = "OrgStruct_CC"))

Results <- Results %>% group_by(OrgStruct_Division, DDTrained) %>%
  summarise(DivTotalMiles = sum(TotalMiles), DivTotalMVAs = sum(TotalMVAs))

Results <- Results %>% mutate(Rate = DivTotalMVAs * 1000000 / DivTotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/DDTrainedRates.csv", row.names = FALSE)

Results <- DriverHist %>% group_by(DDTrained) %>% 
                          summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
                                    TotalMVAs = sum(MVAct, na.rm = TRUE),
                                    TotalAtFaultMVAS = sum(AtFaultMVAct, na.rm = TRUE)) %>%
                          mutate(Rate = TotalMVAs * 1000000 / TotalMiles,
                                 AtFaultMvaRate = TotalAtFaultMVAS * 1000000 / TotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/BinaryDDTrainedRates.csv", row.names = FALSE)


Results <- DriverHist %>% group_by(CC,SafeTrained) %>%
  summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), TotalMVAs = sum(MVAct, na.rm = TRUE)) %>%
  mutate(Rate = TotalMVAs * 1000000 / TotalMiles)

Results <- left_join(Results, CCLut, by = c("CC" = "OrgStruct_CC"))

Results <- Results %>% group_by(OrgStruct_Division, SafeTrained) %>%
  summarise(DivTotalMiles = sum(TotalMiles), DivTotalMVAs = sum(TotalMVAs))

Results <- Results %>% mutate(Rate = DivTotalMVAs * 1000000 / DivTotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/SafeTrainedRates.csv", row.names = FALSE)

Results <- DriverHist %>% group_by(SafeTrained) %>% 
  summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
            TotalMVAs = sum(MVAct, na.rm = TRUE),
            TotalAtFaultMVAS = sum(AtFaultMVAct, na.rm = TRUE)) %>%
  mutate(Rate = TotalMVAs * 1000000 / TotalMiles,
         AtFaultMvaRate = TotalAtFaultMVAS * 1000000 / TotalMiles)

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAProject/BinarySafeTrainedRates.csv", row.names = FALSE)







Results <- DriverHist %>% group_by(MthsSinceSmith) %>%
               summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
                         TotalMVAs = sum(MVAct, na.rm = TRUE),
                         TotalAtFaultMVAs = sum(AtFaultMVAct, na.rm = TRUE), 
                         Ct = n()) %>% 
               mutate(Rate = TotalMVAs * 1000000 / TotalMiles, 
                      AtFaultMvaRate = TotalAtFaultMVAs * 1000000 / TotalMiles, 
                      EmpRate = TotalMVAs / Ct, 
                      EmpAFRate = TotalAtFaultMVAs / Ct)

# Results <- Results %>% mutate(ClassRecent = 
#                                 case_when(.$MthsSinceSmith %in% c(0:2) ~ "0-2 months",
#                                           .$MthsSinceSmith %in% c(3:5) ~ "3-5 months",
#                                           .$MthsSinceSmith %in% c(6:8) ~ "6-8 months",
#                                           .$MthsSinceSmith %in% c(9:11) ~ "9-11 months",
#                                           TRUE                       ~ ">= 12 months"
#                                           ))

# Results$ClassRecent <- factor(Results$ClassRecent, levels = c("0-2 months", "3-5 months",
#                                                               "6-8 months", "9-11 months", ">= 12 months"))

# ggplot(Results %>% filter(MthsSinceSmith < 17 & MthsSinceSmith >= 0), 
#        aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
#   theme_hc() +
#   scale_fill_pander() + 
#   labs(y = "Count", fill = "")


# ggplot(Results %>% filter(MthsSinceSmith < 17 & MthsSinceSmith >= 0), 
#        aes(MthsSinceSmith, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
#   theme_pander() +
#   scale_fill_pander() + 
#   labs(x = "Months Since Smith", col = "")

Results <- Results %>% mutate(ClassRecent = 
                                case_when(.$MthsSinceSmith %in% c(-50:-1) ~ "Before Training",
                                          .$MthsSinceSmith %in% c(0:50) ~ "After Training",
                                          TRUE                       ~ ">= 50 months"
                                ))

Results$ClassRecent <- factor(Results$ClassRecent, levels = c("Before Training", "After Training",
                                                              ">= 50 months"))

ggplot(Results %>% filter(is.na(MthsSinceSmith)==FALSE), 
       aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of MVA Rate Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceSmith)==FALSE), 
       aes(AtFaultMvaRate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of At Fault MVA Rate Relative to Training")



ggplot(Results %>% filter(is.na(MthsSinceSmith)==FALSE), 
       aes(MthsSinceSmith, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since Smith", col = "") + 
  ggtitle("MVA Rate for Months Relative to Training")


ggplot(Results %>% filter(is.na(MthsSinceSmith)==FALSE), 
       aes(MthsSinceSmith, AtFaultMvaRate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since Smith", col = "") + 
  ggtitle("At Fault MVA Rate for Months Relative to Training")





Results <- DriverHist %>% group_by(MthsSinceDD) %>%
  summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
            TotalMVAs = sum(MVAct, na.rm = TRUE),
            TotalAtFaultMVAs = sum(AtFaultMVAct, na.rm = TRUE), 
            Ct = n()) %>% 
  mutate(Rate = TotalMVAs * 1000000 / TotalMiles, 
         AtFaultMvaRate = TotalAtFaultMVAs * 1000000 / TotalMiles, 
         EmpRate = TotalMVAs / Ct,
         EmpAFRate = TotalAtFaultMVAs / Ct) 
# 
# Results <- Results %>% mutate(ClassRecent = 
#                                 case_when(.$MthsSinceDD %in% c(0:2) ~ "0-2 months",
#                                           .$MthsSinceDD %in% c(3:5) ~ "3-5 months",
#                                           .$MthsSinceDD %in% c(6:8) ~ "6-8 months",
#                                           .$MthsSinceDD %in% c(9:11) ~ "9-11 months",
#                                           TRUE                       ~ ">= 12 months"
#                                 ))
# 
# Results$ClassRecent <- factor(Results$ClassRecent, levels = c("0-2 months", "3-5 months",
#                                                               "6-8 months", "9-11 months", ">= 12 months"))
# 
# ggplot(Results %>% filter(MthsSinceDD < 17 & MthsSinceDD >= 0), 
#        aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
#   theme_hc() +
#   scale_fill_pander() + 
#   labs(y = "Count", fill = "")
# 
# 
# ggplot(Results %>% filter(MthsSinceDD < 17 & MthsSinceDD >= 0), 
#        aes(MthsSinceDD, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
#   theme_pander() +
#   scale_fill_pander() + 
#   labs(x = "Months Since Defensive Driver", col = "")

Results <- Results %>% mutate(ClassRecent = 
                                case_when(.$MthsSinceDD %in% c(-50:-1) ~ "Before DD",
                                          .$MthsSinceDD %in% c(0:50) ~ "After DD",
                                          TRUE                       ~ ">= 50 months"
                                ))

Results$ClassRecent <- factor(Results$ClassRecent, levels = c("Before DD", "After DD",
                                                              ">= 50 months"))

ggplot(Results %>% filter(is.na(MthsSinceDD)==FALSE), 
       aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of MVA Rate Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceDD)==FALSE), 
       aes(AtFaultMvaRate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of At Fault MVA Rate Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceDD)==FALSE), 
       aes(MthsSinceDD, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since Defensive Driver", col = "") + 
  ggtitle("MVA Rate for Months Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceDD)==FALSE), 
       aes(MthsSinceDD, AtFaultMvaRate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since Defensive Driver", col = "") + 
  ggtitle("At Fault MVA Rate for Months Relative to Training")



Results <- DriverHist %>% group_by(MthsSinceSafe) %>%
  summarise(TotalMiles = sum(MilesPerDriver, na.rm = TRUE), 
            TotalMVAs = sum(MVAct, na.rm = TRUE),
            TotalAtFaultMVAs = sum(AtFaultMVAct, na.rm = TRUE), 
            Ct = n()) %>% 
  mutate(Rate = TotalMVAs * 1000000 / TotalMiles, 
         AtFaultMvaRate = TotalAtFaultMVAs * 1000000 / TotalMiles)

# Results <- Results %>% mutate(ClassRecent = 
#                                 case_when(.$MthsSinceSafe %in% c(0:2) ~ "0-2 months",
#                                           .$MthsSinceSafe %in% c(3:5) ~ "3-5 months",
#                                           .$MthsSinceSafe %in% c(6:8) ~ "6-8 months",
#                                           .$MthsSinceSafe %in% c(9:11) ~ "9-11 months",
#                                           TRUE                       ~ ">= 12 months"
#                                 ))
# 
# Results$ClassRecent <- factor(Results$ClassRecent, levels = c("0-2 months", "3-5 months",
#                                                               "6-8 months", "9-11 months", ">= 12 months"))
# 
# ggplot(Results %>% filter(MthsSinceSafe < 9 & MthsSinceSafe >= 0), 
#        aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
#   theme_hc() +
#   scale_fill_pander() + 
#   labs(y = "Count", fill = "")
# 
# 
# ggplot(Results %>% filter(MthsSinceSafe < 9 & MthsSinceSafe >= 0), 
#        aes(MthsSinceSafe, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
#   theme_pander() +
#   scale_fill_pander() + 
#   labs(x = "Months Since CDT Safe Driver", col = "")

Results <- Results %>% mutate(ClassRecent = 
                                case_when(.$MthsSinceSafe %in% c(-50:-1) ~ "Before Training",
                                          .$MthsSinceSafe %in% c(0:50) ~ "After Training",
                                          TRUE                       ~ ">= 50 months"
                                ))

Results$ClassRecent <- factor(Results$ClassRecent, levels = c("Before Training", "After Training",
                                                              ">= 50 months"))

ggplot(Results %>% filter(is.na(MthsSinceSafe)==FALSE), 
       aes(Rate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of MVA Rate Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceSafe)==FALSE), 
       aes(AtFaultMvaRate, fill = factor(ClassRecent))) + geom_histogram(bins =25) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", fill = "") + 
  ggtitle("Distribution of At Fault MVA Rate Relative to Training")


ggplot(Results %>% filter(is.na(MthsSinceSafe)==FALSE), 
       aes(MthsSinceSafe, Rate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since CDT Safe Driver", col = "") + 
  ggtitle("MVA Rate for Months Relative to Training")

ggplot(Results %>% filter(is.na(MthsSinceSafe)==FALSE), 
       aes(MthsSinceSafe, AtFaultMvaRate)) + geom_line(size =1) + geom_point(aes(col = ClassRecent), size = 3) +
  theme_pander() +
  scale_fill_pander() + 
  labs(x = "Months Since CDT Safe Driver", col = "") + 
  ggtitle("At Fault MVA Rate for Months Relative to Training")


###  


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



