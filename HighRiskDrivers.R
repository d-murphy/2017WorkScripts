library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(stringr)
library(caret)
library(ggthemes)



DriverHistWithViol <- data.frame(`EmpID`=character(0), Year = numeric(0), Location=character(0))


Driver <- EmpDir %>% select(`Personnel No.`, `Is Driver Final?`, Location)
temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & Calc_Year > 2015) %>%
  select(`Driver Employee ID`)
Driver$`Is Driver Final?` <- ifelse(Driver$`Personnel No.` %in% temp, "Yes", Driver$`Is Driver Final?` )
Driver <- Driver %>% filter(`Is Driver Final?` == "Yes") %>% select(`Personnel No.`, Location)
colnames(Driver) <- c("EmpID", "Location")



for(i in 2014:2017){
  
      Driver$Year <- i
      DriverHistWithViol <- bind_rows(DriverHistWithViol, Driver)
}

MVActJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>%
  group_by(Calc_EmpID, Calc_Year) %>%
  summarise(MVAct = n())
colnames(MVActJoin) <- c("EmpID", "Year", "MVAct")


DriverHistWithViol <- left_join(DriverHistWithViol, MVActJoin, by = c("EmpID" = "EmpID", "Year" = "Year"))



LMconvictions <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/LMconvictions.xlsx",skip = 3)

lookup <- EmpDir %>% mutate(key = paste0(toupper(`Last name`),",", toupper(`First name`),",", `Middle name`)) %>% select(key, `Personnel No.`)


LMconvictions <- left_join(LMconvictions, lookup, by = c("Name"="key"))

LMconvictions$test <- ifelse( !is.na(LMconvictions$`Emp. #`) & (str_sub(LMconvictions$`Emp. #`, 1,4)=="7500" & str_length(LMconvictions$`Emp. #`)==8) ,0,1)

LMconvictions$`Emp. #` <- ifelse( !is.na(LMconvictions$`Emp. #`) & (str_sub(LMconvictions$`Emp. #`, 1,4)=="7500" & str_length(LMconvictions$`Emp. #`)==8) ,
                                  LMconvictions$`Emp. #`,  LMconvictions$`Personnel No.`)


LMconvictions$`Personnel No.` <- NULL
lookup <- EmpDir %>% mutate(key = paste0(toupper(`Last name`),",", toupper(`First name`))) %>% select(key, `Personnel No.`)
LMconvictions <- left_join(LMconvictions, lookup, by = c("Name"="key"))

LMconvictions$test <- ifelse( !is.na(LMconvictions$`Emp. #`) & (str_sub(LMconvictions$`Emp. #`, 1,4)=="7500" & str_length(LMconvictions$`Emp. #`)==8) ,0,1)
LMconvictions$`Emp. #` <- ifelse( !is.na(LMconvictions$`Emp. #`) & (str_sub(LMconvictions$`Emp. #`, 1,4)=="7500" & str_length(LMconvictions$`Emp. #`)==8) ,
                                  LMconvictions$`Emp. #`,  LMconvictions$`Personnel No.`)
LMconvictions$`Personnel No.` <- NULL

LMconvictions$Year <- year(LMconvictions$`Activity Date`)

EmpWithLMconv <- unique(LMconvictions$`Emp. #`)

DriverHistWithViol$LMconvLastYear <- NA

for(i in 1:dim(DriverHistWithViol)[1]){
  
  if(DriverHistWithViol$EmpID[i] %in% EmpWithLMconv){
    
    temp <- LMconvictions %>% filter(`Emp. #` == DriverHistWithViol$EmpID[i] &
                                      Year == DriverHistWithViol$Year[i]-1) %>%
                              summarise(ct = n())
    DriverHistWithViol$LMconvLastYear[i] <- temp$ct[1]
      
  }
  
}




EmpWithMVA <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% select(Calc_EmpID)

DriverHistWithViol$MVAsLastYear <- NA

for(i in 1:dim(DriverHistWithViol)[1]){
  
  if(DriverHistWithViol$EmpID[i] %in% EmpWithMVA$Calc_EmpID){
    
    temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                                Calc_EmpID == DriverHistWithViol$EmpID[i] &
                                Calc_Year == DriverHistWithViol$Year[i]-1) %>%
      summarise(ct = n())
    DriverHistWithViol$MVAsLastYear[i] <- temp$ct[1]
  }
  
}





LMmvas <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/LMmvas.xlsx",skip = 3)

lookup <- EmpDir %>% mutate(key = paste0(toupper(`Last name`),",", toupper(`First name`),",", `Middle name`)) %>% select(key, `Personnel No.`)


LMmvas <- left_join(LMmvas, lookup, by = c("Name"="key"))

LMmvas$test <- ifelse( !is.na(LMmvas$`Emp. #`) & (str_sub(LMmvas$`Emp. #`, 1,4)=="7500" & str_length(LMmvas$`Emp. #`)==8) ,0,1)

LMmvas$`Emp. #` <- ifelse( !is.na(LMmvas$`Emp. #`) & (str_sub(LMmvas$`Emp. #`, 1,4)=="7500" & str_length(LMmvas$`Emp. #`)==8) ,
                           LMmvas$`Emp. #`,  LMmvas$`Personnel No.`)


LMmvas$`Personnel No.` <- NULL
lookup <- EmpDir %>% mutate(key = paste0(toupper(`Last name`),",", toupper(`First name`))) %>% select(key, `Personnel No.`)
LMmvas <- left_join(LMmvas, lookup, by = c("Name"="key"))

LMmvas$test <- ifelse( !is.na(LMmvas$`Emp. #`) & (str_sub(LMmvas$`Emp. #`, 1,4)=="7500" & str_length(LMmvas$`Emp. #`)==8) ,0,1)
LMmvas$`Emp. #` <- ifelse( !is.na(LMmvas$`Emp. #`) & (str_sub(LMmvas$`Emp. #`, 1,4)=="7500" & str_length(LMmvas$`Emp. #`)==8) ,
                           LMmvas$`Emp. #`,  LMmvas$`Personnel No.`)
LMmvas$`Personnel No.` <- NULL

LMmvas$Year <- year(LMmvas$`Activity Date`)

EmpWithLMmva <- unique(LMmvas$`Emp. #`)

DriverHistWithViol$LMmvaLastYear <- NA

for(i in 1:dim(DriverHistWithViol)[1]){
  
  if(DriverHistWithViol$EmpID[i] %in% EmpWithLMmva){
    
    temp <- LMmvas %>% filter(`Emp. #` == DriverHistWithViol$EmpID[i] &
                                       Year == DriverHistWithViol$Year[i]-1) %>%
      summarise(ct = n())
    DriverHistWithViol$LMmvaLastYear[i] <- temp$ct[1]
    
  }
  
}

DriverHistWithViol <- DriverHistWithViol %>% rowwise() %>% mutate(AnyIncidentLastYear = sum(c(LMmvaLastYear, LMconvLastYear, MVAsLastYear), na.rm=TRUE))

plt <- DriverHistWithViol %>% filter(Year > 2015) %>% group_by(AnyIncidentLastYear, MVAct) %>% summarise(ct = n()) %>% 
  group_by(AnyIncidentLastYear) %>% mutate(PctOfGrp = ct / sum(ct))
plt$MVAct <- ifelse(is.na(plt$MVAct),0,plt$MVAct)
ggplot(data=plt, aes(x=AnyIncidentLastYear, y=PctOfGrp, fill = factor(MVAct))) + geom_bar(stat="identity")


DriverHistWithViol$AnyIncBinary <- ifelse(DriverHistWithViol$AnyIncidentLastYear>0, 1,0)

plt <- DriverHistWithViol %>% filter(Year > 2015) %>% group_by(AnyIncBinary, MVAct) %>% summarise(ct = n()) %>% 
  group_by(AnyIncBinary) %>% mutate(PctOfGrp = ct / sum(ct))
ggplot(data=plt, aes(x=AnyIncBinary, y=ct, fill = factor(MVAct))) + geom_bar(stat="identity")


DriverHistWithViol$MVActBinary <- ifelse(DriverHistWithViol$MVAct<1 | is.na(DriverHistWithViol$MVAct),"No MVA","Had MVA" )


### Did an MVA in License Monitor increase your chance of an MVA this year
DriverHistWithViol$LMmvaLastYearBinary <- ifelse(DriverHistWithViol$LMmvaLastYear<1 | is.na(DriverHistWithViol$LMmvaLastYear),"No LM MVA","Had LM MVA" )

DriverHistWithViol %>% filter(Year>2015) %>% group_by(LMmvaLastYearBinary, MVActBinary) %>% summarise(ct = n()) %>% 
  group_by(LMmvaLastYearBinary) %>% mutate(PctOfGrp = ct / sum(ct))


### Did a violation in License Monitor increase your chance of a MVA this year
DriverHistWithViol$LMconvLastYearBinary <- ifelse(DriverHistWithViol$LMconvLastYear<1 | is.na(DriverHistWithViol$LMconvLastYear),"No Violation","Had violation" )

DriverHistWithViol %>% filter(Year>2015) %>% group_by(LMconvLastYearBinary, MVActBinary) %>% summarise(ct = n()) %>% 
  group_by(LMconvLastYearBinary) %>% mutate(PctOfGrp = ct / sum(ct))

### Did an MVA the previous year increase the chance of an MVA this year

DriverHistWithViol$MVALastYearBinary <- ifelse(DriverHistWithViol$MVAsLastYear<1 | is.na(DriverHistWithViol$MVAsLastYear),"No SIMS MVA","Had SIMS MVA" )

DriverHistWithViol %>% filter(Year>2015) %>% group_by(MVALastYearBinary, MVActBinary) %>% summarise(ct = n()) %>% 
  group_by(MVALastYearBinary) %>% mutate(PctOfGrp = ct / sum(ct))

### Did any infraction (SIMS MVA, License Monitor MVA, or License Monitor violation) increase your chance of an MVA this year

DriverHistWithViol %>% filter(Year>2015) %>% group_by(AnyIncBinary, MVActBinary) %>% summarise(ct = n()) %>% 
  group_by(AnyIncBinary) %>% mutate(PctOfGrp = ct / sum(ct))


DriverHistWithViol$MVActBinary <- ifelse(DriverHistWithViol$MVAct<1 | is.na(DriverHistWithViol$MVAct),0,1)
DriverHistWithViol$LMmvaLastYearBinary <- ifelse(DriverHistWithViol$LMmvaLastYear<1 | is.na(DriverHistWithViol$LMmvaLastYear),0,1 )
DriverHistWithViol$LMconvLastYearBinary <- ifelse(DriverHistWithViol$LMconvLastYear<1 | is.na(DriverHistWithViol$LMconvLastYear),0,1 )
DriverHistWithViol$MVALastYearBinary <- ifelse(DriverHistWithViol$MVAsLastYear<1 | is.na(DriverHistWithViol$MVAsLastYear),0,1 )

DriverHistWithViol$Location <- as.factor(DriverHistWithViol$Location)

train <- DriverHistWithViol[sample(nrow(DriverHistWithViol)),]
train <- train %>% select(MVActBinary:MVALastYearBinary)

test <- train[round(dim(train)[1]*.8):dim(train)[1],]
train <- train[1:round(dim(train)[1]*.8),]



plot(train)

model <- glm(MVActBinary ~ .,family = "binomial", train)
test$p <- predict(model, test, type = "response")
colAUC(test$p, test$MVActBinary, plotROC = TRUE)






AD <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/ADTraining.xls", skip = 17)
AD$`Launch Date` <- ymd(AD$`Launch Date`)
AD$`Due Date` <- ymd(AD$`Due Date`)
AD$`Completion Date` <- ymd(AD$`Completion Date`)
AD$Status <- ifelse(is.na(AD$`Completion Date`),  
                    ifelse(today()>AD$`Due Date`, "Overdue", "Enrolled"),
                    "Completed"
)


HazardPercResults <- AD %>% filter(Course == "Hazard Perception Evaluation 2.0 - USA PSEG LI" & 
                                     `Completion Date` < ymd("2016-11-1")) %>% 
                            select(`Employee ID`, Course, `Average(%)`, `Completion Date`)
remove(AD)

HazardPercResults$MVAct <- NA

for (i in 1:dim(HazardPercResults)[1]){
  
  temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & 
                    Date >= HazardPercResults$`Completion Date`[i] & 
                    Date < HazardPercResults$`Completion Date`[i] + 366 &
                    Calc_EmpID == HazardPercResults$`Employee ID`[i]) %>% 
             summarise(ct = n())
  
  HazardPercResults$MVAct[i] <- temp$ct[1]
  
}

HazardPercResults$HadMVA <- ifelse(HazardPercResults$MVAct>0,"Had MVA","No MVA")
HazardPercResults$GradeClass <- HazardPercResults$`Average(%)` - (HazardPercResults$`Average(%)` %% 10)

#View(HazardPercResults %>% group_by(GradeClass) %>% summarise(ct = n(), NumWmvas = sum(HadMVA)) %>% mutate(Pct = NumWmvas / ct))

HazardPercResults %>% ggplot(aes(GradeClass, fill = factor(HadMVA, levels = c("No MVA", "Had MVA")))) + geom_bar() + 
  labs(x = "Grade Truncated", y = "Count of Employees", fill = "") +
  theme_hc() +
  scale_fill_pander() 

HazardPercResults %>% ggplot(aes(GradeClass, fill = factor(HadMVA, levels = c("No MVA", "Had MVA")))) + geom_bar(position = "fill" ) + 
  labs(x = "Grade Truncated", y = "Percentage", fill = "") +
  theme_hc() +
  scale_fill_pander() 


HazardPercResults$GradeClass2 <- ifelse(HazardPercResults$`Average(%)`>=70,"Pass","Fail")

#View(HazardPercResults %>% group_by(GradeClass2) %>% summarise(ct = n(), NumWmvas = sum(HadMVA)) %>% mutate(Pct = NumWmvas / ct))


HazardPercResults %>% ggplot(aes(GradeClass2, fill = factor(HadMVA, levels = c("No MVA", "Had MVA")))) + geom_bar() + 
  labs(x = "", y = "Count of Employees", fill = "") +
  theme_hc() +
  scale_fill_pander() 

HazardPercResults %>% ggplot(aes(GradeClass2, fill = factor(HadMVA, levels = c("No MVA", "Had MVA")))) + geom_bar(position = "fill" ) + 
  labs(x = "", y = "Percentage", fill = "") +
  theme_hc() +
  scale_fill_pander() 
