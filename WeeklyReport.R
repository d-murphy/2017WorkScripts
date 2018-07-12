library(dplyr)
library(readxl)
library(stringr)
library(lubridate)


#### Note:  MTD doesn't work for SLT report - see note below

# Save current data to Temp folder on Desktop.  Keep CCLut.csv on Desktop.  

#ReportDate <- ymd(today())     # Use for current date
ReportDate <- ymd("2018-7-8")   #Use to run report on another date


### Dataframe to save results:  

Results <- data.frame()


### 4 queries for OSHAs

temp <- data.frame()


temp <- Events %>% filter(year(Date) == year(ReportDate) & 
                        `Event Subtype` %in% c("Death", "Other Recordable Case", 
                                               "Job Transfer or Restriction","Days Away from Work")  &
                        `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "OSHA"
temp$Range <- "YTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()

temp <- Events %>% filter(Date >= ymd(paste0((year(ReportDate)-1),"-01-01")) & 
                        Date <= (ReportDate-365) &
                        `Event Subtype` %in% c("Death", "Other Recordable Case", 
                                               "Job Transfer or Restriction","Days Away from Work")  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "OSHA"
temp$Range <- "PYTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= ymd(paste0(year(ReportDate),"-",month(ReportDate),"-01")) & 
                        Date <= ReportDate &
                        `Event Subtype` %in% c("Death", "Other Recordable Case", 
                                               "Job Transfer or Restriction","Days Away from Work")  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "OSHA"
temp$Range <- "MTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()

temp <- Events %>% filter(Date >= (ReportDate - 7)  &
                        Date <= ReportDate &  
                        `Event Subtype` %in% c("Death", "Other Recordable Case", 
                                               "Job Transfer or Restriction","Days Away from Work") &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "OSHA"
temp$Range <- "WTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


### 4 queries for First Aid

temp <- Events %>% filter(year(Date) == year(ReportDate) & 
                        `Event Subtype` == "First Aid Case"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "FirstAid"
temp$Range <- "YTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()

temp <- Events %>% filter(Date >= ymd(paste0((year(ReportDate)-1),"-01-01")) & 
                        Date <= (ReportDate-365) & 
                        `Event Subtype` == "First Aid Case"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "FirstAid"
temp$Range <- "PYTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()



temp <- Events %>% filter(Date >= ymd(paste0(year(ReportDate),"-",month(ReportDate),"-01")) & 
                        Date <= ReportDate& 
                        `Event Subtype` == "First Aid Case"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "FirstAid"
temp$Range <- "MTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= (ReportDate - 7)  &
                        Date <= ReportDate & 
                        `Event Subtype` == "First Aid Case"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "FirstAid"
temp$Range <- "WTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


### 4 queries for MVA


temp <- Events %>% filter(year(Date) == year(ReportDate) & 
                        `MV Classification` %in% c("MV - On the job", "MC - Commuting"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "MVA"
temp$Range <- "YTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= ymd(paste0((year(ReportDate)-1),"-01-01")) & 
                        Date <= (ReportDate-365) & 
                        `MV Classification` %in% c("MV - On the job", "MC - Commuting"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "MVA"
temp$Range <- "PYTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()



temp <- Events %>% filter(Date >= ymd(paste0(year(ReportDate),"-",month(ReportDate),"-01")) & 
                        Date <= ReportDate& 
                        `MV Classification` %in% c("MV - On the job", "MC - Commuting"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "MVA"
temp$Range <- "MTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()



temp <- Events %>% filter(Date >= (ReportDate - 7)  &
                        Date <= ReportDate & 
                        `MV Classification` %in% c("MV - On the job", "MC - Commuting"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "MVA"
temp$Range <- "WTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()

### 4 queries for Lost Time Cases

temp <- Events %>% filter(year(Date) == year(ReportDate) & 
                        `Event Subtype` == "Days Away from Work"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "LostTime"
temp$Range <- "YTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= ymd(paste0((year(ReportDate)-1),"-01-01")) & 
                        Date <= (ReportDate-365) &
                        `Event Subtype` == "Days Away from Work"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "LostTime"
temp$Range <- "PYTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= ymd(paste0(year(ReportDate),"-",month(ReportDate),"-01")) & 
                        Date <= ReportDate &
                        `Event Subtype` == "Days Away from Work"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised"))%>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "LostTime"
temp$Range <- "MTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= (ReportDate - 7)  &
                        Date <= ReportDate &  
                        `Event Subtype` == "Days Away from Work" &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised"))  %>% 
  group_by(OrgStruct_Department) %>%
  summarise(Count = n())

temp$Counting <- "LostTime"
temp$Range <- "WTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()







# Sum of Lost Time Days



temp <- Events %>% filter(year(Date) == year(ReportDate) & 
                         `Event Subtype` == "Days Away from Work"  &
                           `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>% 
   group_by(OrgStruct_Department) %>%
   summarise(Count = sum(YTDLostDays, na.rm = TRUE))
 
 
temp$Counting <- "LostTimeDays"
temp$Range <- "YTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= ymd(paste0((year(ReportDate)-1),"-01-01")) &
                        Date <= (ReportDate-365) &
                        `Event Subtype` == "Days Away from Work"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>%
  group_by(OrgStruct_Department) %>%
  summarise(Count = sum(YTDLostDays, na.rm = TRUE))

temp$Counting <- "LostTimeDays"
temp$Range <- "PYTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()

### This needs to be fixed to add all days from December.  May need to borrow from Scorecard Script


temp <- Events %>% filter(Date >= ymd(paste0(year(ReportDate),"-",month(ReportDate),"-01")) &
                        Date <= ReportDate &
                        `Event Subtype` == "Days Away from Work"  &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised")) %>%
  group_by(OrgStruct_Department) %>%
  summarise(Count = sum(MTDDays, na.rm = TRUE))

temp$Counting <- "LostTimeDays"
temp$Range <- "MTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()


temp <- Events %>% filter(Date >= (ReportDate - 7)  &
                        Date <= ReportDate &
                        `Event Subtype` == "Days Away from Work" &
                          `Type of Person` %in% c("Employee", "Contractor PSEG Supervised"))  %>%
  group_by(OrgStruct_Department) %>%
  summarise(Count = sum(MTDDays, na.rm = TRUE))

temp$Counting <- "LostTimeDays"
temp$Range <- "WTD"
Results <- bind_rows(Results, temp)
temp <- data.frame()



Results$key <- paste0(Results$OrgStruct_Department,Results$Counting,Results$Range)
Results <- Results[c(5,1,3,4,2)]

write.csv(Results, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/WeeklyUpdateExport.csv",row.names = FALSE)

#### Code used to find latest OSHA and MVA:  

## Begin with OSHAs

Locations <- as.data.frame(unique(Events$Calc_EmployeeYard))
colnames(Locations) <- c("Location")

Divisions <- as.data.frame(unique(Events$OrgStruct_Department))
colnames(Divisions) <- c("Division")


Last <- data.frame()

for(i in 1:dim(Locations)[1]){
  
  Divisions$Location <- Locations$Location[i] 
  Last <- bind_rows(Last,Divisions)
  
}


Last$Date <- as.Date(NA)
OSHAdf <- Events %>% filter(`Event Subtype` %in% c("Death", "Other Recordable Case", 
                                               "Job Transfer or Restriction","Days Away from Work"))


for(i in 1:dim(Last)[1]){
  temp <- OSHAdf %>% filter(Calc_EmployeeYard== Last$Location[i] & 
                              OrgStruct_Department == Last$Division[i]) %>% 
    select(Date) %>%
    summarise(Last = max(Date))
  Last$Date[i] <- temp$Last[1]  
}

Last$DaysSinceLast <- today() - Last$Date 
Last$IncidentType <- "OR"

Last$key <- paste0(Last$IncidentType, Last$Division,Last$Location)

Last <- Last[c(6,5,1,2,3,4)]

Export <- data.frame()
Export <- bind_rows(Export, Last)

## Then MVA

Last$IncidentType <- "MVA"
Last$Date <- as.Date(NA)

MVAdf <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting"))

for(i in 1:dim(Last)[1]){
  temp <- MVAdf %>% filter(Calc_EmployeeYard== Last$Location[i] & 
                             OrgStruct_Department == Last$Division[i]) %>% 
    select(Date) %>%
    summarise(Last = max(Date))
  Last$Date[i] <- temp$Last[1]  
}

Last$DaysSinceLast <- today() - Last$Date 
Last$key <- paste0(Last$IncidentType, Last$Division,Last$Location)

Export <- bind_rows(Export, Last)
write.csv(Export, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Last.csv",row.names = FALSE)

remove(Export, Last, Results, Locations, Divisions, MVAdf, OSHAdf)


