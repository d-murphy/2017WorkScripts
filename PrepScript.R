library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(xlsx)


### Values {name}_ are not in the original data set.  
### Calc_ are calculated values
### OrgStruct_ are joined from OrgStructure.csv
### EmpDir_ are joined from EmpDir.xlsx


## Use to clean environment
# remove(list = ls())


ReportDate <- ymd("2017-8-20")  
#ReportDate <- ymd(today())
CurrentYear <- year(today())
LastYear <- year(today() - 365)


# read_excel failed to read LTD Dates in properly.  read.xlsx2 didn't use NAs.  Used read.xlsx2 to read LTD Dates and joined to Events

Events <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/75003082.xlsx")

LTDDays <- read.xlsx2("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/75003082.xlsx", sheetIndex = 1, stringsAsFactors=FALSE)
LTDDays <- LTDDays %>% select(System.Event.ID, Date.Out.of.Work,Date.Returned.to.Work, Date.Restricted, Date.Returned.to.Full.Duty)

### This should be improved later
LTDDays$Date.Out.of.Work <- ymd(LTDDays$Date.Out.of.Work)
LTDDays$Date.Returned.to.Work <- ymd(LTDDays$Date.Returned.to.Work)
LTDDays$Date.Restricted <- ymd(LTDDays$Date.Restricted)
LTDDays$Date.Returned.to.Full.Duty <- ymd(LTDDays$Date.Returned.to.Full.Duty)
colnames(LTDDays)[1] <- c("System Event ID")

Events <- Events %>% select(-`Date Out of Work`, -`Date Returned to Work`, -`Date Restricted`, -`Date Returned to Full Duty`)
Events <- left_join(Events, LTDDays, by= "System Event ID")
remove(LTDDays)


Events$Date <- ymd_hm(Events$`Incident Date / Incident Time`)
Events$Calc_Year <- year(Events$`Incident Date / Incident Time`)
Events$Calc_Month <- month(Events$`Incident Date / Incident Time`)
Events$Calc_Day <- day(Events$`Incident Date / Incident Time`)
Events$Calc_Time <- hm(substring(Events$`Incident Date / Incident Time`,12,16))
Events$Calc_Hour <- hour(Events$Calc_Time)
Events$Calc_EmpID <- ifelse(is.na(Events$`Personnel No.`), Events$`Driver Employee ID`,Events$`Personnel No.`)
Events$`Total Lost Days` <- as.numeric(Events$`Total Lost Days`)
Events$`Total Restricted Days` <- as.numeric(Events$`Total Restricted Days`)
Events$Calc_InjIll <- ifelse(is.na(Events$`Nature of Injury`), Events$`Illness Type`, Events$`Nature of Injury`)


# Adding OrgStruct_

CCLut <- read.csv("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/OrgStructure.csv")
colnames(CCLut) <- paste("OrgStruct",colnames(CCLut), sep = "_")
Events$CC <- as.numeric(str_sub(Events$`Cost Center`,1,4))
Events <- left_join(Events,CCLut,by = c("CC" = "OrgStruct_CC"))

# Adding EmpDir and EmpDir_ columns

EmpDir <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/EmpDir.xlsx")
EmpDir <- EmpDir[1:79]    # cut off blank columns

# join OrgStruct_ to EmpDir

EmpDir$CC <- as.numeric(EmpDir$`Cost Center`)
EmpDir <- left_join(EmpDir, CCLut, by = c("CC" = "OrgStruct_CC"))    
EmpDir$`Cost Center` <- NULL

# Join driver training info to EmpDir

SafeTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SAFETraining.xlsx")
SafeTraining$`Personnel No.` <- as.character(SafeTraining$`Personnel No.`)
SafeTraining <- SafeTraining %>% select(`Personnel No.`, `Date of SAFE Driver Training`)
SafeTraining <- SafeTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of SAFE Driver Training` = max(`Date of SAFE Driver Training`))
SafeTraining <- ungroup(SafeTraining)
EmpDir <- left_join(EmpDir, SafeTraining, by = "Personnel No.")

DDTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/DDTraining.xlsx")
DDTraining$`Personnel No.` <- as.character(DDTraining$`Personnel No.`)
DDTraining <- DDTraining %>% select(`Personnel No.`, `Date of Defensive Driver Training`)
DDTraining <- DDTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of Defensive Driver Training` = max(`Date of Defensive Driver Training`))
DDTraining <- ungroup(DDTraining)
EmpDir <- left_join(EmpDir, DDTraining, by = "Personnel No.")


SmithTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SmithTraining.xlsx")
SmithTraining$`Personnel No.` <- as.character(SmithTraining$`Personnel No.`)
SmithTraining <- SmithTraining %>% select(`Personnel No.`, `Date of Smith Training`)
SmithTraining <- SmithTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of Smith Training` = max(`Date of Smith Training`))
SmithTraining <- ungroup(SmithTraining)
EmpDir <- left_join(EmpDir, SmithTraining, by = "Personnel No.")

remove(SafeTraining, DDTraining, SmithTraining)


AD <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/ADTraining.xls", skip = 17)
AD$`Launch Date` <- ymd(AD$`Launch Date`)
AD$`Due Date` <- ymd(AD$`Due Date`)
AD$`Completion Date` <- ymd(AD$`Completion Date`)
AD$Status <- ifelse(is.na(AD$`Completion Date`),  
                    ifelse(today()>AD$`Due Date`, "Overdue", "Enrolled"),
                    "Completed"
)
AD$Source <- "Alert Driving Statistics Report"
colnames(AD)[1] <- "Personnel No."

ADHPE <- AD %>% filter(Course == "Hazard Perception Evaluation 2.0 - USA PSEG LI") %>%
  select(`Personnel No.`, Status)
colnames(ADHPE)[2] <- "Hazard Perception Evaluation Status"
EmpDir <- left_join(EmpDir, ADHPE, by = "Personnel No.")

ADHPTT <- AD %>% filter(Course == "Hazard Perception Targeted Training - USA PSEG LI") %>%
  select(`Personnel No.`, Status)
colnames(ADHPTT)[2] <- "Hazard Perception Targeted Training Status"
EmpDir <- left_join(EmpDir, ADHPTT, by = "Personnel No.")

ADY2 <- AD %>% filter(Course == "PSEG LI Driver Training Y2") %>%
  select(`Personnel No.`, Status)
colnames(ADY2)[2] <- "PSEG LI Driver Training Y2 Status"
EmpDir <- left_join(EmpDir, ADY2, by = "Personnel No.")

ADY3 <- AD %>% filter(Course == "PSEG LI Driver Training Y3") %>%
  select(`Personnel No.`, Status)
colnames(ADY3)[2] <- "PSEG LI Driver Training Y3 Status"
EmpDir <- left_join(EmpDir, ADY3, by = "Personnel No.")

remove(ADHPE, ADHPTT, ADY2, ADY3, AD)

IsDriver <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/Original Driver Training Status Report with Company Driver Status.xlsx")
IsDriver$`Personnel No.` <- as.character(IsDriver$`Personnel No.`)

EmpDir <- left_join(EmpDir, IsDriver %>% select(`Personnel No.`,`Is Driver Final?`), by = "Personnel No.")
EmpDir$`Is Driver Final?` <- ifelse(is.na(EmpDir$`Is Driver Final?`), 
                                    ifelse(is.na(EmpDir$`Date of Smith Training`) & 
                                             is.na(EmpDir$`Date of Defensive Driver Training`) &
                                             is.na(EmpDir$`Date of SAFE Driver Training`) &
                                             is.na(EmpDir$`Hazard Perception Evaluation Status`) & 
                                             is.na(EmpDir$`Hazard Perception Targeted Training Status`) &
                                             is.na(EmpDir$`PSEG LI Driver Training Y2 Status`) & 
                                             is.na(EmpDir$`PSEG LI Driver Training Y3 Status`), "No", "Yes"
                                    ), EmpDir$`Is Driver Final?`)

remove(IsDriver)


# join EmpDir to Events
EmpDirJoin <- EmpDir %>% select(`Personnel No.`,`Job Title:`,Location, `Name:`, `Is Driver Final?`, 
                                `Date of Smith Training`, `Date of Defensive Driver Training`,`Date of SAFE Driver Training`,
                                `Hazard Perception Evaluation Status`, `Hazard Perception Targeted Training Status`,
                                `PSEG LI Driver Training Y2 Status`, `PSEG LI Driver Training Y3 Status`)
colnames(EmpDirJoin) <- paste("EmpDir", colnames(EmpDirJoin), sep = "_")
Events <- left_join(Events, EmpDirJoin, by = c("Calc_EmpID" = "EmpDir_Personnel No."))
remove(EmpDirJoin)

# join Fleet to Events

Fleet <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Fleet.xlsx")
Fleet$VehID <- str_sub(Fleet$Equipment, 3,8)
Fleet <- Fleet %>% select(VehID, `User status`, Location, AcquistnValue, ConstructYear, 
                          Description, Manufacturer, `Model number`, `Functional Loc.`)  
colnames(Fleet) <- paste("Fleet",colnames(Fleet), sep = "_")

temp <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/VehicleMileage4_15-3_16.xlsx")

####
temp <- temp %>% select(`Driver Name`, `Current Reading (MILES)`, 
                        `Prior 12 months usage (MILES)`, `Object Type`)
colnames(temp) <- c("Fleet_VehID", "3-16Miles", "3-16Prior12MthsMiles", "Fleet_Category")
Fleet <- left_join(Fleet, temp, by = "Fleet_VehID")

### need to confirm with tom that these changes are warrented

for(i in 1:dim(Fleet)[1]){  
  
  Fleet$`Fleet_Functional Loc.`[i] <- trimws(str_split_fixed(Fleet$`Fleet_Functional Loc.`[i],"-", n=3)[2])
  
}  

Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "MOB3BRENT", 
                                        "BRENTWOOD", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "MOB4PATCH", 
                                        "PATCHOGUE", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "MOB6PATCH", 
                                        "PATCHOGUE", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "MOB2HICKS", 
                                        "HICKSVILLE", Fleet$`Fleet_Functional Loc.`)

Fleet$`Fleet_Functional Loc.` <- paste0(toupper(substr(Fleet$`Fleet_Functional Loc.`, 1, 1)), 
                                        tolower(substring(Fleet$`Fleet_Functional Loc.`, 2)))

Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "Bridgehamp", 
                                                  "Bridgehampton", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "Hewlet", 
                                                  "Hewlett", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "Portjeff", 
                                                  "Port Jefferson", Fleet$`Fleet_Functional Loc.`)
Fleet$`Fleet_Functional Loc.` <- ifelse(Fleet$`Fleet_Functional Loc.` == "Rosyln", 
                                                  "Roslyn", Fleet$`Fleet_Functional Loc.`)

### Adds MVA count to Fleet dataset

temp <- Events %>% 
          filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% 
          select(`Company Vehicle Number`) %>%
          group_by(`Company Vehicle Number`) %>%
          summarise("MVA Count" = n())

Fleet <- left_join(Fleet, temp, by = c("Fleet_VehID" = "Company Vehicle Number"))

### Adds mileage to fleet 

temp <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Mileage/VehicleMileage4_16-3_17.xlsx")
temp <- temp %>% select(`Driver Name`, `Current Reading (MILES)`, `Prior 12 months usage (MILES)`)
colnames(temp) <- c("Fleet_VehID", "3-17Miles", "3-17Prior12MthsMiles")
Fleet <- left_join(Fleet, temp, by = "Fleet_VehID")

remove(temp)

### Adds Backup Tech

temp <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/FleetBackupTech.xlsx")
temp$Fleet_VehID <- as.character(temp$Fleet_VehID)
temp$SafetyBackupCam <- ymd(temp$SafetyBackupCam)
temp$SafetyBackupSensor <- ymd(temp$SafetyBackupSensor)
Fleet <- left_join(Fleet, temp, by = "Fleet_VehID")
remove(temp)

###  Add Fleet Classification


temp <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/FleetClassification.xlsx")
Fleet <- left_join(Fleet, temp, by = "Fleet_Description")

remove(temp)

### Add ticket count to Fleet

Tickets <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Ticket Log 2017.xlsx", sheet = 3)
temp <- Tickets %>% 
            group_by(`PSEG Unit`) %>%
            summarise(Fleet_NumTickets = n())

temp <- ungroup(temp)
temp$`PSEG Unit` <- as.character(temp$`PSEG Unit`)

Fleet <- left_join(Fleet, temp, by = c("Fleet_VehID" = "PSEG Unit"))
remove(temp)

temp <- Tickets %>% filter(`Type (Red Light, Speeding)` != "SPEEDING ")  %>%
  group_by(`PSEG Unit`) %>%
  summarise(Fleet_NumTixWOspeed = n())

temp <- ungroup(temp)
temp$`PSEG Unit` <- as.character(temp$`PSEG Unit`)

Fleet <- left_join(Fleet, temp, by = c("Fleet_VehID" = "PSEG Unit"))
remove(temp)


# Add Fleet To Events

Events <- left_join(Events, Fleet, by = c("Company Vehicle Number" = "Fleet_VehID"))



# join Fleet costs to Events

FleetCosts <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Fleet Repair Costs.xlsx")
FleetCosts$`System Event ID` <- as.character(FleetCosts$`System Event ID`)
FleetCosts$Cost <- as.numeric(FleetCosts$Cost)
colnames(FleetCosts)[2:3] <- paste("FleetCost",colnames(FleetCosts)[2:3], sep = "_")
Events <- left_join(Events, FleetCosts, by = "System Event ID")
remove(FleetCosts)



FleetCosts <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Fleet Total Loss Costs.xlsx")
FleetCosts$`System Event ID` <- as.character(FleetCosts$`System Event ID`)
colnames(FleetCosts)[2] <- paste("FleetCost",colnames(FleetCosts)[2], sep = "_")
Events <- left_join(Events, FleetCosts, by = "System Event ID")
remove(FleetCosts)

Events$FleetCost_CostNARM <- ifelse(is.na(Events$FleetCost_Cost),0,Events$FleetCost_Cost)
Events$`FleetCost_Total Loss CostNARM` <- ifelse(is.na(Events$`FleetCost_Total Loss Cost`),0,Events$`FleetCost_Total Loss Cost`)

Events <- Events %>% mutate(FleetCost_Total = FleetCost_CostNARM + `FleetCost_Total Loss CostNARM`)

Events$FleetCost_CostNARM <- NULL
Events$`FleetCost_Total Loss CostNARM` <- NULL


# Employee Yard is captured for injuries, not for MVAs.  
# Calc_EmployeeYard takes Location from Employee Directory if MVA - this is error prone if Employee moves, 

Events$Calc_EmployeeYard <- ifelse(is.na(Events$`Personnel Sub Area`), Events$EmpDir_Location, Events$`Personnel Sub Area`)

Events$Calc_EmployeeYard <- ifelse(Events$Calc_EmployeeYard == "Bridgehampton-H", "Bridgehampton", Events$Calc_EmployeeYard)

Events$Calc_YTDLostDays <- NA

for(i in 1:dim(Events)[1]){
  
  if( is.na(Events$Date.Out.of.Work[i]) == FALSE &
      year(Events$Date.Out.of.Work[i]) == year(Events$`Incident Date / Incident Time`[i]) &
      (month(Events$Date.Out.of.Work[i]) < month(ReportDate) | ( month(Events$Date.Out.of.Work[i]) == month(ReportDate) & 
                                                                  day(Events$Date.Out.of.Work[i]) < day(ReportDate)
                                                                )
       ) &
       (month(Events$`Incident Date / Incident Time`[i]) < month(ReportDate) | (
           month(Events$`Incident Date / Incident Time`[i]) == month(ReportDate) & 
           day(Events$`Incident Date / Incident Time`[i]) <= day(ReportDate) 
          )
        )
    ){
  
        if(Events$Calc_Year[i] == LastYear){
              Events$Calc_YTDLostDays[i] <- ReportDate - 364 - Events$Date.Out.of.Work[i]
              } else if(Events$Calc_Year[i] == CurrentYear){
              Events$Calc_YTDLostDays[i] <- ReportDate - Events$Date.Out.of.Work[i]
              }
  
          }
}

Events$Calc_YTDLostDays <- as.numeric(Events$Calc_YTDLostDays)
Events$Calc_YTDLostDays <- ifelse(Events$Calc_YTDLostDays > Events$`Total Lost Days`, Events$`Total Lost Days`,Events$Calc_YTDLostDays)


Events$Calc_CrashResp <- NA
for(i in 1:dim(Events)[1]){
  
  if(grepl("OTHER", Events$`Crash Type (105)`[i])){
    Events$Calc_CrashResp[i] <- "Other Vehicle"
  } else if (grepl("PS", Events$`Crash Type (105)`[i])) {
    Events$Calc_CrashResp[i] <- "PS Vehicle"
  }
  
}

Events$Calc_CrashType <- NA
for(i in 1:dim(Events)[1]){
  
  if(grepl(" - ", Events$`Crash Type (105)`[i])){
    
    Events$Calc_CrashType[i] <- substring(Events$`Crash Type (105)`[i], 
                                          regexpr(" - ", Events$`Crash Type (105)`[i])[1] + 3)
    substring
  }
  
}

Events$Calc_CrashType <- gsub("  ", " ", Events$Calc_CrashType)  # one value has an extra space. 


Events <- Events %>% distinct()  # Was needed because Fleet joins were creating extra rows

EventsJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% 
    select(`System Event ID`, Date) %>% 
    arrange(Date) %>% 
    mutate(DaysToLastMVA = difftime(Date, lag(Date), units = "days"))
EventsJoin$Date <- NULL
Events <- left_join(Events, EventsJoin, by="System Event ID")

remove(EventsJoin)

EventsJoin <- Events %>% 
  filter(`Event Subtype` %in% c("Days Away from Work", "Other Recordable Case", "Job Transfer or Restriction")) %>% 
  select(`System Event ID`, Date) %>% 
  arrange(Date) %>% 
  mutate(DaysToLastOsha = difftime(Date, lag(Date), units = "days"))
EventsJoin$Date <- NULL
Events <- left_join(Events, EventsJoin, by="System Event ID")
remove(EventsJoin)

LocationUpdate <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/LocationCorrections.xlsx")
LocationUpdate <- LocationUpdate %>% select(`System Event ID`, MVAZipCodeUpdated)
LocationUpdate$`System Event ID` <- as.character(LocationUpdate$`System Event ID`)

Events <- left_join(Events,LocationUpdate, by="System Event ID")
remove(LocationUpdate)

# Add Town Name and County to Events

TownNames <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/LI_TownNames.xlsx")
Events <- left_join(Events, TownNames, by = "MVAZipCodeUpdated")
remove(TownNames)

Events <- Events %>% arrange(desc(Date))

write.csv(Events, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Events.csv", row.names = FALSE)
write.csv(Fleet, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Fleet.csv", row.names = FALSE)
write.csv(EmpDir, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/EmpDir.csv", row.names = FALSE)

### To check Fleet WO Numbers

#write.csv((Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting")) %>%
#             select(`System Event ID`, Date, `Job Number`, `MV Classification`, `Fleet Vehicle`, `Company Vehicle Number`, `Cost Center`)),
#          "C:/Users/murphyd/Desktop/Temp/temp.csv")


# 
# remove(list = ls())



