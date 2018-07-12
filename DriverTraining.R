library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(lubridate)


#### Moved to Prep Script

# SafeTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SAFETraining.xlsx")
# SafeTraining$`Personnel No.` <- as.character(SafeTraining$`Personnel No.`)
# SafeTraining <- SafeTraining %>% select(`Personnel No.`, `Date of SAFE Driver Training`)
# SafeTraining <- SafeTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of SAFE Driver Training` = max(`Date of SAFE Driver Training`))
# SafeTraining <- ungroup(SafeTraining)
# EmpDir <- left_join(EmpDir, SafeTraining, by = "Personnel No.")
# 
# DDTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/DDTraining.xlsx")
# DDTraining$`Personnel No.` <- as.character(DDTraining$`Personnel No.`)
# DDTraining <- DDTraining %>% select(`Personnel No.`, `Date of Defensive Driver Training`)
# DDTraining <- DDTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of Defensive Driver Training` = max(`Date of Defensive Driver Training`))
# DDTraining <- ungroup(DDTraining)
# EmpDir <- left_join(EmpDir, DDTraining, by = "Personnel No.")
# 
# 
# SmithTraining <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/SmithTraining.xlsx")
# SmithTraining$`Personnel No.` <- as.character(SmithTraining$`Personnel No.`)
# SmithTraining <- SmithTraining %>% select(`Personnel No.`, `Date of Smith Training`)
# SmithTraining <- SmithTraining %>% group_by(`Personnel No.`) %>% summarise(`Date of Smith Training` = max(`Date of Smith Training`))
# SmithTraining <- ungroup(SmithTraining)
# EmpDir <- left_join(EmpDir, SmithTraining, by = "Personnel No.")
# 
# remove(SafeTraining, DDTraining, SmithTraining)

# Find supervisor - if report to level is blank, looks to next level

EmpDir$Supervisor <- ifelse(is.na(EmpDir$`Report To Name 8`),EmpDir$`Report To Name 7`,EmpDir$`Report To Name 8`)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 6`,EmpDir$Supervisor)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 5`,EmpDir$Supervisor)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 4`,EmpDir$Supervisor)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 3`,EmpDir$Supervisor)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 2`,EmpDir$Supervisor)
EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor), EmpDir$`Report To Name 1`,EmpDir$Supervisor)

EmpDir$Manager <- NA

EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 8`,1,3)=="Mgr", EmpDir$`Report To Name 8`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 7`,1,3)=="Mgr", EmpDir$`Report To Name 7`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 6`,1,3)=="Mgr", EmpDir$`Report To Name 6`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 5`,1,3)=="Mgr", EmpDir$`Report To Name 5`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 4`,1,3)=="Mgr", EmpDir$`Report To Name 4`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 3`,1,3)=="Mgr", EmpDir$`Report To Name 3`, EmpDir$Manager)
EmpDir$Manager <- ifelse(str_sub(EmpDir$`Report To Position 2`,1,3)=="Mgr", EmpDir$`Report To Name 2`, EmpDir$Manager)



# Find supervisor email

EmpDir$SupervisorLU <- ifelse(is.na(EmpDir$`Middle name`),
                              paste0(toupper(EmpDir$`First name`)," ",
                                     toupper(EmpDir$`Last name`)),
                              paste0(toupper(EmpDir$`First name`)," ",
                                     toupper(EmpDir$`Last name`), " ",
                                     toupper(EmpDir$`Middle name`))
)
EmpDir$SupervisorEmail <- NA
EmpDir$SupervisorLocation <- NA
EmpDir$SupervisorJobTitle <- NA

for(i in 1:dim(EmpDir)[1]){
  
  temp <- EmpDir %>% filter(SupervisorLU == EmpDir$Supervisor[i]) %>% select(`E-mail`,Location, `Job Title:`)
  
  EmpDir$SupervisorEmail[i] <- temp$`E-mail`[1]
  EmpDir$SupervisorLocation[i] <- temp$Location[1]
  EmpDir$SupervisorJobTitle[i] <- temp$`Job Title:`[1]

}

EmpDir$ManagerEmail <- NA
EmpDir$ManagerLocation <- NA
EmpDir$ManagerJobTitle <- NA

for(i in 1:dim(EmpDir)[1]){
  
  temp <- EmpDir %>% filter(SupervisorLU == EmpDir$Supervisor[i]) %>% select(`E-mail`,Location, `Job Title:`)
  
  EmpDir$ManagerEmail[i] <- temp$`E-mail`[1]
  EmpDir$ManagerLocation[i] <- temp$Location[1]
  EmpDir$ManagerJobTitle[i] <- temp$`Job Title:`[1]
  
}


### This was used to create a billing location file for the prescription safety glasses program.

# EmpDir$Supervisor <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Dir"),EmpDir$`Name:`,EmpDir$Supervisor)
# EmpDir$SupervisorLocation <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Dir"),EmpDir$Location,EmpDir$SupervisorLocation)
# EmpDir$SupervisorEmail <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Dir"),EmpDir$`E-mail`,EmpDir$SupervisorEmail)
# 
# EmpDir$Supervisor <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Pres"),EmpDir$`Name:`,EmpDir$Supervisor)
# EmpDir$SupervisorLocation <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Pres"),EmpDir$Location,EmpDir$SupervisorLocation)
# EmpDir$SupervisorEmail <- ifelse(str_detect(EmpDir$SupervisorJobTitle,"Pres"),EmpDir$`E-mail`,EmpDir$SupervisorEmail)

# EmpDir$Supervisor <- ifelse(is.na(EmpDir$Supervisor),EmpDir$`Name:`,EmpDir$Supervisor)
# EmpDir$SupervisorLocation <- ifelse(is.na(EmpDir$SupervisorLocation),EmpDir$Location,EmpDir$SupervisorLocation)
# EmpDir$SupervisorEmail <- ifelse(is.na(EmpDir$SupervisorEmail),EmpDir$`E-mail`,EmpDir$SupervisorEmail)



#EmpDirCopy <- EmpDir

#### Moved to Prep Script

# AD <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/ADTraining.xls", skip = 17)
# AD$`Launch Date` <- ymd(AD$`Launch Date`)
# AD$`Due Date` <- ymd(AD$`Due Date`)
# AD$`Completion Date` <- ymd(AD$`Completion Date`)
# AD$Status <- ifelse(is.na(AD$`Completion Date`),  
#                         ifelse(today()>AD$`Due Date`, "Overdue", "Enrolled"),
#                     "Completed"
#                     )
# AD$Source <- "Alert Driving Statistics Report"
# colnames(AD)[1] <- "Personnel No."
# 
# ADHPE <- AD %>% filter(Course == "Hazard Perception Evaluation 2.0 - USA PSEG LI") %>%
#                 select(`Personnel No.`, Status)
# colnames(ADHPE)[2] <- "Hazard Perception Evaluation Status"
# EmpDir <- left_join(EmpDir, ADHPE, by = "Personnel No.")
# 
# ADHPTT <- AD %>% filter(Course == "Hazard Perception Targeted Training - USA PSEG LI") %>%
#   select(`Personnel No.`, Status)
# colnames(ADHPTT)[2] <- "Hazard Perception Targeted Training Status"
# EmpDir <- left_join(EmpDir, ADHPTT, by = "Personnel No.")
# 
# ADY2 <- AD %>% filter(Course == "PSEG LI Driver Training Y2") %>%
#   select(`Personnel No.`, Status)
# colnames(ADY2)[2] <- "PSEG LI Driver Training Y2 Status"
# EmpDir <- left_join(EmpDir, ADY2, by = "Personnel No.")
# 
# ADY3 <- AD %>% filter(Course == "PSEG LI Driver Training Y3") %>%
#   select(`Personnel No.`, Status)
# colnames(ADY3)[2] <- "PSEG LI Driver Training Y3 Status"
# EmpDir <- left_join(EmpDir, ADY3, by = "Personnel No.")
# 
# remove(ADHPE, ADHPTT, ADY2, ADY3, AD)

# IsDriver <- read_excel("//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/Original Driver Training Status Report with Company Driver Status.xlsx")
# IsDriver$`Personnel No.` <- as.character(IsDriver$`Personnel No.`)
# 
# EmpDir <- left_join(EmpDir, IsDriver %>% select(`Personnel No.`,`Is Driver Final?`), by = "Personnel No.")
# EmpDir$`Is Driver Final?` <- ifelse(is.na(EmpDir$`Is Driver Final?`), 
#                                     ifelse(is.na(EmpDir$`Date of Smith Training`) & 
#                                            is.na(EmpDir$`Date of Defensive Driver Training`) &
#                                            is.na(EmpDir$`Date of SAFE Driver Training`) &
#                                            is.na(EmpDir$`Hazard Perception Evaluation Status`) & 
#                                            is.na(EmpDir$`Hazard Perception Targeted Training Status`) &
#                                            is.na(EmpDir$`PSEG LI Driver Training Y2 Status`) & 
#                                            is.na(EmpDir$`PSEG LI Driver Training Y3 Status`), "No", "Yes"
#                                            ), EmpDir$`Is Driver Final?`)

CCsForDistribution <- unique(EmpDir %>% filter(EmpDir$`Is Driver Final?`=="Yes") %>% select(CC))

#CCsForDistribution <- unique(IsDriver %>% filter(`CC Drives` == "Yes") %>% select(`Cost Center #`))

write.csv(EmpDir %>% 
            select(`Personnel No.`, `Name:`, `Job Title:`, Location, 
                   OrgStruct_Line.of.business, OrgStruct_Department, CC,`Cost Center Text`, Supervisor,
                   `Is Driver Final?`, 
                   `Hazard Perception Evaluation Status`, `Hazard Perception Targeted Training Status`,
                   `PSEG LI Driver Training Y2 Status`, `PSEG LI Driver Training Y3 Status`, `PSEG LI Driver Training Y4 Status`,
                   `Date of Smith Training`, `Date of SAFE Driver Training`, `Date of Defensive Driver Training`) %>%
            arrange(desc(OrgStruct_Line.of.business), OrgStruct_Department),
          "//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTrainingStatus.csv", row.names = FALSE, na="")


# write.csv(EmpDir,
#           "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/EmpDir.csv", row.names = FALSE, na="")

Managers <- unique(EmpDir %>% filter(CC %in% CCsForDistribution$CC & !grepl("Dir", SupervisorJobTitle, ignore.case=TRUE)) %>% 
                     select(SupervisorEmail, SupervisorJobTitle) %>% 
                     arrange(SupervisorEmail))

#####   This didn't work in 11/2017 - corrected in excel

write.csv(Managers, "//gccscif01.psegliny.com/Safety/Safety Training/1-TrainingRecords/DriverTraining/ManagersEmail.csv",
          row.names = FALSE)

