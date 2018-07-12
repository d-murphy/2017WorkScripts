library(dplyr)
library(stringr)
library(lubridate)
library(xlsx)
library(tidyr)

ReportMonth <- 7
CurrentYear <- 2018


Scorecard <- Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", 
                                                      "Days Away from Work", 
                                                      "Job Transfer or Restriction" )  & 
                                 Calc_Year == CurrentYear & Calc_Month <= ReportMonth) %>% 
                        select(OrgStruct_Line.of.business, `System Event ID`, Date, Calc_Month, Calc_Hour, `Affected Person`, `Job Title`,
                               OrgStruct_Department, `Personnel Sub Area`, `Cost Center`, `MV Classification`, 
                               `Total Lost Days`, `Total Restricted Days`, `Accident Type`, Calc_InjIll, 
                               `Part of Body Affected`, `Actual Lost Workdays`, Date.Out.of.Work, Date.Returned.to.Work, 
                               `Actual Restricted Workdays`, Date.Restricted, Date.Returned.to.Full.Duty, `Incident Long Description` ) %>%
                        arrange(OrgStruct_Line.of.business, Date)



Scorecard$JanLsDays <- NA
Scorecard$FebLsDays <- NA
Scorecard$MarLsDays <- NA
Scorecard$AprLsDays <- NA
Scorecard$MayLsDays <- NA
Scorecard$JunLsDays <- NA
Scorecard$JulLsDays <- NA
Scorecard$AugLsDays <- NA
Scorecard$SepLsDays <- NA
Scorecard$OctLsDays <- NA
Scorecard$NovLsDays <- NA
Scorecard$DecLsDays <- NA


for(i in 1:dim(Scorecard)[1]){
  Scorecard$JanLsDays[i] <- ifelse(regexpr("Jan",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Jan",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Jan",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$JanLsDays[i] <- ifelse(regexpr("<",Scorecard$JanLsDays[i])<0,Scorecard$JanLsDays[i],
                                   str_sub(Scorecard$JanLsDays[i],1,1))
  
}



for(i in 1:dim(Scorecard)[1]){
  Scorecard$FebLsDays[i] <- ifelse(regexpr("Feb",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Feb",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Feb",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$FebLsDays[i] <- ifelse(regexpr("<",Scorecard$FebLsDays[i])<0,Scorecard$FebLsDays[i],
                                   str_sub(Scorecard$FebLsDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$MarLsDays[i] <- ifelse(regexpr("Mar",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Mar",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Mar",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$MarLsDays[i] <- ifelse(regexpr("<",Scorecard$MarLsDays[i])<0,Scorecard$MarLsDays[i],
                                   str_sub(Scorecard$MarLsDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$AprLsDays[i] <- ifelse(regexpr("Apr",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Apr",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Apr",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$AprLsDays[i] <- ifelse(regexpr("<",Scorecard$AprLsDays[i])<0,Scorecard$AprLsDays[i],
                                   str_sub(Scorecard$AprLsDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$MayLsDays[i] <- ifelse(regexpr("May",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("May",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("May",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$MayLsDays[i] <- ifelse(regexpr("<",Scorecard$MayLsDays[i])<0,Scorecard$MayLsDays[i],
                                   str_sub(Scorecard$MayLsDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$JunLsDays[i] <- ifelse(regexpr("Jun",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Jun",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Jun",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$JunLsDays[i] <- ifelse(regexpr("<",Scorecard$JunLsDays[i])<0,Scorecard$JunLsDays[i],
                                   str_sub(Scorecard$JunLsDays[i],1,1))
  
}


for(i in 1:dim(Scorecard)[1]){
  Scorecard$JulLsDays[i] <- ifelse(regexpr("Jul",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Jul",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Jul",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$JulLsDays[i] <- ifelse(regexpr("<",Scorecard$JulLsDays[i])<0,Scorecard$JulLsDays[i],
                                   str_sub(Scorecard$JulLsDays[i],1,1))
  
}


for(i in 1:dim(Scorecard)[1]){
  Scorecard$AugLsDays[i] <- ifelse(regexpr("Aug",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Aug",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Aug",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$AugLsDays[i] <- ifelse(regexpr("<",Scorecard$AugLsDays[i])<0,Scorecard$AugLsDays[i],
                                   str_sub(Scorecard$AugLsDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$SepLsDays[i] <- ifelse(regexpr("Sep",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Sep",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Sep",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$SepLsDays[i] <- ifelse(regexpr("<",Scorecard$SepLsDays[i])<0,Scorecard$SepLsDays[i],
                                   str_sub(Scorecard$SepLsDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$OctLsDays[i] <- ifelse(regexpr("Oct",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Oct",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Oct",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$OctLsDays[i] <- ifelse(regexpr("<",Scorecard$OctLsDays[i])<0,Scorecard$OctLsDays[i],
                                   str_sub(Scorecard$OctLsDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$NovLsDays[i] <- ifelse(regexpr("Nov",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Nov",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Nov",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$NovLsDays[i] <- ifelse(regexpr("<",Scorecard$NovLsDays[i])<0,Scorecard$NovLsDays[i],
                                   str_sub(Scorecard$NovLsDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$DecLsDays[i] <- ifelse(regexpr("Dec",Scorecard$`Actual Lost Workdays`[i])[1]>0,
                                   str_sub(Scorecard$`Actual Lost Workdays`[i],
                                           ((regexpr("Dec",Scorecard$`Actual Lost Workdays`[i])[1])+11),
                                           ((regexpr("Dec",Scorecard$`Actual Lost Workdays`[i])[1])+12)),
                                   0)
  Scorecard$DecLsDays[i] <- ifelse(regexpr("<",Scorecard$DecLsDays[i])<0,Scorecard$DecLsDays[i],
                                   str_sub(Scorecard$DecLsDays[i],1,1))
  
}


Scorecard$JanResDays <- NA
Scorecard$FebResDays <- NA
Scorecard$MarResDays <- NA
Scorecard$AprResDays <- NA
Scorecard$MayResDays <- NA
Scorecard$JunResDays <- NA
Scorecard$JulResDays <- NA
Scorecard$AugResDays <- NA
Scorecard$SepResDays <- NA
Scorecard$OctResDays <- NA
Scorecard$NovResDays <- NA
Scorecard$DecResDays <- NA


#### First for loops through Restricted Days.  This can be a function at a later date.  


for(i in 1:dim(Scorecard)[1]){
  Scorecard$JanResDays[i] <- ifelse(regexpr("Jan",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Jan",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Jan",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$JanResDays[i] <- ifelse(regexpr("<",Scorecard$JanResDays[i])<0,Scorecard$JanResDays[i],
                                  str_sub(Scorecard$JanResDays[i],1,1))
  
}



for(i in 1:dim(Scorecard)[1]){
  Scorecard$FebResDays[i] <- ifelse(regexpr("Feb",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Feb",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Feb",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$FebResDays[i] <- ifelse(regexpr("<",Scorecard$FebResDays[i])<0,Scorecard$FebResDays[i],
                                  str_sub(Scorecard$FebResDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$MarResDays[i] <- ifelse(regexpr("Mar",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Mar",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Mar",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$MarResDays[i] <- ifelse(regexpr("<",Scorecard$MarResDays[i])<0,Scorecard$MarResDays[i],
                                  str_sub(Scorecard$MarResDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$AprResDays[i] <- ifelse(regexpr("Apr",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Apr",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Apr",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$AprResDays[i] <- ifelse(regexpr("<",Scorecard$AprResDays[i])<0,Scorecard$AprResDays[i],
                                  str_sub(Scorecard$AprResDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$MayResDays[i] <- ifelse(regexpr("May",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("May",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("May",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$MayResDays[i] <- ifelse(regexpr("<",Scorecard$MayResDays[i])<0,Scorecard$MayResDays[i],
                                  str_sub(Scorecard$MayResDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$JunResDays[i] <- ifelse(regexpr("Jun",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Jun",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Jun",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$JunResDays[i] <- ifelse(regexpr("<",Scorecard$JunResDays[i])<0,Scorecard$JunResDays[i],
                                  str_sub(Scorecard$JunResDays[i],1,1))
  
}


for(i in 1:dim(Scorecard)[1]){
  Scorecard$JulResDays[i] <- ifelse(regexpr("Jul",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Jul",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Jul",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$JulResDays[i] <- ifelse(regexpr("<",Scorecard$JulResDays[i])<0,Scorecard$JulResDays[i],
                                  str_sub(Scorecard$JulResDays[i],1,1))
  
}


for(i in 1:dim(Scorecard)[1]){
  Scorecard$AugResDays[i] <- ifelse(regexpr("Aug",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Aug",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Aug",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$AugResDays[i] <- ifelse(regexpr("<",Scorecard$AugResDays[i])<0,Scorecard$AugResDays[i],
                                  str_sub(Scorecard$AugResDays[i],1,1))
  
}

for(i in 1:dim(Scorecard)[1]){
  Scorecard$SepResDays[i] <- ifelse(regexpr("Sep",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Sep",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Sep",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$SepResDays[i] <- ifelse(regexpr("<",Scorecard$SepResDays[i])<0,Scorecard$SepResDays[i],
                                  str_sub(Scorecard$SepResDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$OctResDays[i] <- ifelse(regexpr("Oct",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Oct",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Oct",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$OctResDays[i] <- ifelse(regexpr("<",Scorecard$OctResDays[i])<0,Scorecard$OctResDays[i],
                                  str_sub(Scorecard$OctResDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$NovResDays[i] <- ifelse(regexpr("Nov",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Nov",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Nov",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$NovResDays[i] <- ifelse(regexpr("<",Scorecard$NovResDays[i])<0,Scorecard$NovResDays[i],
                                  str_sub(Scorecard$NovResDays[i],1,1))
  
}
for(i in 1:dim(Scorecard)[1]){
  Scorecard$DecResDays[i] <- ifelse(regexpr("Dec",Scorecard$`Actual Restricted Workdays`[i])[1]>0,
                                  str_sub(Scorecard$`Actual Restricted Workdays`[i],
                                          ((regexpr("Dec",Scorecard$`Actual Restricted Workdays`[i])[1])+11),
                                          ((regexpr("Dec",Scorecard$`Actual Restricted Workdays`[i])[1])+12)),
                                  0)
  Scorecard$DecResDays[i] <- ifelse(regexpr("<",Scorecard$DecResDays[i])<0,Scorecard$DecResDays[i],
                                  str_sub(Scorecard$DecResDays[i],1,1))
  
}

Scorecard <- Scorecard %>% select(-`Actual Lost Workdays`, -`Actual Restricted Workdays`)
Scorecard <- Scorecard %>% select(1:20, 22:45, 21)
  
colnames(Scorecard) <- c("Line of Business", "SIMS ID", "Date / Time", "Month", "Hour", "Affected Employee",
                         "Job Title", "Department", "Yard", "Cost Center", "MV Classification", "Total Lost Days",
                         "Total Restricted Days", "Accident Type", "Injury / Illness Type", "Part of Body Affected",
                         "Date Out of Work", "Date Returned to Work", "Date Restricted", "Date Returned to Full Duty", 
                         "Jan Lost Days", "Feb Lost Days", "Mar Lost Days", "Apr Lost Days", "May Lost Days", "Jun Lost Days",
                         "Jul Lost Days", "Aug Lost Days", "Sep Lost Days", "Oct Lost Days", "Nov Lost Days", "Dec Lost Days", 
                         "Jan Res Days", "Feb Res Days", "Mar Res Days", "Apr Res Days", "May Res Days", "Jun Res Days",
                         "Jul Res Days", "Aug Res Days", "Sep Res Days", "Oct Res Days", "Nov Res Days", "Dec Res Days", 
                         "Description")

Scorecard <- as.data.frame(Scorecard)

write.csv(Scorecard, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/DaysAway.csv", row.names = FALSE)

# write.xlsx(Scorecard, 
#            "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx",
#            sheetName = "Sheet1",col.names = TRUE, row.names = FALSE, append = TRUE)
# 
# write.xlsx(Scorecard, 
#            "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx",
#            sheetName = "Sheet2",col.names = TRUE, row.names = FALSE, append = TRUE)


wb <- loadWorkbook("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx")
sheets <- getSheets(wb)
removeSheet(wb, sheetName="Injury Cases")
yourSheet <- createSheet(wb, sheetName="Injury Cases")
addDataFrame(Scorecard, yourSheet, row.names = FALSE)
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx")



ScorecardMVAs <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") & 
                                 Calc_Year == CurrentYear & Calc_Month <= ReportMonth) %>% 
  select(OrgStruct_Line.of.business, `System Event ID`, Date, Calc_Month, Calc_Hour, `EmpDir_Name:`, `EmpDir_Job Title:`,
         OrgStruct_Department, Calc_EmployeeYard, `Cost Center`, `MV Classification`, 
         Calc_CrashResp, Calc_CrashType, Fleet_Description, `Incident Long Description`) %>%
  arrange(OrgStruct_Line.of.business, Date)


colnames(ScorecardMVAs) <- c("Line of Business", "SIMS ID", "Date / Time", "Month", "Hour",
                             "Driver Name", "Driver Job Title", "Department", "Yard",
                             "Cost Center", "MV Classification", "Crash Responsibility", "Crash Type", 
                             "Vehicle Description", "Description")


ScorecardMVAs <- as.data.frame(ScorecardMVAs)

wb <- loadWorkbook("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx")
sheets <- getSheets(wb)
removeSheet(wb, sheetName="MVA Cases")
yourSheet <- createSheet(wb, sheetName="MVA Cases")
addDataFrame(ScorecardMVAs, yourSheet, row.names = FALSE)
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/Scorecard.xlsx")



#### InSite Report

wb <- loadWorkbook("//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")
   
sheets <- getSheets(wb)


InjuryIllnessTypesToCount <- c("Strain/Sprain",
                               "Bruise/Contusion",
                               "Orthopedic condition - Occupational",
                               "Dislocation",
                               "Fracture/Break",
                               "Musculoskeletal - Cumulative Trauma",
                               "Carpal Tunnel Syndrome",
                               "Repeated Motion Trauma")

#Count all Mus-Skeletal-OSHAs

temp <- Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction") &
                  Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
           group_by(Calc_Year) %>%
           summarise(Yearct = n())


removeSheet(wb, sheetName="OSHAs")
newSheet <- createSheet(wb, sheetName="OSHAs")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")

#Count all Mus-Skeletal-OSHAs  YTD

temp <- Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction") &
                    Calc_Month <= ReportMonth &
                    Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
  group_by(Calc_Year) %>%
  summarise(Yearct = n())

removeSheet(wb, sheetName="OSHAsYTD")
newSheet <- createSheet(wb, sheetName="OSHAsYTD")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")


#Count all Mus-Skeletal injuries

temp <- Events %>% filter(Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
  group_by(Calc_Year) %>%
  summarise(Yearct = n())

removeSheet(wb, sheetName="AllInjuries")
newSheet <- createSheet(wb, sheetName="AllInjuries")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")


#Count all Mus-Skeletal injuries

temp <-Events %>% filter(Calc_Month <= ReportMonth &
                    Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
  group_by(Calc_Year) %>%
  summarise(Yearct = n())

removeSheet(wb, sheetName="AllInjuriesYTD")
newSheet <- createSheet(wb, sheetName="AllInjuriesYTD")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")


#Breakdown of Current Year injuries

temp <- Events %>% filter(Calc_Month <= ReportMonth &
                          Calc_Year == CurrentYear &
                    Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
  group_by(Calc_InjIll) %>%
  summarise(Yearct = n()) %>%
  mutate(Percentage = round(100*Yearct / sum(Yearct),2))

removeSheet(wb, sheetName="CurrentYrInjuriesByType")
newSheet <- createSheet(wb, sheetName="CurrentYrInjuriesByType")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")


#Breakdown of Body Part Affected

BodyParts <- Events %>% filter(Calc_Month <= ReportMonth &
                                 Calc_Year == CurrentYear &
                                 Calc_InjIll %in% InjuryIllnessTypesToCount) %>%
              select(`Part of Body Affected`) %>% 
              separate(`Part of Body Affected`, c("BP1", "BP2", "BP3", 
                                                               "BP4", "BP5", "BP6",
                                                               "BP7", "BP8", "BP9", "BP10"), sep = ",", fill = "right")


BodyParts <- BodyParts %>% gather("test", `Parts of Body Affected`, BP1:BP10,na.rm = TRUE)
BodyParts$`Parts of Body Affected` <- trimws(BodyParts$`Parts of Body Affected`)

temp <- BodyParts %>% group_by(`Parts of Body Affected`) %>%
                  summarise(ct = n()) %>%
                  arrange(desc(ct))

removeSheet(wb, sheetName="CurrentYrBodyPartAffected")
newSheet <- createSheet(wb, sheetName="CurrentYrBodyPartAffected")
addDataFrame(data.frame(temp), newSheet, row.names = FALSE)    
saveWorkbook(wb, "//gccscif01.psegliny.com/Safety/Incidents-Scorecard Data/2017/InsiteMonthlyRequest/InsiteMonthlyRequest.xlsx")


