library(dplyr)
library(xlsx)
library(stringr)
library(tidyr)


BodyPartCoord <- read.xlsx("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/BodyPartCoord.xlsx", sheetName = "Sheet1")

colnames(BodyPartCoord) <- c("Parts of Body Affected", "Xcoord", "Ycoord")



InjuriesTableau <- Events %>% filter(`Event Subtype` %in% c("Report Only", "First Aid Case", "Days Away from Work", 
                                                            "Job Transfer or Restriction", "Other Recordable Case")) %>%
                              select(`System Event ID`, Calc_InjIll, `Accident Type`, 
                                     OrgStruct_Line.of.business, OrgStruct_Department, `Personnel Sub Area`, `Event Subtype`, 
                                     Date, Calc_Hour, `Incident Long Description`, `Part of Body Affected`, `Total Lost Days`, 
                                     `Total Restricted Days`)

UpdatedNames <- read.xlsx("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/NewNamesInjIllAccType.xlsx", sheetName = "Sheet1",
                          stringsAsFactors = FALSE)

InjuriesTableau <- left_join(InjuriesTableau, UpdatedNames, by = c("Calc_InjIll" = "Original"))
colnames(InjuriesTableau)[14] <- "Injury or Illness Type (Name updated)"

InjuriesTableau <- left_join(InjuriesTableau, UpdatedNames, by = c("Accident Type" = "Original"))
colnames(InjuriesTableau)[15] <- "Accident Type (Name updated)"

InjuriesTableau$`Injury or Illness Type (Name updated)` <- ifelse(is.na(InjuriesTableau$`Injury or Illness Type (Name updated)`), 
                                                   "Not Known", InjuriesTableau$`Injury or Illness Type (Name updated)`)

InjuriesTableau$`Accident Type (Name updated)` <- ifelse(is.na(InjuriesTableau$`Accident Type (Name updated)`), 
                                                   "Not Known", InjuriesTableau$`Accident Type (Name updated)`)


InjuriesTableau <- InjuriesTableau %>% filter(OrgStruct_Department %in% c("Operations-East Division",
                                                                          "Operations-West Division",
                                                                          "Meter Services",
                                                                          "PSEG LI FEMA",
                                                                          "Training Support & Contractor Svcs",
                                                                          "Planning Resources & Engineering",
                                                                          "Revenue Operations",
                                                                          "Projects & Construction",
                                                                          "Transmission Ops",
                                                                          "Customer Contact & Billing",
                                                                          "T&D Services",
                                                                          "Energy Efficiencey & Renewables",
                                                                          "Security",
                                                                          "Information Technology",
                                                                          "Constomer Experience & Utility Marketing",
                                                                          "T&D Emergency Planning"
                                                                          ))

BodyParts <- Events %>% filter(`Event Subtype` %in% c("Report Only", "First Aid Case", "Days Away from Work", 
                                                            "Job Transfer or Restriction", "Other Recordable Case")) %>%
  select(`System Event ID`, `Part of Body Affected`)


BodyParts <- BodyParts %>% separate(`Part of Body Affected`, c("BP1", "BP2", "BP3", 
                                                               "BP4", "BP5", "BP6",
                                                               "BP7", "BP8", "BP9", "BP10"), sep = ",", fill = "right")


BodyParts <- BodyParts %>% gather("test", `Parts of Body Affected`, BP1:BP10,na.rm = TRUE)


BodyParts$test <- NULL
BodyParts$`Parts of Body Affected` <- trimws(BodyParts$`Parts of Body Affected`)


BodyParts <- left_join(BodyParts, BodyPartCoord, by = "Parts of Body Affected")

write.csv(BodyParts, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/BodyParts.csv", row.names = FALSE)
write.csv(InjuriesTableau, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/InjuriesForBodyParts.csv", row.names = FALSE)
