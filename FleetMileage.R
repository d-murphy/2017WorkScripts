
### Reviewing Fleet Vehicle Mileage


library(dplyr)
library(readxl)




FleetCatMvaRate <- as.data.frame(unique(Fleet$Fleet_Category))
colnames(FleetCatMvaRate) <- c("Fleet_Category")
FleetCatMvaRate$Fleet_Category <- as.character(FleetCatMvaRate$Fleet_Category)

temp <- Fleet %>% filter(`3-17Prior12MthsMiles` > 0 ) %>% 
                  select(Fleet_Category, `3-17Prior12MthsMiles`) %>% 
                  group_by(Fleet_Category) %>% 
                  summarise("3-17Prior12MthsMiles" = sum(`3-17Prior12MthsMiles`))
temp <- ungroup(temp)


FleetCatMvaRate <- left_join(FleetCatMvaRate, temp, by = "Fleet_Category")
  
temp <- Events %>% filter(Date < mdy("4/1/17") & 
                          Date > mdy("3/31/16") & 
                          `MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% 
                   select(Fleet_Category) %>% 
                   group_by(Fleet_Category) %>%
                   summarise("MVActYrEnding3-17" = n())
temp <- ungroup(temp)

FleetCatMvaRate <- left_join(FleetCatMvaRate, temp, by = "Fleet_Category")




temp <- Fleet %>% filter(`3-16Prior12MthsMiles` > 0 ) %>% 
  select(Fleet_Category, `3-16Prior12MthsMiles`) %>% 
  group_by(Fleet_Category) %>% 
  summarise("3-16Prior12MthsMiles" = sum(`3-16Prior12MthsMiles`))
temp <- ungroup(temp)



FleetCatMvaRate <- left_join(FleetCatMvaRate, temp, by = "Fleet_Category")

temp <- Events %>% filter(Date < mdy("4/1/16") & 
                            Date > mdy("3/31/15") & 
                            `MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% 
  select(Fleet_Category) %>% 
  group_by(Fleet_Category) %>%
  summarise("MVActYrEnding3-16" = n())
temp <- ungroup(temp)



FleetCatMvaRate <- left_join(FleetCatMvaRate, temp, by = "Fleet_Category")

FleetCatMvaRate  <- FleetCatMvaRate %>% rowwise() %>% 
                          mutate(`Miles4_15-3_17` = sum(`3-16Prior12MthsMiles`, `3-17Prior12MthsMiles`, na.rm = TRUE))
FleetCatMvaRate <- ungroup(FleetCatMvaRate)

FleetCatMvaRate  <- FleetCatMvaRate %>% rowwise() %>%
                          mutate(`MVAct4_15-3_17` = sum(`MVActYrEnding3-16`, `MVActYrEnding3-17`, na.rm= TRUE))
FleetCatMvaRate <- ungroup(FleetCatMvaRate)


FleetCatMvaRate$VehicleRate <- FleetCatMvaRate$`MVAct4_15-3_17`  * 1000000 / FleetCatMvaRate$`Miles4_15-3_17`

temp <- Fleet %>% select(Fleet_Category) %>% group_by(Fleet_Category) %>% summarise(VehTypeCt = n())
temp <- ungroup(temp)

FleetCatMvaRate <- left_join(FleetCatMvaRate, temp, by = "Fleet_Category")


FleetLocMvaRate <- Fleet %>% 
                          filter(Fleet_Class %in% c("Aerial","Heavy Duty","Light Duty") & 
                                (`3-17Prior12MthsMiles` > 0 | 
                                 `3-16Prior12MthsMiles` > 0 | 
                                 `MVA Count` > 0)) %>% 
                           rowwise() %>%
                           mutate(`Miles4_15-3_17` = sum(`3-16Prior12MthsMiles`, `3-17Prior12MthsMiles`, na.rm = TRUE)) 
FleetLocMvaRate <- ungroup(FleetLocMvaRate)

FleetLocMvaRate <- FleetLocMvaRate %>% 
                           group_by(`Fleet_Functional Loc.`) %>%
                           summarise(MilesSum = sum(`Miles4_15-3_17`))
FleetLocMvaRate <- ungroup(FleetLocMvaRate)

temp <- Fleet %>% filter(Fleet_Class %in% c("Aerial","Heavy Duty","Light Duty") & 
                           (`3-17Prior12MthsMiles` > 0 | 
                              `3-16Prior12MthsMiles` > 0 | 
                              `MVA Count` > 0)) %>% 
                  select(`Fleet_Functional Loc.`) %>% 
                  group_by(`Fleet_Functional Loc.`) %>% 
                  summarise(LocTypeCt = n())
temp <- ungroup(temp)

FleetLocMvaRate <- left_join(FleetLocMvaRate, temp, by = "Fleet_Functional Loc.")





temp <-  Events %>% filter(Date < mdy("4/1/17") &
                             Date > mdy("3/31/15") &
                             `MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>%
  select(`Fleet_Functional Loc.`) %>%
  group_by(`Fleet_Functional Loc.`) %>%
  summarise(`MVAct4_15-3_17` = n())




 FleetLocMvaRate <- full_join(FleetLocMvaRate, temp, by = "Fleet_Functional Loc.")



FleetLocMvaRate <- FleetLocMvaRate %>% mutate(LocationMVARate = `MVAct4_15-3_17` * 1000000 / MilesSum)

MVAsforTableau <- Events %>% 
                        filter(`MV Classification` %in% c("MV - On the job","MC - Commuting") ) %>%
                        select(`System Event ID`, `MV Classification`, Fleet_Category, Calc_CrashType, Calc_CrashResp,
                                     Calc_Hour, `Fleet_Functional Loc.`, Fleet_Category,
                               Fleet_Class, MVAZipCodeUpdated, TownName, County, 
                               `Incident Long Description`, `Specific Location/Address where incident occured`,
                               OrgStruct_Line.of.business, OrgStruct_Department, Date)


write.csv(FleetCatMvaRate, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/FleetCatMvaRate.csv", row.names = FALSE)
write.csv(FleetLocMvaRate, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/FleetLocMvaRate.csv", row.names = FALSE)
write.csv(MVAsforTableau, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVAsforTableau.csv", row.names = FALSE)


remove(FleetCatMvaRate, FleetLocMvaRate)




#Compared zip code counts for Light Duty vs Heavy Duty, not really conclusive.  
# 
# temp <- MVAsforTableau %>% 
#               filter(Fleet_Class == "Light Duty") %>%
#               select(MVAZipCodeUpdated) %>%
#               group_by(MVAZipCodeUpdated) %>% 
#               summarise(LightDutyZipCt = n())
# 
# temp2 <- MVAsforTableau %>% 
#                filter(Fleet_Class == "Heavy Duty" | Fleet_Class == "Aerial") %>%
#                select(MVAZipCodeUpdated) %>%
#                group_by(MVAZipCodeUpdated) %>% 
#                summarise(HeavyDutyZipCt = n())
# 
# temp <- full_join(temp, temp2, by = "MVAZipCodeUpdated")
# 
# temp$LightDutyZipCt <- ifelse(is.na(temp$LightDutyZipCt), 0,temp$LightDutyZipCt)
# temp$HeavyDutyZipCt <- ifelse(is.na(temp$HeavyDutyZipCt), 0,temp$HeavyDutyZipCt)
# 
# temp$ZipCodeCtDif = temp$HeavyDutyZipCt - temp$LightDutyZipCt
# 
# write.csv(temp, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/ZipCodeCtDif.csv", row.names = FALSE)



### Finds Fleet_Class and Location Rates

temp <- Fleet %>% filter(Fleet_Class %in% c("Aerial","Heavy Duty","Light Duty") & 
                           (`3-17Prior12MthsMiles` > 0 | 
                              `3-16Prior12MthsMiles` > 0 | 
                              `MVA Count` > 0)) %>%
                  group_by(`Fleet_Functional Loc.`, Fleet_Class) %>%
                  summarise(ct = n(), Miles = sum(`3-17Prior12MthsMiles`, `3-16Prior12MthsMiles`, na.rm=TRUE))
temp <- ungroup(temp)

Join <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                            Date < mdy("4/1/17") & 
                            Date > mdy("3/31/15")) %>%
                   group_by(`Fleet_Functional Loc.`, Fleet_Class) %>%
                   summarise("2yearCt" = n())
Join <- ungroup(Join)


temp$key <- paste0(temp$`Fleet_Functional Loc.`,temp$Fleet_Class)
Join$key <- paste0(Join$`Fleet_Functional Loc.`,Join$Fleet_Class)
Join <- Join %>% select(key, `2yearCt`)


temp <- left_join(temp, Join, by = "key")
temp$key <- NULL

temp$Rate <- temp$`2yearCt` * 1000000 / temp$Miles

write.csv(temp, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/FleetLocClassRates.csv", row.names = FALSE)

### Finds rate for Fleet_Class

temp <- temp %>% select(-Rate)
temp <- temp %>% group_by(Fleet_Class) %>%
                 summarise(ClassMiles = sum(Miles, na.rm = TRUE), 
                           ClassMVAs = sum(`2yearCt`, na.rm = TRUE),
                           ClassVehCt = sum(ct, na.rm = TRUE))
temp$Rate <- temp$ClassMVAs * 1000000 / temp$ClassMiles

write.csv(temp, "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/FleetClassRates.csv", row.names = FALSE)

 # View(Fleet %>% 
 #        group_by(`Fleet_Functional Loc.`, Fleet_Class) %>%
 #           summarise(ct = n(), Miles = sum(`3-17Prior12MthsMiles`, `3-16Prior12MthsMiles`, na.rm=TRUE), 
 #                     MVATot = sum(`MVA Count`, na.rm = TRUE)) %>% 
 #           mutate(Rate = MVATot/Miles * 1000000))
  

# View(Fleet %>% group_by(`Fleet_Functional Loc.`) %>% 
#        summarise(TixTots = sum(Fleet_NumTickets, na.rm = TRUE),
#                  TixTotsWOspeed = sum(Fleet_NumTixWOspeed, na.rm = TRUE),
#                  MilesTots = sum(`3-16Prior12MthsMiles`, `3-17Prior12MthsMiles`, na.rm=TRUE)) %>% 
#      mutate(AllTixRate = TixTots / MilesTots * 1000000, NoSpeedTixRate = TixTotsWOspeed / MilesTots * 1000000))
# 
# View(Fleet %>% 
#           filter(Fleet_Class != "Off Road") %>%
#           group_by(`Fleet_Functional Loc.`, Fleet_Class) %>% 
#           summarise(VehCt = n(), TickTot = sum(Fleet_NumTickets, na.rm = TRUE),
#                     MVATots = sum(`MVA Count`, na.rm=TRUE), 
#                     MilesTots = sum(`3-17Prior12MthsMiles`, `3-16Prior12MthsMiles`, na.rm=TRUE)) %>% 
#   mutate(GroupRate = MVATots / MilesTots * 1000000) %>% 
#     filter(MVATots > 3))
# 
# View(Fleet %>%
#         filter(Fleet_Class != "Off Road") %>% 
#        mutate(MilesCat = round(`3-17Prior12MthsMiles` + `3-16Prior12MthsMiles`, -3)) %>% 
#        group_by(MilesCat) %>% 
#        summarise(MVATots = sum(`MVA Count`, na.rm=TRUE), 
#                  MilesTots = sum(`3-17Prior12MthsMiles`, `3-16Prior12MthsMiles`, na.rm=TRUE),
#                  VehCt = n(), TickTot = sum(Fleet_NumTickets, na.rm = TRUE)) %>% 
#        mutate(Rate = MVATots / MilesTots *1000000)) 
# 
# 
# 
# library (ggplot2)
# 
# 
# temp <- Fleet 
# temp$Tix <- ifelse((is.na(temp$Fleet_NumTickets)), "No Tickets","Has Tickets")
# 
# 
# View(temp %>% 
#        filter(Fleet_Class != "Off Road" & `Fleet_User status` != "PROC") %>%
#        group_by(Tix) %>% summarise(VehCt = n(), TickTot = sum(Fleet_NumTickets, na.rm = TRUE),
#                                           MVATots = sum(`MVA Count`, na.rm=TRUE), 
#                                           MilesTots = sum(`3-17Prior12MthsMiles`, `3-16Prior12MthsMiles`, na.rm=TRUE)) %>% 
#        mutate(GroupRate = MVATots / MilesTots * 1000000))

