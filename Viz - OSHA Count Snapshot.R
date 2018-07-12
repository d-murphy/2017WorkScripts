library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(knitr)
library(tidyr)
library(ggthemes)


### First section creates OSHA count table

xcoord <- data.frame(xcoord = c(1,2,3,4), Calc_Year = c(2014,2015,2016,2017))
ycoord <- data.frame(ycoord = c(1.5,2), Type = c("YearEndCt", "YTDCt"))
Labels <- data.frame(Type = c("Label", "Label", "Label", "Label", "Label", "Label"), 
                     xcoord = c(-0.5,-0.5,1,2,3,4),
                     ycoord = c(1.5,2,2.5,2.5,2.5,2.5),
                     ValueToPrint = c("Year End Total", "YTD Total", "2014", "2015", "2016","2017"))

OSHAct <- Events %>% filter(`Type of Person` == "Employee" & `Event Subtype` %in% c("Days Away from Work",
                                                                                "Other Recordable Case",
                                                                                "Job Transfer or Restriction")) %>% 
                 group_by(Calc_Year) %>% 
                 summarise(YearEndCt =n())

Join <- Events %>% filter(`Type of Person` == "Employee" & `Event Subtype` %in% c("Days Away from Work",
                                                                                "Other Recordable Case",
                                                                                "Job Transfer or Restriction") &
                        (Calc_Month < month(today()) | (Calc_Month == month(today()) & Calc_Day < day(today())))) %>% 
  group_by(Calc_Year) %>% 
  summarise(YTDCt =n())

OSHAct <- ungroup(OSHAct)
Join <- ungroup(Join)

OSHAct <- left_join(OSHAct,Join,by="Calc_Year")

remove(Join)


OSHAct <- gather(OSHAct,"Type","ValueToPrint",2:3)
OSHAct <- left_join(OSHAct,xcoord,by="Calc_Year")
OSHAct <- left_join(OSHAct,ycoord,by="Type")

OSHAct$Year <- as.character(OSHAct$Calc_Year)
OSHAct$ValueToPrint <- as.character(OSHAct$ValueToPrint)


OSHAct <- bind_rows(OSHAct,Labels)


plot <- ggplot(OSHAct,aes(xcoord,ycoord,label = ValueToPrint,color=Type)) + geom_text(fontface="bold",size=6) +
  scale_x_continuous(limits = c(-1.5,5), breaks = c()) + 
  scale_y_continuous(limits = c(1.25, 2.75), breaks = c()) +
  geom_segment(x=.5,xend=.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=1.5,xend=1.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=2.5,xend=2.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=3.5,xend=3.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=-1.5,xend=4.5,y=1.75,yend=1.75,color="black",size=2) +
  geom_segment(x=-1.5,xend=4.5,y=2.25,yend=2.25,color="black",size=2) +
  labs(x="",y="") +
  ggtitle(paste0("OSHA Count Snapshot on ",month(today()),"/",day(today()),"/",year(today()))) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values = c("black","#152e56","#1f437c"),
                     breaks = c("YearEndCt", "YTDCt", "Label"),
                     guide = FALSE)
plot

#ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/OSHA Count Snapshot.png", width = 7,height = 3, dpi=200)

### Next section creates mva table

MVAct <- Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting")) %>% 
  group_by(Calc_Year) %>% 
  summarise(YearEndCt =n())

Join <- Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting") &
                            (Calc_Month < month(today()) | (Calc_Month == month(today()) & Calc_Day < day(today())))) %>% 
  group_by(Calc_Year) %>% 
  summarise(YTDCt =n())

MVAct <- ungroup(MVAct)
Join <- ungroup(Join)

MVAct <- left_join(MVAct,Join,by="Calc_Year")

remove(Join)


MVAct <- gather(MVAct,"Type","ValueToPrint",2:3)
MVAct <- left_join(MVAct,xcoord,by="Calc_Year")
MVAct <- left_join(MVAct,ycoord,by="Type")

MVAct$Year <- as.character(MVAct$Calc_Year)
MVAct$ValueToPrint <- as.character(MVAct$ValueToPrint)


MVAct <- bind_rows(MVAct,Labels)

plot <- ggplot(MVAct,aes(xcoord,ycoord,label = ValueToPrint,color=Type)) + geom_text(fontface="bold",size=6) +
  scale_x_continuous(limits = c(-1.5,5), breaks = c()) + 
  scale_y_continuous(limits = c(1.25, 2.75), breaks = c()) +
  geom_segment(x=.5,xend=.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=1.5,xend=1.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=2.5,xend=2.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=3.5,xend=3.5,y=1.25,yend=2.75,color="black",size=2) +
  geom_segment(x=-1.5,xend=4.5,y=1.75,yend=1.75,color="black",size=2) +
  geom_segment(x=-1.5,xend=4.5,y=2.25,yend=2.25,color="black",size=2) +
  labs(x="",y="") +
#  ggtitle(paste0("MVA Count Snapshot on ",month(today()),"/",day(today()),"/",year(today()))) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values = c("black","#152e56","#1f437c"),
                     breaks = c("YearEndCt", "YTDCt", "Label"),
                     guide = FALSE)
plot

#ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVA Count Snapshot.png", width = 7,height = 3, dpi=200)

## None of this worked
# ggsave(plot = plot, "////portal.psegliny.com//DavWWWRoot//HealthSafety//SiteAssets//SitePages//Reports//plot.png")
# ggsave(plot = plot, "//portal.psegliny.com/DavWWWRoot/HealthSafety/SiteAssets/SitePages/Reports/plot.png")
# ggsave(plot = plot, "\\\\portal.psegliny.com\\DavWWWRoot\\HealthSafety\\SiteAssets\\SitePages\\Reports\\plot.png")
# ggsave(plot = plot, "\\portal.psegliny.com\DavWWWRoot\HealthSafety\SiteAssets\SitePages\Reports\plot.png")
# 
# write.csv(OSHAct, "\\\\portal.psegliny.com@SSL\\HealthSafety\\SiteAssets\\SitePages\\Reports\\test1.csv")
# 
# 
# "//portal.psegliny.com/DavWWWRoot/HealthSafety/SiteAssets/SitePages/Home/plot.png"
# write.csv(OSHAct, "//portal.psegliny.com@SSL/DavWWWRoot/HealthSafety/SiteAssets/SitePages/Reports/test.csv")
# write.csv(OSHAct, "\\\\portal.psegliny.com\\HealthSafety\\SiteAssets\\SitePages\\Reports\\test.csv")

## Works but turned off
#ggsave("C:/Users/murphyd/Desktop/temp/plot.pdf", width = 7,height = 3, dpi=100)

# ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/OSHA Count Snapshot.png", width = 7,height = 3, dpi=100)

Events$Calc_DayNum <- yday(Events$Date)



# Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction")) %>%
#   ggplot(aes(Calc_DayNum, fill = `Event Subtype`)) + geom_histogram(binwidth = 1) + facet_grid(Calc_Year ~ . ) +
#   theme_hc() +
#   scale_fill_pander() + 
#   scale_y_continuous(breaks = c(0,1,2)) +
#   scale_x_continuous(breaks = c(1,32, 60, 91,121,152,182,213,244,274,305,335), 
#                      labels = c("Jan", "Feb", "Mar", "Apr","May","Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")) + 
#   labs(y = "Count on day", x = "", fill = "") +
#   ggtitle(paste0("PSEG Long Island OSHA History as of ", today())) + 
#   theme(strip.text.y = element_text(size = 16),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16),
#         legend.text = element_text(size =16))
#         #, colour = "black", angle = 90))

#binary

### This creates the view of days with OSHA



Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction")) %>%
  ggplot(aes(Calc_DayNum)) + geom_histogram(binwidth = 1, fill = "orange") + facet_grid(Calc_Year ~ . ) +
  # theme_hc() +
  scale_fill_pander() + 
  scale_y_continuous(limits =   c(0,1), breaks = c()) +
  scale_x_continuous(breaks = c(1,32, 60, 91,121,152,182,213,244,274,305,335), 
                     labels = c("Jan", "Feb", "Mar", "Apr","May","Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")) + 
  labs(y = "Mark for each day when an OSHA occurred", x = "", fill = "") +
  ggtitle(paste0("PSEG Long Island OSHA History as of ", today())) + 
  theme(strip.text.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size =14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        panel.background = element_rect(fill = "white"))

## Save as 1300 x 837

### This creates the view of days with OSHA

Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting")) %>%
  ggplot(aes(Calc_DayNum)) + geom_histogram(binwidth = 1, fill = "dark blue") + facet_grid(Calc_Year ~ . ) +
#  theme_hc() +
  scale_fill_pander() + 
  scale_y_continuous(limits =   c(0,1), breaks = c()) +
  scale_x_continuous(breaks = c(1,32, 60, 91,121,152,182,213,244,274,305,335), 
                     labels = c("Jan", "Feb", "Mar", "Apr","May","Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")) + 
  labs(y = "Mark for each day when an MVA occurred", x = "", fill = "") +
  ggtitle(paste0("PSEG Long Island MVA History as of ", today())) + 
  theme(strip.text.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size =14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        panel.background = element_rect(fill = "white"))


### This view creates the step chart of OSHA counts

years <- data.frame(year = c(2014:2018))
days <- data.frame(day = c(1:366))
CountByDay <- merge(years,days)

CountByDayJoin <- Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction")) %>% 
      group_by(Calc_Year, Calc_DayNum) %>% summarise(ct = n())

CountByDay <- left_join(CountByDay, CountByDayJoin, by = c("year" = "Calc_Year", "day" = "Calc_DayNum"))
remove(years, days, CountByDayJoin)
CountByDay$ct <- ifelse(is.na(CountByDay$ct),0,CountByDay$ct)
CountByDay <- CountByDay %>% group_by(year) %>% mutate(YearSum = cumsum(ct))

CountByDay$YearSum <- ifelse(CountByDay$year == year(today()) & CountByDay$day >= yday(today()), NA, CountByDay$YearSum )

plot <- CountByDay %>% ggplot(aes(x = day, y=YearSum,color = factor(year))) + 
  geom_step(size = 1) +
  scale_x_continuous(limits =   c(0,366),
                     breaks = c(1,32, 60, 91,121,152,182,213,244,274,305,335), 
                     labels = c("Jan", "Feb", "Mar", "Apr","May","Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")) + 
  scale_y_continuous(breaks = c(seq(0,60,10))) +
  theme_hc() +
  scale_color_pander() + 
  labs(y = "OSHA Counts by Year", x = "", color = "") +
  ggtitle(paste0("OSHA Count During Year\nas of ", today())) +
  theme(plot.title = element_text(hjust = 0.5))

plot

ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/OSHA Count History.png", width = 4,height = 3, dpi=100)



CountByDayJoin <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>% 
  group_by(Calc_Year, Calc_DayNum) %>% summarise(Mvct = n())


CountByDay <- left_join(CountByDay, CountByDayJoin, by = c("year" = "Calc_Year", "day" = "Calc_DayNum"))
remove(CountByDayJoin)
CountByDay$Mvct <- ifelse(is.na(CountByDay$Mvct),0,CountByDay$Mvct)
CountByDay <- CountByDay %>% group_by(year) %>% mutate(MvYearSum = cumsum(Mvct))

CountByDay$MvYearSum <- ifelse(CountByDay$year == year(today()) & CountByDay$day >= yday(today()), NA, CountByDay$MvYearSum )

plot <- CountByDay %>% ggplot(aes(x = day, y=MvYearSum,color = factor(year))) + 
  geom_step(size = 1) +
  scale_x_continuous(limits =   c(0,366),
                     breaks = c(1,32, 60, 91,121,152,182,213,244,274,305,335), 
                     labels = c("Jan", "Feb", "Mar", "Apr","May","Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")) + 
  scale_y_continuous(breaks = c(seq(0,120,20))) +
  theme_hc() +
  scale_color_pander() + 
  labs(y = "MVA Counts during Year", x = "", color = "") +
#  ggtitle(paste0("MVA Count During Year\nas of ", today())) +
  theme(plot.title = element_text(hjust = 0.5))

plot

ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVA Count History.png", width = 5,height = 4, dpi=100)




















### This shows the scatterplot of OSHA rate by location

OshaRatesByYard <- EmpDir %>% group_by(Location) %>% summarise(ct = n())
Hours <- 4347045 + 363740
OshaRatesByYard$Hours <- OshaRatesByYard$ct / sum(OshaRatesByYard$ct) * Hours

OshaRatesByYard <- left_join(OshaRatesByYard %>% filter(ct > 25), 
                             Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction") & 
                                           Calc_Year == 2017 ) %>% 
                             group_by(`Personnel Sub Area`) %>% summarise('Osha Ct' = n()), 
                             by = c("Location" = "Personnel Sub Area")) 

LUT <- c("Bethpage,NY" = "BP", 
         "Brentwood" = "BW", 
         "Bridgehampton-H" = "BH",
         "Greenlawn" = "GL", 
         "Hewlett" = "HE",
         "Hicksville" = "HI", 
         "Melville Call C" = "ME", 
         "Patchogue" = "PA", 
         "Port Jefferson" = "PJ", 
         "Riverhead" = "RH", 
         "Roslyn" = "RS", 
         "Uniondale" = "UN")

OshaRatesByYard$Location <- LUT[OshaRatesByYard$Location]
OshaRatesByYard$`Osha Ct` <- ifelse(is.na(OshaRatesByYard$`Osha Ct`),0, OshaRatesByYard$`Osha Ct`)


OshaRatesByYard %>% ggplot(aes(Hours, `Osha Ct`, label = Location)) + 
  geom_point() + 
  geom_text(hjust = -.1, nudge_x = 0.15, angle = 45) +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "OSHA Count", x = "Hours" , caption = "Hours are estimated based on head counts") +
  ggtitle("2017 OSHA Counts and Hours by Location") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(intercept = 0, slope = (4.75/1000000), size=1) + 
  geom_text(x = 1000000, y = 5, angle = 28, label = "Top Decile Rate")


### Distribution of days between OSHA

Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction") ) %>%
  ggplot(aes(DaysToLastOsha)) + geom_histogram(binwidth = 2, fill = "orange") +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", x = "Days Between OSHA Recordable Injuries")
  
### Distribution of days between MVA

Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting")) %>%
  ggplot(aes(DaysToLastMVA)) + geom_histogram(binwidth = 2, fill = "dark blue") +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", x = "Days Between MVAs")

### Distribution of days between Lost Time

Events %>% filter(`Event Subtype` == "Days Away from Work") %>%
  ggplot(aes(`Total Lost Days`)) + geom_histogram() +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", x = "Lost Days")


### MVA Type, Accident Type, Injury Type charts created for Ray's AET presentation



Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction")) %>% 
  group_by(`Accident Type`) %>%
  summarise(Count = n()) %>%
  mutate(Rank = rank(-Count, ties.method = "first")) %>%
  filter(Rank <= 7) %>%
  ggplot(aes(reorder(`Accident Type`,Count), Count, fill = factor(1))) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", x = "Accident Type", fill = "", title = "Accident Types 2014 - March 2018") +
  guides(fill = FALSE)

Events %>% filter(`Event Subtype` %in% c("Other Recordable Case", "Days Away from Work", "Job Transfer or Restriction") & 
                  Calc_InjIll != "None") %>% 
  group_by(Calc_InjIll) %>%
  summarise(Count = n()) %>%
  mutate(Rank = rank(-Count, ties.method = "first")) %>%
  filter(Rank <= 7) %>%
  ggplot(aes(reorder(Calc_InjIll,Count), Count, fill = factor(1))) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_hc() +
  scale_fill_pander() + 
  labs(y = "Count", x = "Injury Type", fill = "", title = "Top 7 Injury Types 2014 - March 2018") +
  guides(fill = FALSE)


Events %>% filter(`MV Classification` %in% c("MV - On the job","MC - Commuting") & Calc_Year == 2018) %>% 
  group_by(Calc_CrashType, Calc_CrashResp) %>%
  summarise(Count = n()) %>%
  mutate(Rank = rank(-Count, ties.method = "first")) %>%
  filter(Rank <= 7) %>%
  ggplot(aes(reorder(Calc_CrashType,Count,sum), Count,fill = Calc_CrashResp)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_hc() +
  scale_fill_pander() + 
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16))+
  labs(y = "Count", x = "Accident Type", fill = "")#, title = "MVA Types 2018") 








