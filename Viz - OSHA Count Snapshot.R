library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(knitr)
library(tidyr)
library(ggthemes)


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

ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/OSHA Count Snapshot.png", width = 7,height = 3, dpi=200)

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

ggsave("//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/MVA Count Snapshot.png", width = 7,height = 3, dpi=200)

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
