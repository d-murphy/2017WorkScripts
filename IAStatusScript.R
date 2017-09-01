library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggthemes)





# Load and fix SIMS data on Events

Eventsdf <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/75003082.xlsx")
CCLut <- read.csv("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/OrgStructure.csv")
ReqChanges <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/EventIDsExcusedOrAdded.xlsx")
IASubmissionDate <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/EventIDsIASubmissionDate.xlsx")
SimsPostingDate <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/EventIDsSimsPostingDate.xlsx")
ActionStatusOverride <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/ActionIDStatusOverride.xlsx")
Actionsdf <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/75003082A.xlsx")

# Update Actions with incorrect Event IDs

ActionEventIDcorrections <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/ActionsWithUpdatedEventIDs.xlsx")
ActionEventIDcorrections$`New Event ID` <- as.character(ActionEventIDcorrections$`New Event ID`)
Actionsdf <- left_join(Actionsdf, ActionEventIDcorrections, by = "System Action ID")
Actionsdf$ID <- ifelse(is.na(Actionsdf$`New Event ID`), Actionsdf$ID, Actionsdf$`New Event ID`)
Actionsdf$`New Event ID` <- NULL



Eventsdf <- separate(Eventsdf, `Incident Date / Incident Time`, c("Date", "Time"),
                     sep = " ", remove = FALSE, convert = FALSE)

Eventsdf$Date <- ymd(Eventsdf$Date)
Eventsdf$Year <- year(Eventsdf$Date)



colnames(Eventsdf)[3] <- "EventID"
Eventsdf$EventID <- as.numeric(Eventsdf$EventID)

# Add Cost Centers



Eventsdf$CC <- substring(Eventsdf$`Cost Center`,1,4)
Eventsdf$CC <- as.numeric(Eventsdf$CC)

Eventsdf <- left_join(Eventsdf,CCLut, by = "CC")

remove(CCLut)

# Load IDs excused from IAs or required, filter out into two data frames. 


IAsReq <- ReqChanges %>% filter(Change == "Required") %>% select(EventID)
IAsExc <- ReqChanges %>% filter(Change == "Excused") %>% select(EventID)

# Load IA submission dates


IASubmissionDate$IASubmitDate <- ymd(IASubmissionDate$IASubmitDate)

# Load SIMS posting Date


SimsPostingDate$SimsPostDate <- ymd(SimsPostingDate$SimsPostDate)


# Load Required Actions

Actionsdf$`Completed Date` <- as.Date(Actionsdf$`Completed Date`, origin = "1899-12-30")
Actionsdf$ID <- as.numeric(Actionsdf$ID)
colnames(Actionsdf)[3] <- "EventID"


# Filter only trackable Events, that is OSHAs, non-excluable first-aids, and any special cases required (e.g. switching, high potential near miss)

Eventsdf <- Eventsdf %>% filter (Year>=2016 & ((
                                            `Event Subtype` %in% c("Days Away from Work", "Other Recordable Case", 
                                                                   "Job Transfer or Restriction", "First Aid Case") & 
                                            `Type of Person` == "Employee" ) | 
                                            EventID %in% IAsReq$EventID ))

remove(IAsReq)


Eventsdf <- left_join(Eventsdf, IASubmissionDate, by = "EventID")
Eventsdf <- left_join(Eventsdf, SimsPostingDate, by = "EventID")

remove(SimsPostingDate, IASubmissionDate)

Eventsdf$DaysToSims <- Eventsdf$SimsPostDate - Eventsdf$Date
Eventsdf$DaysToIA <- Eventsdf$IASubmitDate - Eventsdf$Date

IAsExc$DaysToIAClass <- "Excused"
IAsExc$IAStatus <- "Excused"
IAsExc$DaysToSimsClass <- "Excused"
Eventsdf <- left_join(Eventsdf, IAsExc, by = "EventID")
remove(IAsExc)

# Days to IA Class

Eventsdf$DaysToIAClass <- ifelse(Eventsdf$DaysToIA < 3, "2 Days or Less", Eventsdf$DaysToIAClass)
Eventsdf$DaysToIAClass <- ifelse(Eventsdf$DaysToIA < 5 & Eventsdf$DaysToIA >= 3, "3 - 5 Days", Eventsdf$DaysToIAClass)
Eventsdf$DaysToIAClass <- ifelse(Eventsdf$DaysToIA >= 5, "6+ Days", Eventsdf$DaysToIAClass)

# Days to SIMS Class

Eventsdf$DaysToSimsClass <- ifelse(Eventsdf$DaysToSims < 1, "Same Day", Eventsdf$DaysToSimsClass)
Eventsdf$DaysToSimsClass <- ifelse(Eventsdf$DaysToSims < 2 & Eventsdf$DaysToSims >= 1, "Next Day", Eventsdf$DaysToSimsClass)
Eventsdf$DaysToSimsClass <- ifelse(Eventsdf$DaysToSims < 3 & Eventsdf$DaysToSims >= 2, "Two Days", Eventsdf$DaysToSimsClass)
Eventsdf$DaysToSimsClass <- ifelse(Eventsdf$DaysToSims >= 3, "3+ Days", Eventsdf$DaysToSimsClass)

# IA Status

Eventsdf$IAStatus <- ifelse(is.na(Eventsdf$DateOnIA) & is.na(Eventsdf$IAStatus),"Outstanding",Eventsdf$IAStatus)
Eventsdf$IAStatus <- ifelse(Eventsdf$DateOnIA == "No IA" & is.na(Eventsdf$IAStatus), "IA not completed", Eventsdf$IAStatus)
Eventsdf$IAStatus <- ifelse(Eventsdf$DateOnIA == "No" & is.na(Eventsdf$IAStatus), "Completed", Eventsdf$IAStatus)
Eventsdf$IAStatus <- ifelse(Eventsdf$DateOnIA == "Yes" & is.na(Eventsdf$IAStatus), "Completed", Eventsdf$IAStatus)



# export rows that have potential issues


PotentialIssues <- Eventsdf %>% filter((DateOnIA != "Yes" | is.na(DateOnIA))| is.na(SimsPostDate))

write.csv(PotentialIssues, "//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/PotentialIssues.csv")



# IA Status



# Check on outstanding actions

EventsWithIAs <- Eventsdf %>% filter(IAStatus != "Excused") %>% select(EventID)

Actionsdf <- Actionsdf %>% filter(EventID %in% EventsWithIAs$EventID & (!`System Action ID` %in%    #excludes actions which would be counted in error
                                                                          c(850, 851, 859, 861, 875, 887, 888, 1050, 1051, 1209, 1213, 1245, 1268, 1307, 1308, 1380)
                                                                        ))

EventdfJoin <- Eventsdf %>% select (EventID, Line.of.business, Division, Year)

Actionsdf <- left_join(Actionsdf, EventdfJoin, by = "EventID")
remove(EventdfJoin)
colnames(Actionsdf)[23] <- "YearOfEvent"
Actionsdf$YearOfAction <- year(Actionsdf$`Target Date`)



Actionsdf$StatusReport <- ifelse(is.na(Actionsdf$`Completed Date`),"Incomplete","Complete")

#override action status in SIMS
Actionsdf$StatusReport <- ifelse(Actionsdf$`System Action ID` %in% ActionStatusOverride$ID,"Complete",Actionsdf$StatusReport )
remove(ActionStatusOverride)

Actionsdf$StatusReport <- ifelse(Actionsdf$StatusReport == "Incomplete" & Actionsdf$`Target Date` < today(),
                                 "Overdue",Actionsdf$StatusReport )


ActionSummary <- Actionsdf %>% group_by(StatusReport, Line.of.business,YearOfEvent,YearOfAction) %>% summarise(Count = n())
ActionSummary <- spread(ActionSummary, StatusReport, Count, fill=0)

write.csv(ActionSummary, 
          "//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/ActionSummary.csv",
          row.names = FALSE)

OutstandingActions <- Actionsdf %>% filter(StatusReport != "Complete")
temp <- Eventsdf %>% select(EventID, `Incident Long Description`)
OutstandingActions <- left_join(OutstandingActions,temp,by = "EventID")

OutstandingActions <- OutstandingActions %>% 
                          select(`System Action ID`,Division, `Action Assignee`, Recommendation,
                                                    `Target Date`, `Original Target Date`, `Incident Long Description`) %>%
                          arrange(`Target Date`)

write.csv(OutstandingActions,
          "//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/OutstandingActions.csv",
          row.names = FALSE)

remove(ActionSummary)


EventSummary <- Eventsdf %>% group_by(IAStatus, Line.of.business,Year) %>% summarise(Count = n())
EventSummary <- spread(EventSummary, IAStatus, Count, fill=0)
EventSummary <- EventSummary %>% arrange(Year,Line.of.business)

write.csv(EventSummary, 
          "//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/EventSummary.csv",
          row.names = FALSE)

OutstandingIAs <- Eventsdf %>% filter(IAStatus %in% c("IA not completed", "Outstanding") & Year == 2017) %>%
                                select(EventID, Division,Reporter, Date, `Event Subtype`, IAStatus, `Incident Alert Description` )

write.csv(OutstandingIAs,"//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/OutstandingIAs.csv",
          row.names = FALSE)

# graphs for monthly report report. 

#Page 1

Page1Export <- Eventsdf %>% group_by(Year, IAStatus) %>% summarize(Count = n())



#Page 3 

# Eventsdf %>% ggplot(aes(x=Year, fill=IAStatus)) + geom_bar()

Page3Export <- Eventsdf %>% group_by(Year, IAStatus) %>% summarize(Count = n())

Page3Export %>% ggplot(aes(x=Year,y=Count,fill=factor(IAStatus))) + 
        geom_bar(stat = "identity",position = "dodge") + 
        geom_text(aes(label=Count),vjust=-.5,position=position_dodge(.9), size = 3) +
        scale_x_continuous(limits = c(2015.5,2017.5), breaks = c(2016,2017)) +
        theme_hc() +
        scale_fill_hc() + 
        ggtitle("IA Status for all Injuries and High Potential Near Misses") + 
        labs(fill = "", caption = "'IA not completed' are events where an IA is no longer expected.  IAs labeled as 'Outstanding' are still expected to be submitted.") +
        theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size =8))
        

# Page 4 slide

Page4Export <- Eventsdf %>% filter(DaysToIAClass != "Excused") %>% 
             group_by(DaysToIAClass, Year) %>%
             summarize(Count = n()) 


Page4Export %>% ggplot(aes(x=Year,y=Count,fill=factor(DaysToIAClass))) + 
  geom_bar(stat = "identity",position = "dodge") + 
  geom_text(aes(label=Count),vjust=-.5,position=position_dodge(.9), size = 3)+ 
  scale_x_continuous(limits = c(2015.5,2017.5), breaks = c(2016,2017)) +
  theme_hc() +
  scale_fill_hc() + 
  ggtitle("Number of days to Complete IA Report") + 
  labs(fill = "", caption = "Incomplete IAs are not factored into Counts.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size =8))

# Page 4 slide potential replacement

Eventsdf$DaysToIAwithMissing <- ifelse(Eventsdf$IAStatus == "Outstanding",as.integer(today()-Eventsdf$Date),Eventsdf$DaysToIA)
Eventsdf$YearWithOutstanding <- ifelse(Eventsdf$IAStatus == "Outstanding",paste0(Eventsdf$Year," - Outstanding"),Eventsdf$Year)

Eventsdf %>% filter(IAStatus != "Excused") %>% 
    ggplot(aes(x=DaysToIAwithMissing, fill = factor(YearWithOutstanding))) + geom_histogram(binwidth = 5,center=2.5)  +
    theme_hc() +
    scale_fill_hc() + 
    ggtitle("Number of days to Complete IA Report with Outstanding Reports") + 
    labs(fill = "", x = "Number of Days", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_continuous(breaks=seq(0,max(Eventsdf$DaysToIAwithMissing, na.rm = TRUE),5))

# Page 5 slide

Page5Export <- Eventsdf %>% filter(IAStatus == "Completed") %>% group_by(Line.of.business, Year) %>%
             summarize(Avg = mean(DaysToIA, na.rm= TRUE))

Page5Export %>% ggplot(aes(x=Year,y=Avg,fill=factor(Line.of.business))) + 
  geom_bar(stat = "identity",position = "dodge") + 
  geom_text(aes(label= round(Avg, digits = 1)),vjust=-.5,position=position_dodge(.9), size = 3) + 
  scale_x_continuous(limits = c(2015.5,2017.5), breaks = c(2016,2017)) +
  theme_hc() +
  scale_fill_hc() + 
  ggtitle("Average Number of Days to Complete IA Report by Line of Business") + 
  labs(fill = "", caption = "Incomplete IAs are not factored into Averages.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size =8))


# Page 6 slide

Page6Export <- Eventsdf %>% filter (DaysToSims >= 0) %>% group_by(DaysToSimsClass) %>% summarize(Count = n())
Page6Export <- ungroup(Page6Export)
Page6Export$DaysToSimsClass <- factor(Page6Export$DaysToSimsClass, 
                                      levels = c("Same Day", "Next Day", "Two Days", "3+ Days"))
Page6Export <- Page6Export %>% arrange(DaysToSimsClass)
Page6Export$acum <- NA
Page6Export$acum[1] <- Page6Export$Count[1]
Page6Export$acum[2] <- Page6Export$acum[1] + Page6Export$Count[2]
Page6Export$acum[3] <- Page6Export$acum[2] + Page6Export$Count[3]
Page6Export$acum[4] <- Page6Export$acum[3] + Page6Export$Count[4]
Page6Export$acumNorm <- Page6Export$acum / Page6Export$acum[4]


# 4 is input as the number of rows variable, nr.  Page6Export$acum[4] replace N
# see original code here:  https://rpubs.com/dav1d00/ggpareto

Df_ticks <- data.frame(xtick0 = rep(4 +.55, 11), xtick1 = rep(4 +.59, 11), 
                       ytick = seq(0, Page6Export$acum[4], Page6Export$acum[4]/10))
y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")

g <- factor(Page6Export$DaysToSimsClass)

Page6Export %>% ggplot(aes(x=DaysToSimsClass,y=Count,fill = "1")) + 
  geom_bar(stat = "identity")  + 
  geom_text(aes(label=Count),position=position_stack(vjust=0.5), size = 3) +   
  geom_line(aes(x=DaysToSimsClass,y=acum, group=1)) +
  geom_text(aes(x=DaysToSimsClass, y = acum, label=paste0(round(acumNorm*100,digits = 1)," %")), pch = 19, vjust=-1, size = 3) +
  scale_y_continuous(breaks=seq(0, Page6Export$acum[4], Page6Export$acum[4]/10), limits=c(-.02 * Page6Export$acum[4], Page6Export$acum[4] * 1.02)) +
  guides(fill = FALSE, color = FALSE) +
  annotate("rect", xmin = 4 + .55, xmax = 4 + 1, 
           ymin = -.02 * Page6Export$acum[4], ymax = Page6Export$acum[4] * 1.02, fill = "white") +
  annotate("text", x = 4 + .8, y = seq(0, Page6Export$acum[4], Page6Export$acum[4]/10), label = y2, size = 3.5) +
  geom_segment(x = 4 + .55, xend = 4 + .55, y = -.02 * Page6Export$acum[4], yend = Page6Export$acum[4] * 1.02, color = "grey50") +
  geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
  theme_hc() +
  scale_fill_hc() + 
  ggtitle("Number of Days to Complete SIMS Report")  + 
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Days from Event")

# Page 7 slide


Actionsdf$PlotXAxis <- paste0(Actionsdf$YearOfAction, " - ", Actionsdf$Line.of.business)

Page7Export <- Actionsdf %>% group_by(PlotXAxis,StatusReport) %>% summarise(Count = n())
Page7Export$StatusReport <- ifelse(Page7Export$StatusReport=="Incomplete", "Incomplete, but not past due", Page7Export$StatusReport)

Page7Export %>% ggplot(aes(x=PlotXAxis, y=Count, fill=StatusReport, label = Count)) + 
  geom_bar(stat="identity") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), color="white") +
  theme_hc() +
  scale_fill_hc() + 
  ggtitle("Action Status by Year and Line of Business") + 
  labs(fill = "", x="", y="Count") + 
  scale_x_discrete(limits = c("2016 - Cust Srvs","2016 - T&D Elec Ops","2017 - Cust Srvs", "2017 - T&D Elec Ops")) +
  theme(plot.title = element_text(hjust = 0.5))

write.csv(Actionsdf %>% 
            filter(StatusReport=="Overdue") %>% 
            select (`System Action ID`, Division, `Action Assignee`, Recommendation, `Target Date`) %>%
            arrange(`Target Date`), 
          "//gccscif01.psegliny.com/Safety/Murphy/FilesToMaintain/Monthly IA Report and Action Tracking/OverdueActionsForReport.csv")
