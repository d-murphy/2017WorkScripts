

library(dplyr)
library(caret)




FleetHist <- data.frame(Month = numeric(0), Year = numeric(0))

FleetVehIDs <- Fleet %>% filter(`Fleet_User status`=="ACTV" & 
                                  Fleet_Class != "Off Road") %>%
                         select(Fleet_VehID, Fleet_Location, `Fleet_Functional Loc.`, `3-16Prior12MthsMiles`,
                                `3-17Prior12MthsMiles`, Fleet_Category)

for(i in 2015:2017){
  
  if(i %in% c(2015,2016)){
    for(j in 1:12){
      
      FleetVehIDs$Month <- j
      FleetVehIDs$Year <- i
      FleetHist <- bind_rows(FleetHist, FleetVehIDs)
    }
  }else{
    for(j in 1:7){
      
      FleetVehIDs$Month <- j
      FleetVehIDs$Year <- i
      FleetHist <- bind_rows(FleetHist, FleetVehIDs)
    }
  }
}

remove(FleetVehIDs)


Tickets <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/Data Uploads/Ticket Log 2017.xlsx", sheet = 3)

Tickets <- Tickets %>% filter(`Type (Red Light, Speeding)` != "SPEEDING ") %>% 
                       select(`Violation Date`, `PSEG Unit`)

VehsWithTix <- unique(Tickets$`PSEG Unit`)

FleetHist$TixLast12Mths <- NA

for(i in 1:dim(FleetHist)[1]){
  
  if(FleetHist$Fleet_VehID[i] %in% VehsWithTix){
    
    temp <- Tickets %>% filter(`PSEG Unit` == FleetHist$Fleet_VehID[i] & 
                               `Violation Date` < ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i],"-01")) &
                               `Violation Date` > ymd(paste0(FleetHist$Year[i]-1,"-",FleetHist$Month[i],"-01"))) %>%
                        summarise(ct = n())
    FleetHist$TixLast12Mths[i] <- temp$ct[1]
  }
  
}


FleetHist$TixNextMth <- NA

for(i in 1:dim(FleetHist)[1]){
  
  if(FleetHist$Fleet_VehID[i] %in% VehsWithTix){
    
    temp <- Tickets %>% filter(`PSEG Unit` == FleetHist$Fleet_VehID[i] & 
                                 `Violation Date` > ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i],"-01")) &
                                 `Violation Date` < ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i]+1,"-01"))) %>%
      summarise(ct = n())
    FleetHist$TixNextMth[i] <- temp$ct[1]
  }
  
}



remove(VehsWithTix)

VehsWithMVAs <- unique(Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting")) %>%
                                  select(`Company Vehicle Number`))

VehsWithMVAs$`Company Vehicle Number` <- trimws(VehsWithMVAs$`Company Vehicle Number`)

FleetHist$MVAsLast12Mths <- NA

for(i in 1:dim(FleetHist)[1]){
  
  if(FleetHist$Fleet_VehID[i] %in% VehsWithMVAs$`Company Vehicle Number`){
    
    temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                              `Company Vehicle Number` == FleetHist$Fleet_VehID[i] &
                              Date < ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i],"-01")) &
                              Date > ymd(paste0(FleetHist$Year[i]-1,"-",FleetHist$Month[i],"-01"))) %>%
      summarise(ct = n())
    FleetHist$MVAsLast12Mths[i] <- temp$ct[1]
  }
  
}



FleetHist$MVAsNextMth <- NA

for(i in 1:dim(FleetHist)[1]){
  
  if(FleetHist$Fleet_VehID[i] %in% VehsWithMVAs$`Company Vehicle Number`){
    
    temp <- Events %>% filter(`MV Classification` %in% c("MV - On the job", "MC - Commuting") &
                                `Company Vehicle Number` == FleetHist$Fleet_VehID[i] &
                                Date > ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i],"-01")) &
                                Date < ymd(paste0(FleetHist$Year[i],"-",FleetHist$Month[i]+1,"-01"))) %>%
      summarise(ct = n())
    FleetHist$MVAsNextMth[i] <- temp$ct[1]
  }
  
}


FleetHist$`Fleet_Functional Loc.` <- as.factor(FleetHist$`Fleet_Functional Loc.`)
FleetHist$Fleet_Category <- factor(FleetHist$Fleet_Category)
FleetHist$TotalMiles <- FleetHist$`3-16Prior12MthsMiles` + FleetHist$`3-17Prior12MthsMiles`
FleetHist$TixLast12Mths <- ifelse(is.na(FleetHist$TixLast12Mths),0,FleetHist$TixLast12Mths)
FleetHist$TixNextMth <- ifelse(is.na(FleetHist$TixNextMth),0,FleetHist$TixNextMth)
FleetHist$MVAsLast12Mths <- ifelse(is.na(FleetHist$MVAsLast12Mths),0,FleetHist$MVAsLast12Mths)
FleetHist$MVAsNextMth <- ifelse(is.na(FleetHist$MVAsNextMth),0,FleetHist$MVAsNextMth)

FleetHist$MVAsNextMth <- ifelse(FleetHist$MVAsNextMth==0,0,1)



train <- FleetHist[sample(nrow(FleetHist)),]
train <- train %>% select(`Fleet_Functional Loc.`, Fleet_Category:TotalMiles)

test <- train[round(dim(train)[1]*.8):dim(train)[1],]
train <- train[1:round(dim(train)[1]*.8),]



plot(train)

model <- glm(MVAsNextMth ~ .,family = "binomial", train)
test$p <- predict(model, test, type = "response")
test$p_class <- ifelse(test$p > 0.008, 1, 0)

confusionMatrix(test$p_class, test$MVAsNextMth)
colAUC(test$p, test$MVAsNextMth, plotROC = TRUE)

