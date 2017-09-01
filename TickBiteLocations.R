
### Tick bit script


library(png)
library(ggplot2)
library(ggimage)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(readxl)
library(ggmap)

temp <- Events %>% filter(`Nature of Injury`=="Bite-Insect (Not Sting)" ) %>% select(`Part of Body Affected`) 

numberCases <- dim(temp)[1]

bodyparts <- vector(mode="character")

for(i in 1:dim(temp)[1]){
  temp2 <- gregexpr(pattern = ',',temp$`Part of Body Affected`[i])
  
  if(temp2[[1]][1] == -1){
    bodyparts <- append(bodyparts,temp$`Part of Body Affected`[i])
  } else {
    
    temp2 <- strsplit(temp$`Part of Body Affected`[i], ",")
    bodyparts <- append(bodyparts,temp2[[1]][1])
    bodyparts <- append(bodyparts,temp2[[1]][2])
  }
}

bodyparts <- trimws(bodyparts)

bpunique <- as.data.frame(unique(bodyparts))
bpunique$Count <- NA

colnames(bpunique) <- c("BodyPart","Count")
bpunique$BodyPart <- as.character(bpunique$BodyPart)

for(i in 1:dim(bpunique)[1]){
  
  bpunique$Count[i] <- sum(bodyparts==bpunique$BodyPart[i])
  
}

bpSum <- sum(bpunique$Count)

bpunique$BPfrequent <- ifelse(bpunique$Count/bpSum>.06,1,0)
bpunique$BPfrequent <- factor(bpunique$BPfrequent)



write.csv(bpunique,"//gccscif01.psegliny.com/Safety/Murphy/OpenProjects/TickBiteLocations/bpunique.csv", row.names = FALSE)
bpJoin <- read_excel("//gccscif01.psegliny.com/Safety/Murphy/OpenProjects/TickBiteLocations/bpuniqueLocs.xlsx")

bpunique <- left_join(bpunique,bpJoin,key = "BodyPart")


img <- readPNG("//gccscif01.psegliny.com/Safety/Murphy/OpenProjects/TickBiteLocations/bodyTemplate.png") 

final <- ggimage(img, fullpage = FALSE, scale_axes = FALSE) + 
          geom_point(aes(x = xcoord * dim(img)[2], 
                         y = ycoord* dim(img)[1]),color = '#ff993a', data=bpunique) +
          geom_label(aes(x = xcoord * dim(img)[2], 
                         y = ycoord* dim(img)[1], 
                         label = paste0(BodyPart," - ",Count),
                         size = BPfrequent, hjust = 1*hjust), data=bpunique,
                         color = '#0f4296', nudge_x = bpunique$nudgeAdj)  +
          scale_size_manual(values = c(3,5),
                            breaks = c(0,1)) +
  theme_map() + 
  ggtitle("Where are ticks found? - 2016 - YTD 2017") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + 
  labs( caption = paste0("Results based on incidents between 1/1/2016 and ", month(today()),"/",day(today()),"/",year(today()),".")) 

ggsave(final, file = "C:/Users/murphyd/Desktop/OpenProjects/TickBiteLocations/ManWithTickCounts.png")



## Used for Location of Ticks

write.csv(Events %>% filter(`Nature of Injury` == "Bite-Insect (Not Sting)" & is.na(MVAZipCodeUpdated) == FALSE ) %>%
            select(`System Event ID`, Date, Calc_Year, MVAZipCodeUpdated), 
          "//gccscif01.psegliny.com/Safety/Murphy/Data Downloads/TickBiteLocations.csv", 
          row.names = FALSE)