require (ggplot2)
library (data.table)
library(plyr)
library(dplyr)
library(gridExtra)
library(TTR)
library(zoo)
library(RColorBrewer)
library(reshape)
library(lubridate)
library(lme4)
library(strucchange)
library(tree)
library(httr) #API
library(jsonlite) #API
library(rlist)
library(googledrive)
library(RCurl)
library(tidyquant)
library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(mblm)
library(segmented)
options(stringsAsFactors = FALSE)


##################################################################
###                    LOAD DATA                               ##
##################################################################


######################################
###      StreamFlow Data            ##
######################################
#import observed river data. Output from RiverData.R script.
RiverCombo<-read.csv("~/Data/Modified/River/combo_riverobs_veg.csv", header=TRUE, sep=",")
RiverCombo$POLYGON_ID <-as.character(RiverCombo$POLYGON_ID)

#Calculate the average monthly difference between observed and estimated flow for each polygon.
RiverCombo_month<- ddply(RiverCombo, .(month, POLYGON_ID, COMID), summarize, monthlymeandiff=mean(diff))
RiverCombo_month <- RiverCombo_month[order(RiverCombo_month$POLYGON_ID, RiverCombo_month$month),]


############################################
###      Paired NDVI & DTW Data           ##
############################################
Cottonwood_SeasonalMedian <- read.csv('~/Data/Modified/Merged/Cottonwood_SeasonalMedian.csv', header=TRUE,sep=",")
Willow_SeasonalMedian <- read.csv('~/Data/Modified/Merged/Willow_SeasonalMedian.csv', header=TRUE,sep=",")
ValleyOak_SeasonalMedian <- read.csv('~/Data/Modified/Merged/ValleyOak_SeasonalMedian.csv', header=TRUE,sep=",")

AllVeg_SeasonalMedian <- rbind (Cottonwood_SeasonalMedian, Willow_SeasonalMedian, ValleyOak_SeasonalMedian)
AllVeg_SeasonalMedian$POLYGON_ID <- as.character(AllVeg_SeasonalMedian$POLYGON_ID)

#Fix empty HR data
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64890)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64891)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64892)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64887)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64902)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==64897)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==149092)]<-"San Francisco Bay"
AllVeg_SeasonalMedian$HR[which(AllVeg_SeasonalMedian$POLYGON_ID==56309)]<-"Sacramento River"


##Remove outlier points from paired dataset in South Coast.  These are willow polygons that underwent
# arundo removal (restoration work) in the summer of 2020. (Source: Personal Communication with Prado Basin)

for (i in 1:length(AllVeg_SeasonalMedian$POLYGON_ID)){
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110853 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110943 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110794 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110959 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110945 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110876 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110963 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110872 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  if (AllVeg_SeasonalMedian$HR[i] == "South Coast" && AllVeg_SeasonalMedian$POLYGON_ID[i]==110880 && AllVeg_SeasonalMedian$Year[i]==2020 && AllVeg_SeasonalMedian$SEASON[i]==3){
    AllVeg_SeasonalMedian<-AllVeg_SeasonalMedian[-i,]
  }
  AllVeg_SeasonalMedian
} 



########################################################
##                SEGMENT DATAFRAMES                 ##
########################################################

AllVeg_SeasonalMedian=AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$DTW<=30 & AllVeg_SeasonalMedian$DTW>=0),]
AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$season=="aJan-Mar"),]<- NA
AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$season=="Oct-Dec"),]<- NA
AllVeg_SeasonalMedian<- na.omit(AllVeg_SeasonalMedian)


AllVeg_SeasonalMedian<- left_join(AllVeg_SeasonalMedian,RiverCombo, by="POLYGON_ID")
AllVeg_SeasonalMedian$veg<-factor(AllVeg_SeasonalMedian$veg,levels=c("Cottonwood","Willow","Valley Oak"))


#write.csv(AllVeg_SeasonalMedian, "~/Data/Modified/Merged/AllVeg_SeasonalMedian.csv")


