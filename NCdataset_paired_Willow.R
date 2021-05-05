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
COMID_veg <- read.csv('~/Data/Modified/River/NCveg_COMID.csv', header=TRUE,sep=",")
flow_mod <- read.csv('~/Data/Original/River/USGS_AlteredFlow/MXStatus.csv', header=TRUE,sep=",")
stream <- left_join(COMID_veg,flow_mod,by="COMID")
stream$POLYGON_ID<-as.character(stream$POLYGON_ID)

######################################
###      Groundwater Levels         ##
######################################

#Load Groundwater Monitoring Well Data
df<- read.csv('~/Data/Modified/Groundwater/CASGEM_SelectElev.csv', header=TRUE, sep =",")
df$date <- as.Date(df$date, format = '%Y-%m-%d')
df$SITE_CODE <-as.character(df$SITE_CODE)
df$HR<- as.character(df$HR)
df$X<-NULL

#Convert CASGEM data from US empirical units to metric
df$WSE_m <- df$WSE*0.3048 #Water Surface Elevation at well
df$GSE_WSE_m <-df$GSE*0.3048 #Depth to Water at well

df$WSE <-NULL
df$GSE_WSE <-NULL
CASGEM<-na.omit(df)
CASGEM <- CASGEM[order(CASGEM$SITE_CODE,CASGEM$date),]



######################################
###      Sentinel-2 Indices         ##
######################################

### Load SpatialIntersect file from ArcGIS output that associates the closest well site_code to each veg polygon  ###

#####   WILLOW   ##### 
Willow_spatialintersect<- read.csv('~/Data/Modified/Groundwater/Willow_CASGEM_closest1km_SpatialJoin.csv', header=TRUE, sep=",")
Willow_spatialintersect$POLYGON_ID <-as.character(Willow_spatialintersect$POLYGON_ID)
Willow_spatialintersect <- data.frame(Willow_spatialintersect$POLYGON_ID,Willow_spatialintersect$SITE_CODE)
colnames(Willow_spatialintersect)<- c('POLYGON_ID','SITE_CODE')
Willow_spatialintersect[which(Willow_spatialintersect$SITE_CODE==" "),]<-NA
Willow_spatialintersect<- na.omit(Willow_spatialintersect)
Willow_DEM <- read.csv('~/Data/Modified/Groundwater/GoodingiiWillow_DEM.csv')  #Load mean elevation data for each veg polygon
Willow_DEM$POLYGON_ID <-as.character(Willow_DEM$POLYGON_ID)
Willow_DEM$Rowid <- NULL
Willow_DEM$COUNT <- NULL
Willow_DEM$AREA <- NULL
Willow_wells <- left_join(Willow_spatialintersect, Willow_DEM, by= "POLYGON_ID")
colnames(Willow_wells)<- c('POLYGON_ID','SITE_CODE','VEG_Elev')
Willow_wells<-left_join(Willow_wells,CASGEM,by="SITE_CODE")
Willow_wells$DTW <- Willow_wells$VEG_Elev - Willow_wells$WSE_m               #Calculate Depth to Groundwater (meters) for each veg polygon


###    Load NDVI data from GEE output for species of interest  ###

#####   WILLOW   #####
Willow_NDVI <- read.csv('~/Data/Original/Sentinel/Willow_NDVI_2015_2020.csv', header = TRUE, sep = ",")
Willow_NDVI$system.index <-NULL
Willow_NDVI$.geo <-NULL
Willow_NDVI$POLYGON_ID <- as.character(Willow_NDVI$POLYGON_ID)
Willow_NDVI$date <-as.Date(Willow_NDVI$date)
colnames(Willow_NDVI) <- c('POLYGON_ID', 'date', 'NDVI')
Willow_NDVI <- Willow_NDVI[order(Willow_NDVI$POLYGON_ID, Willow_NDVI$date),]
Willow_NDVI <- Willow_NDVI[!duplicated(Willow_NDVI),] #remove duplicated NDVI lines


##################################################################
##################################################################
###                    FUNCTIONS                                ##
##################################################################
##################################################################

#######################################
##   Function to Interpolate NDVI  data  ##
#######################################


SentinelInterp <- function (veg_NDVI, veg_wells){
  ## NDVI ##
  #Interpolate NDVI data and create a new data frame with daily results
  uniq_veg_NDVI<- unique(veg_NDVI$POLYGON_ID)
  
  z<- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("POLYGON_ID", "date","NDVI"))
  z$POLYGON_ID <- as.numeric(z$POLYGON_ID)
  z$date <- as.Date(z$date)
  z$NDVI <- as.numeric(z$NDVI)
  
  
  veg_NDVI_interp<- data.frame()
  for (i in 1:length(uniq_veg_NDVI)){
    z <- veg_NDVI[which(veg_NDVI$POLYGON_ID==uniq_veg_NDVI[i]),]
    zz <- data.frame(date = seq(z$date[1], z$date[nrow(z)], by =1))
    zzz <- merge(z, zz, by ="date", all=T) 
    zzz$LinApprox <- na.approx(zzz$NDVI)
    zzz$POLYGON_ID <- uniq_veg_NDVI[i]
    veg_NDVI_interp <- rbind(veg_NDVI_interp, zzz)
  }
  colnames(veg_NDVI_interp) <- c("date", "POLYGON_ID", "NDVI","NDVI_LinApprox")
  veg_NDVI_interp$NDVI <- NULL
  
  ## Join NDVI data with Groundwater Level Data ##
  
  
  results<- left_join(veg_NDVI_interp,veg_wells, by=c("POLYGON_ID","date"))
  results <- na.omit(results)
  results <-results[order(results$POLYGON_ID,results$date),]
  
  results$month<- as.numeric(month(results$date))
  for (i in 1:length(results$month)){
    if (results$month[i] >1 && results$month[i] <=3){
      results$season[i] <- "aJan-Mar"
      results$SEASON[i] <- 1
    }
    if (results$month[i] >3 && results$month[i] <=6) {
      results$season[i] <- "Apr-June"
      results$SEASON[i] <- 2
    }
    if (results$month[i] >6 && results$month[i] <=9) {
      results$season[i] <- "July-Sept"
      results$SEASON[i] <- 3
    }
    if (results$month[i] >9 && results$month[i] <=12){
      results$season[i] <- "Oct-Dec"
      results$SEASON[i] <- 4
    }
  }
  results$MonthYear <- format(as.Date(results$date), "%m-%Y")
  results$Year <- format(as.Date(results$date), "%Y")
  results
}


####################################################################################################################################
### Function to calculate monthly mean groundwater levels, NDVI  for each Polygon_ID for each every vegetation type 
####################################################################################################################################

MonthlyData<- function (veg,HR) {
  results <- data.frame()
  uniq_veg<- unique(veg$POLYGON_ID)
  
  for (i in 1:length(uniq_veg)){
    x<-veg[which(veg$POLYGON_ID==uniq_veg[i]),]
    NDVI_LinApprox<-ddply(x, .(MonthYear), summarize, median=median(NDVI_LinApprox))
    DTW <- ddply(x, .(MonthYear), summarize, median=median(DTW))
    SEASON<- ddply(x, .(MonthYear), summarize, SEASON=max(SEASON))
    y<- left_join(NDVI_LinApprox, DTW, by="MonthYear")
    y <- left_join(y, SEASON, by="MonthYear")
    y$POLYGON_ID <- uniq_veg[i]
    y$HR <-x$HR[1]
    results <- rbind(results, y)
  }
  colnames(results) <- c("MonthYear","NDVI_LinApprox","DTW","SEASON","POLYGON_ID","HR")
  
  for (i in 1:length(results$POLYGON_ID)){
    if (results$SEASON[i]==1){
      results$season[i]="aJan-Mar"
    }
    if (results$SEASON[i]==2) {
      results$season[i]= "Apr-June"
    }
    if (results$SEASON[i]==3) {
      results$season[i] = "July-Sept"
    }
    if (results$SEASON[i]==4) {
      results$season[i] = "Oct-Dec"
    }
  }
  
  results$Year <- substring(results$MonthYear,4,7)
  results
  
}


SeasonalData<- function (veg,HR) {
  results <- data.frame()
  uniq_veg<- unique(veg$POLYGON_ID)
  
  for (i in 1:length(uniq_veg)){
    x<-veg[which(veg$POLYGON_ID==uniq_veg[i]),]
    NDVI_LinApprox<-ddply(x, .(SEASON,Year), summarize, NDVImedian=median(NDVI_LinApprox))
    DTW <- ddply(x, .(SEASON,Year), summarize, DTWmedian=median(DTW))
    y<- left_join(NDVI_LinApprox, DTW, by=c("SEASON","Year"))
    y$POLYGON_ID <- uniq_veg[i]
    y$HR <-x$HR[1]
    results <- rbind(results, y)
  }
  colnames(results) <- c("SEASON","Year","NDVI_LinApprox","DTW","POLYGON_ID","HR")
  
  for (i in 1:length(results$POLYGON_ID)){
    if (results$SEASON[i]==1){
      results$season[i]="aJan-Mar"
    }
    if (results$SEASON[i]==2) {
      results$season[i]= "Apr-June"
    }
    if (results$SEASON[i]==3) {
      results$season[i] = "July-Sept"
    }
    if (results$SEASON[i]==4) {
      results$season[i] = "Oct-Dec"
    }
  }
  
  # colnames(results) <- c("SEASON","NDVI_LinApprox","DTW","POLYGON_ID","season")
  results
}

##################################################################
###                     MAIN SCRIPT                             ##
##################################################################


# Apply function to interpolate NDVI data for each veg type and match up with groundwater data
Willow <- SentinelInterp(Willow_NDVI, Willow_wells)

#  Apply Function to calculate monthly data for each veg type
Willow_MonthMedian <- MonthlyData(Willow, Willow_spatialintersect)

#  Apply Function to calculate seasonal monthly data for each veg type
Willow_SeasonalMedian <- SeasonalData(Willow_MonthMedian, Willow_spatialintersect)


########################################################
##                COMBINE DATAFRAMES                 ##
########################################################
Willow$veg <- "Willow"
Willow_MonthMedian$veg <- "Willow"
Willow_SeasonalMedian$veg <- "Willow"
#write.csv(Willow_SeasonalMedian, "~/Data/Modified/Merged/Willow_SeasonalMedian.csv")

