require (ggplot2)
library (data.table)
library(dplyr)
library(plyr)
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
library(tidyverse) #API
library(rlist)
library(googledrive)
library(tidyquant)
library(RCurl)
library(ggstance)
library(forcats)
library(mblm)
options(stringsAsFactors = FALSE)


##################################################################
##################################################################
###                    LOAD DATA                               ##
##################################################################
##################################################################

######################################
###      StreamFlow Data            ##
######################################
# Load csv file containing which stream reach ("COMID") is associated to each vegetation polygon ("POLYGON_ID")
# This was determined in ArcGIS using a Spatial Join. The closest COMID to a POLYGON_ID within a 1km radius was used.
COMID_veg <- read.csv('~/Data/Modified/River/NCveg_COMID.csv', header=TRUE,sep=",")

# Load USGS Flow Modification data.  Variable Names include:
  # COMID = NHDPlus version 2.1 COMID
  # HiMag = Annual high-flow magnitude (0 or 1; 1 signifies that segment is likely to have altered hydrology for this flow metric)
  # LoMag = Annual low-flow magnitude (0 or 1; 1 signifies that segment is likely to have altered hydrology for this flow metric) 
  # LoVar = Year-to-year variability of annual low-flow magnitude (ditto)
  # Hi Var = ditto, but for high flow
  # LoDur = mean duration of low flows (ditto)
  # HiDur = mean duration of high flows (ditto)
  # MXStatus = sum across the six above metrics. A measure of “total” flow modification (ranges from 0 to 6)
  # MXStatus_prop = MXStatus divided by 6. This could be a measure of the overall “likelihood” of flow modification. 
      # Higher values indicate multiple attributes of the flow regime are altered, 
      # whereas lower number indicated fewer or no attributes of the flow regime are altered. 
      # NOTE that there are only 7 possible values here: 0, 0.17, 0.33, 0.50, 0.67, 0.83, and 1.0.
flow_mod <- read.csv('~/Data/Original/River/USGS_AlteredFlow/MXStatus.csv', header=TRUE,sep=",")

# Merge data frame containing list of unique vegetation POLYGON_IDs used in study with nearest COMID with
# USGS Flow modification data.
stream <- left_join(COMID_veg,flow_mod,by="COMID")
stream$POLYGON_ID<-as.character(stream$POLYGON_ID)



######################################
###      Hydrologic Regions         ##
######################################
### Load SpatialIntersect file from ArcGIS output that associates the Hydrologic Regions for each veg polygon  ###

#####   ValleyOak   ##### 
ValleyOak_spatialintersect<- read.delim('~/Data/Modified/Veg/ValleyOak_HydroRegion.txt', header=TRUE, sep=",")
ValleyOak_spatialintersect<-data.frame(ValleyOak_spatialintersect$POLYGON_ID, ValleyOak_spatialintersect$HR_NAME)
colnames(ValleyOak_spatialintersect) <- c("POLYGON_ID","HR_NAME")
ValleyOak_spatialintersect$POLYGON_ID <- as.character(ValleyOak_spatialintersect$POLYGON_ID)

#####   Willow   ##### 
Willow_spatialintersect<- read.delim('~/Data/Modified/Veg/Willow_HydroRegion.txt', header=TRUE, sep=",")
Willow_spatialintersect<-data.frame(Willow_spatialintersect$POLYGON_ID, Willow_spatialintersect$HR_NAME)
colnames(Willow_spatialintersect) <- c("POLYGON_ID","HR_NAME")
Willow_spatialintersect$POLYGON_ID <- as.character(Willow_spatialintersect$POLYGON_ID)

#####   COTTONWOOD   ##### 
Cottonwood_spatialintersect<- read.delim('~/Data/Modified/Veg/Cottonwood_HydroRegion.txt', header=TRUE, sep=",")
Cottonwood_spatialintersect<-data.frame(Cottonwood_spatialintersect$POLYGON_ID, Cottonwood_spatialintersect$HR_NAME)
colnames(Cottonwood_spatialintersect) <- c("POLYGON_ID","HR_NAME")
Cottonwood_spatialintersect$POLYGON_ID <- as.character(Cottonwood_spatialintersect$POLYGON_ID)

#Combine Veg spatial intersect
AllVeg_spatialintersect<- rbind(Cottonwood_spatialintersect,Willow_spatialintersect,ValleyOak_spatialintersect)



######################################
###      Sentinel-2 Indices         ##
######################################

### Load NDVI data from each veg polygon  ###

#####   COTTONWOOD   ##### 
Cottonwood_NDVI_1 <- read.csv('~/Data/Original/Sentinel/Cottonwood_NDVI_2015_2019.csv', header = TRUE, sep = ",")
Cottonwood_NDVI_2 <- read.csv('~/Data/Original/Sentinel/Cottonwood_NDVI_2019_2020.csv', header = TRUE, sep = ",")
Cottonwood_NDVI <-rbind(Cottonwood_NDVI_1,Cottonwood_NDVI_2)
Cottonwood_NDVI$system.index <-NULL
Cottonwood_NDVI$.geo <-NULL
Cottonwood_NDVI$POLYGON_ID <- as.character(Cottonwood_NDVI$POLYGON_ID)
Cottonwood_NDVI$date <-as.Date(Cottonwood_NDVI$date)
colnames(Cottonwood_NDVI) <- c('POLYGON_ID', 'date', 'NDVI')
Cottonwood_NDVI <- Cottonwood_NDVI[order(Cottonwood_NDVI$POLYGON_ID, Cottonwood_NDVI$date),]
Cottonwood_NDVI <- Cottonwood_NDVI[!duplicated(Cottonwood_NDVI),] #remove duplicated NDVI lines
Cottonwood <- left_join(Cottonwood_NDVI,Cottonwood_spatialintersect, by="POLYGON_ID")
Cottonwood$veg <- "Cottonwood"


#####   WILLOW   ##### 
Willow_NDVI <- read.csv('~/Data/Original/Sentinel/Willow_NDVI_2015_2020.csv', header = TRUE, sep = ",")
Willow_NDVI$system.index <-NULL
Willow_NDVI$.geo <-NULL
Willow_NDVI$POLYGON_ID <- as.character(Willow_NDVI$POLYGON_ID)
Willow_NDVI$date <-as.Date(Willow_NDVI$date)
colnames(Willow_NDVI) <- c('POLYGON_ID', 'date', 'NDVI')
Willow_NDVI <- Willow_NDVI[order(Willow_NDVI$POLYGON_ID, Willow_NDVI$date),]
Willow_NDVI <- Willow_NDVI[!duplicated(Willow_NDVI),] #remove duplicated NDVI lines
Willow <- left_join(Willow_NDVI,Willow_spatialintersect,by="POLYGON_ID")
Willow$veg <- "Willow"


#####   VALLEY OAK   #####
ValleyOak_NDVI <- read.csv('~/Data/Original/Sentinel/ValleyOak_NDVI_2015_2020.csv', header = TRUE, sep = ",")
ValleyOak_NDVI$system.index <-NULL
ValleyOak_NDVI$.geo <-NULL
ValleyOak_NDVI$POLYGON_ID <- as.character(ValleyOak_NDVI$POLYGON_ID)
ValleyOak_NDVI$date <-as.Date(ValleyOak_NDVI$date)
colnames(ValleyOak_NDVI) <- c('POLYGON_ID', 'date', 'NDVI')
ValleyOak_NDVI <- ValleyOak_NDVI[order(ValleyOak_NDVI$POLYGON_ID, ValleyOak_NDVI$date),]
ValleyOak_NDVI <- ValleyOak_NDVI[!duplicated(ValleyOak_NDVI),] #remove duplicated NDVI lines
ValleyOak <- left_join(ValleyOak_NDVI,ValleyOak_spatialintersect,by="POLYGON_ID")
ValleyOak$veg <- "ValleyOak"

# Merge all the Sentinel data for all vegetation 

AllData <- rbind(Cottonwood,Willow,ValleyOak)
# Create month and seasonal variables
AllData$month<- as.numeric(month(AllData$date))
AllData$SEASON<- 1
AllData$SEASON[which(AllData$month==2)]<- 1
AllData$SEASON[which(AllData$month==3)]<- 1
AllData$SEASON[which(AllData$month==4)]<- 2
AllData$SEASON[which(AllData$month==5)]<- 2
AllData$SEASON[which(AllData$month==6)]<- 2
AllData$SEASON[which(AllData$month==7)]<- 3
AllData$SEASON[which(AllData$month==8)]<- 3
AllData$SEASON[which(AllData$month==9)]<- 3
AllData$SEASON[which(AllData$month==10)]<- 4
AllData$SEASON[which(AllData$month==11)]<- 4
AllData$SEASON[which(AllData$month==12)]<- 4

#Create a data frame with monthly averaged NDVI for each polygon id for all cottonwood, willow, and valley oak.  This will be used in the river analysis.
AllData_Monthly <- ddply(AllData, .(POLYGON_ID,month), summarize, monthlymean = mean(NDVI))

#Parse out all NDVI data from growing season
gsNDVIdata<- AllData[which(AllData$SEASON==3 | AllData$SEASON==2),] #Select growing season data
gsNDVIdata$year <- as.numeric(year(gsNDVIdata$date))


########## ALTERED FLOW ############
#Classify whether stream reaches are natural or altered using the National Hydrography Dataset
streamNDVI<- ddply(gsNDVIdata, .(POLYGON_ID,year,SEASON, veg), summarize, NDVI_seasonal=mean(NDVI))
streamNDVI<- left_join(streamNDVI,stream, by="POLYGON_ID")
streamNDVI$NHD[which(streamNDVI$FTYPE=="StreamRiver")]<-"Natural"
streamNDVI$NHD[which(streamNDVI$FTYPE=="Coastline")]<-"Natural"
streamNDVI$NHD[which(streamNDVI$FTYPE=="Pipeline")]<-"Altered"
streamNDVI$NHD[which(streamNDVI$FTYPE=="CanalDitch")]<-"Altered"
streamNDVI$NHD[which(streamNDVI$FTYPE=="Connector")]<-"Altered"
streamNDVI$NHD[which(streamNDVI$FTYPE=="ArtificialPath")]<-"Altered"
streamNDVI$NHD[which(is.na(streamNDVI$MXStatus))]<-NULL      #"NoData"
streamNDVI$NHD[which(streamNDVI$COMID==0)]<-NULL
streamNDVI$NHD[which(streamNDVI$MXStatus>=4)]<-"Altered"
ddply(streamNDVI,.(NHD),summarize, number=length(unique(POLYGON_ID)))
streamNDVI$NHD<- factor(streamNDVI$NHD,levels=c("Natural","Altered"))
streamNDVI$year<- as.factor(streamNDVI$year)
streamNDVI$SEASON<-as.factor(streamNDVI$SEASON)