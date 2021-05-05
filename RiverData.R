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
options(stringsAsFactors = FALSE)


#####################################
#####################################
###     NDVI Data ###
#####################################
#####################################
monthlyNDVI<- read.csv("~/Data/Modified/Merged/AllData_Monthly.csv", header=TRUE, sep=",")
monthlyNDVI$X<-NULL


#####################################
#####################################
###     River Data ###
#####################################
#####################################


url_all<-"https://flow-api.codefornature.org/v2/stream/?comids=0&variables=observed"
obsRiver_all <- GET(url_all)
obsRiver_all_df <- content(obsRiver_all,as="parsed")
# comid_obs<-data.frame(unique(obsRiver_all_df$comid)) 
#export table of comids with observations, so that I can import and map them in ArcGIS
#write.csv(comid_obs, file ="~/Data/Modified/comids_obs.csv", row.names = TRUE)
comid_obs<- read.csv("~/Data/Modified/River/comids_obs.csv", header=TRUE,sep=",")

          ###### One Time Operation #######  
          #Determine which comids have observed river data in the entire California dataset
          #by extracting all observed data COMIDs from API database and importing into ArcGIS
          #to identify COMIDs in the NHD Flowline Version 2 dataset. 
          # Identify all COMIDs that intersect with vegetation polygons in the final dataset 
          # COMID_veg <- read.csv('~/Data/Modified/River/NCveg_COMID.csv', header=TRUE,sep=",")
          # COMID <- unique(COMID_veg$COMID) 
          # COMID <- sort(COMID) #Sort low to high
          # COMID <- COMID[-1]       #Remove 0 value COMID, since it corresponds to all the veg polygons that did not intersect a COMID river reach
          # COMIDs <-paste(COMID, collapse=",") #Create a string of the COMIDs of veg intersects to insert into url for API
          
          
          #Load csv with NHD_flowline spatial data from ArcGIS that have been identified as having a COMID with observed data.
          #Determine which vegetation polygons exist within river systems with observed data
          # NHDflowline_obs <- read.delim('~/Data/Modified/River/NHDflowline_obs.txt', header=TRUE, sep =",")
          # NHDflowline_obs$COMID <- as.numeric(NHDflowline_obs$COMID)
          
          # Join the Vegetation Data (COMID_veg) with the River info (COMIDs with obs data) 
          # comid_obs_river<- left_join(COMID_veg, NHDflowline_obs, by = "COMID")
          # comid_obs_river<- na.omit(comid_obs_river) #This results in only 3,433 veg polygons along COMIDs with obs data
          # comid_obs_river$COMID <- as.character(comid_obs_river$COMID)
          # #comid_obs_SacRiver <- comid_obs_river[which(comid_obs_river$GNIS_NAME.x=="Sacramento River"),]
           
          # Load NHD Flowline COMID data that are spatially joined to HydroRegion
          # NHDflowline_HR<- read.csv('~/Data/Modified/River/NHDflowline_HR_spatialjoin.csv', header=TRUE, sep =",")
          # NHDflowline_withinHR <- NHDflowline_HR[which(NHDflowline_HR$HR_NAME!=" "),] #df with comids existing within HRs.
          # COMID_HR<-data.frame(NHDflowline_withinHR$COMID, NHDflowline_withinHR$HR_NAME)
          # colnames(COMID_HR) <- c('COMID','HR_NAME')
           
          # Load NHD Flowline COMID data that are within groundwater basins
          # NHDFlowline_gwBasinName <- read.delim('~/Data/Modified/River/NHDFlowline_gwBasinName.txt', header=TRUE, sep=",")
          # COMID_gwBasin <- data.frame(NHDFlowline_gwBasinName$COMID, NHDFlowline_gwBasinName$Basin_Su_1)
          # colnames(COMID_gwBasin) <- c("COMID","basin")
          
          # Combine NHD Flowline COMID data to associate the HydroRegion with those within groundwater basins
          # COMID_basinHR <- left_join(COMID_gwBasin,COMID_HR, by="COMID")
          # colnames(COMID_basinHR) <- c("COMID","basin","HR_NAME")
          # COMID_basinHR$COMID <- as.character(COMID_basinHR$COMID)
          
          
          # #Determine which Rivers exist in groundwater basins and have observed data
          #COMID_basinHR<- left_join(comid_obs_river,COMID_basinHR, by="COMID")
          #COMID_basinHR <- na.omit(COMID_gwBasin)
          # COMID_basinHR_RiverName <- data.frame(COMID_gwBasin$COMID, COMID_gwBasin$basin, COMID_gwBasin$GNIS_NAME.x)
          # colnames(COMID_gwBasin_RiverName)<- c("COMID","basin","GNIS_NAME")
          # COMID_gwBasin_RiverName <- left_join(COMID_gwBasin_RiverName,AllVeg, by="COMID")
          # COMID_gwBasin_RiverName <- na.omit(COMID_gwBasin_RiverName)
          # unique(COMID_gwBasin_RiverName$GNIS_NAME.x)
          # 
          # Identify all COMIDs that intersect with vegetation polygons in the final dataset 
          # COMID_veg <- read.csv('~/Data/Modified/River/NCveg_COMID.csv', header=TRUE,sep=",")
          # COMID <- unique(COMID_veg$COMID) 
          # COMID <- sort(COMID) #Sort low to high
          # COMID <- COMID[-1]       #Remove 0 value COMID, since it corresponds to all the veg polygons that did not intersect a COMID river reach
          # COMIDs <-paste(COMID, collapse=",") #Create a string of the COMIDs of veg intersects to insert into url for API

# Analysis of Alteration in Flow Data (Zimmerman et al. 2017 approach)
# Determine which COMIDs have at least five full water years (Oct-Sept) of data within the
# time period of 1996 - 2016.  The 5 years of data don't have to be concurrant, but all 12 months 
# within a given year need to be concurrent.
obsRiver_all_df$year <- as.numeric(obsRiver_all_df$year)
obsRiver_select <- obsRiver_all_df[which(obsRiver_all_df$year >= 1996),]

#For every comid with observed data between 1996 and 2016
comid_select <- as.numeric(unique(obsRiver_select$comid)) #562 comids with observed data between 1996 and 2016.

#extract all modelled and observed data for comids with observed data to compare.
#For every comid with observed data between 1996-2016, determine which have 5 years of monthly data. There are 562 comids with observed data between 1996 and 2016.
#Due to the url limit of 2048 characters, break up the list of comids by thirds to extract data, then combine them in R as data.frames.
comid_select_url_1 <- paste(comid_select[1:187], collapse=",")
url_select_1 <- paste("https://flow-api.codefornature.org/v2/stream/?comids=",comid_select_url_1,sep="")
River_select_1 <- GET(url_select_1)
River_select_df_1 <- content(River_select_1,as="parsed")

comid_select_url_2 <- paste(comid_select[188:375], collapse=",")
url_select_2 <- paste("https://flow-api.codefornature.org/v2/stream/?comids=",comid_select_url_2,sep="")
River_select_2 <- GET(url_select_2)
River_select_df_2 <- content(River_select_2,as="parsed")

comid_select_url_3 <- paste(comid_select[376:562], collapse=",")
url_select_3 <- paste("https://flow-api.codefornature.org/v2/stream/?comids=",comid_select_url_3,sep="")
River_select_3 <- GET(url_select_3)
River_select_df_3 <- content(River_select_3,as="parsed")

#combine all River data into one data.frame.
River_select_df <- rbind (River_select_df_1, River_select_df_2, River_select_df_3)
names(River_select_df)[1] <- "COMID"
River_select_df$COMID<-as.character(River_select_df$COMID)

#Extract only data between 1996 and 2015
River_select_df$year <- as.numeric(River_select_df$year)
River_select_df <- River_select_df[which(River_select_df$year >= 1996),]

#load veg data (cottonwood, valley oak, willow) within 1km of a COMID with obs data
COMID_NCveg3<- read.delim("~/Data/Modified/River/NCveg3_ObsRiver.txt", header=TRUE,sep=",")
NCveg3_obsCOMID <- data.frame(COMID_NCveg3$POLYGON_ID,COMID_NCveg3$VEGETATION,COMID_NCveg3$comids_obs)
colnames(NCveg3_obsCOMID)<- c("POLYGON_ID","veg","COMID")
NCveg3_obsCOMID$COMID<- as.character(NCveg3_obsCOMID$COMID)

#Parse out river obs data (1996-2016) that are within 1km of a veg polygon (cottonwood, willow, valley oak) 
uniq_vegObs<-unique(NCveg3_obsCOMID$COMID)
River_Vegdata<-data.frame()
for (i in  1:length(uniq_vegObs)){
  result<- River_select_df[which(River_select_df$COMID==uniq_vegObs[i]),]
  River_Vegdata <-rbind(result,River_Vegdata)
}


####### Create flow alteration stats for COMIDs along NC veg

n<-unique(River_Vegdata$COMID) #93

river<- data.frame()
for (i in 1:length(n)){
  data <- River_Vegdata[which(River_Vegdata$COMID == n[i]),]
  data <- data[which(data$statistic=="median"),] #Changed from mean
  observed <- data[which(data$variable=="observed"),]
  estimated <- data[which(data$variable=="estimated"),]
  data_join <- left_join(observed, estimated, by=c("year", "month"))
  data_join$statistic.x <- NULL
  data_join$statistic.y <- NULL
  data_join$variable.x <- NULL
  data_join$variable.y <- NULL
  data_join$COMID.y <- NULL
  colnames(data_join)<-c("COMID","year","month","Qobs","Qest")
  river <- rbind(river,data_join)
}

  
  #Merge river obs data (1996-2016) that are within 1km of veg polygon (c, w, vo)
  combo <- left_join(NCveg3_obsCOMID,river, by = "COMID")
  combo <- na.omit(combo)
  length(unique(combo$POLYGON_ID)) #1891 veg polygons with obs data (1996-2016)  
  
  #Attribute HR to combo data
  # #Load NHD Flowline COMID data that are spatially joined to HydroRegion
  NHDflowline_HR<- read.csv('~/Data/Modified/NHDflowline_HR_spatialjoin.csv', header=TRUE, sep =",")
  NHDflowline_withinHR <- NHDflowline_HR[which(NHDflowline_HR$HR_NAME!=" "),] #df with comids existing within HRs.
  COMID_HR<-data.frame(NHDflowline_withinHR$COMID, NHDflowline_withinHR$HR_NAME)
  colnames(COMID_HR) <- c('COMID','HR_NAME')
  COMID_HR$COMID <- as.character(COMID_HR$COMID)
  combo<-left_join(combo,COMID_HR,by="COMID")
  
  
  combo$month<- as.character(combo$month)
  combo$month<-factor(combo$month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  
  #export River dataset for veg polygons 
  #write.csv(combo, file ="~/Data/Modified/River/combo_median_riverobs_veg.csv", row.names = TRUE)
  