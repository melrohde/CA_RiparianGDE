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
library(segmented)
library(rkt)
library(lme4) 
library(lmerTest)
library(emmeans)
library(gmodels)
library(DescTools)
library(qqplotr)
library(ggpubr)
library(rstatix)
library(MuMIn)
options(stringsAsFactors = FALSE)


#Load Polygon names with flow and groundwater data
Veg_match<- read.csv("~/Data/Modified/Veg/Veg_match.csv",header=TRUE,sep=",")

##################################################################
###           Area Statistics by stream designation             ##
##################################################################

StreamArea<-data.frame(streamNDVI$POLYGON_ID,streamNDVI$NHD)
colnames(StreamArea)<- c("POLYGON_ID","NHD")
StreamArea<-StreamArea[!duplicated(StreamArea),] 
Cottonwood_area$veg<-"Cottonwood"
Willow_area$veg <- "Willow"
ValleyOak_area$veg <- "ValleyOak"
veg_area<-rbind(Cottonwood_area,Willow_area,ValleyOak_area)
StreamArea<-left_join(StreamArea,veg_area,by="POLYGON_ID")
ddply(StreamArea, .(NHD),summarize,sum_area=sum(Area_sqm))
ddply(StreamArea, .(veg),summarize,sum_area=sum(Area_sqm))

summary(StreamArea$NHD)


##################################################################
###           SUMMARY STATISTICS FOR ALL NDVI DATA              ##
###      Quantiles, Sample Sizes, and Interquartile Ranges      ##
##################################################################

#Load All NDVI data from "NCdataset_NDVITimeseries.R"
AllData <- read.csv("~/Data/Modified/Merged/AllData.csv", header=TRUE,sep=",")

#Parse out all NDVI data from growing season
gsNDVIdata<- AllData[which(AllData$SEASON==3 | AllData$SEASON==2),] #Select growing season data
gsNDVIdata$year <- as.numeric(year(gsNDVIdata$date))

#Cottonwood#
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Sacramento River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Joaquin River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Tulare Lake")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Central Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Colorado River")])

length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Sacramento River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Joaquin River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Tulare Lake")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Central Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Colorado River")])

IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Sacramento River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Joaquin River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Tulare Lake")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Central Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Colorado River")])

length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "North Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Sacramento River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Francisco Bay")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "San Joaquin River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Tulare Lake")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Central Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "South Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Cottonwood" & gsNDVIdata$HR_NAME== "Colorado River")]))

#Willow#
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Sacramento River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Joaquin River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Tulare Lake")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Central Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Colorado River")])

length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Sacramento River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Joaquin River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Tulare Lake")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Central Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Colorado River")])

IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Sacramento River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Joaquin River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Tulare Lake")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Central Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Colorado River")])

length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "North Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Sacramento River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Francisco Bay")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "San Joaquin River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Tulare Lake")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Central Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "South Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="Willow" & gsNDVIdata$HR_NAME== "Colorado River")]))


#Valley Oak#
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Sacramento River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Joaquin River")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Tulare Lake")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Central Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Coast")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Lahontan")])
quantile(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Colorado River")])

length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Sacramento River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Joaquin River")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Tulare Lake")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Central Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Coast")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Lahontan")])
length(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Colorado River")])

IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Sacramento River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Joaquin River")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Tulare Lake")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Francisco Bay")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Central Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Coast")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Lahontan")])
IQR(gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Colorado River")])

length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "North Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Sacramento River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Francisco Bay")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "San Joaquin River")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Tulare Lake")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Central Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Coast")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "South Lahontan")]))
length(unique(gsNDVIdata$POLYGON_ID[which(gsNDVIdata$veg=="ValleyOak" & gsNDVIdata$HR_NAME== "Colorado River")]))




##################################################################
###                       MIXED MODELS                          ##
##################################################################


####################
##    Figure 2    ##
####################
AllVeg_SeasonalMedian<- read.csv("~/Data/Modified/Merged/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
AllVeg_SeasonalMedian$veg<-as.factor(AllVeg_SeasonalMedian$veg)

veg_model<- function(df){
  lmer(NDVI_LinApprox~DTW + veg + DTW:veg + (1|POLYGON_ID), data=df)
}

veg_output<-veg_model(AllVeg_SeasonalMedian)
summary(veg_output)
r.squaredGLMM(veg_output) #outputs are marginal R squared (R2m) and conditional R squared (R2c)

emmeans(veg_output, list (pairwise~veg), adjust="tukey") # For a pairwise comparison of the intercepts
pairs(emtrends(veg_output,"veg",var="DTW"))
#difflsmeans(veg_output, test.effs = "veg")
plot(veg_output, main="All Veg")


################################
##   Figure S2 (Figure 2 by HR)
################################

AllVeg_SeasonalMedian<- read.csv("~/Data/Modified/Merged/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
AllVeg_SeasonalMedian$veg<-as.factor(AllVeg_SeasonalMedian$veg)
AllVeg_SeasonalMedian$HR<-as.factor(AllVeg_SeasonalMedian$HR)

df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="North Coast" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="North Lahontan" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River" & AllVeg_SeasonalMedian$veg=="Willow"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River" & AllVeg_SeasonalMedian$veg=="Valley Oak"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Francisco Bay" & AllVeg_SeasonalMedian$veg=="Valley Oak"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River" & AllVeg_SeasonalMedian$veg=="Willow"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River" & AllVeg_SeasonalMedian$veg=="Valley Oak"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Tulare Lake" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Central Coast" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Coast" & AllVeg_SeasonalMedian$veg=="Willow"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Coast" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Lahontan" & AllVeg_SeasonalMedian$veg=="Cottonwood"),]



general_model<- function(df){
  lmer(NDVI_LinApprox~DTW + (1|POLYGON_ID), data=df)
}

general_output<-general_model(df)
summary(general_output)
r.squaredGLMM(general_output) #outputs are marginal R squared (R2m) and conditional R squared (R2c)




####################
##    Figure 3    ##
####################


##### Figure 3b - pairwise wilcox (nonparametric test) - BoxPlot of NDVI and Season for all veg#####

## Load Paired Seasonal Flow Alteration and NDVI Data
SeasonalCombo_NDVI_FlowAlt<- read.csv("~/Data/Modified/Merged/SeasonalCombo_NDVI_FlowAlt.csv", header = TRUE)
SeasonalCombo_NDVI_FlowAlt[which(is.na(SeasonalCombo_NDVI_FlowAlt)),]

#Combine season and flow alteration into a single variable
SeasonalCombo_NDVI_FlowAlt$group<- paste(SeasonalCombo_NDVI_FlowAlt$season,"-",SeasonalCombo_NDVI_FlowAlt$FlowAlt_ratio, sep="")

#Reorder group levels
SeasonalCombo_NDVI_FlowAlt<- SeasonalCombo_NDVI_FlowAlt %>% 
    reorder_levels(group, order=c("Spring-inflated","Spring-depleted",
                                  "Summer-inflated","Summer-depleted"))
# Summary Statistics
SeasonalCombo_NDVI_FlowAlt %>% group_by(group) %>%
  get_summary_stats(seasonalNDVI, type="common")
#Pairwise Comparisons of median
pairwise.wilcox.test(SeasonalCombo_NDVI_FlowAlt$seasonalNDVI, SeasonalCombo_NDVI_FlowAlt$group,
                     p.adjust.method = "BH")

kruskal.test(seasonalNDVI ~ group, data = SeasonalCombo_NDVI_FlowAlt)


#####  Figure 3c - NDVI v. DTW plot of Natural and Altered Streams #####

## Load Paired Data ##
AllVeg_SeasonalMedian<- read.csv("~/Data/Modified/Merged/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')

Global_model2<- function(df) {
  lmer(NDVI_LinApprox~NHD * DTW + (1|POLYGON_ID), data=df)
}
AllVeg_SeasonalMedian$NHD<-as.factor(AllVeg_SeasonalMedian$NHD)
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]
global_NHD<- Global_model2(df)
summary(global_NHD)
r.squaredGLMM(global_NHD) #outputs are marginal R squared (R2m) and conditional R squared (R2c)
plot(global_NHD, main="NHD")


################################
##   Figure S3 (Figure 3 by HR)
################################

AllVeg_SeasonalMedian<- read.csv("~/Data/Modified/Merged/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
AllVeg_SeasonalMedian$NHD<-as.factor(AllVeg_SeasonalMedian$NHD)
AllVeg_SeasonalMedian$HR<-as.factor(AllVeg_SeasonalMedian$HR)


df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="North Coast" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="North Lahontan" & is.na(AllVeg_SeasonalMedian$NHD)),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Francisco Bay" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Francisco Bay" & AllVeg_SeasonalMedian$NHD=="Altered"),]

df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Tulare Lake" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Tulare Lake" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Central Coast" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Central Coast" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Coast" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Lahontan" & AllVeg_SeasonalMedian$NHD=="Natural"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Lahontan" & AllVeg_SeasonalMedian$NHD=="Altered"),]
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Lahontan" & is.na(AllVeg_SeasonalMedian$NHD)),]


general_model<- function(df){
  lmer(NDVI_LinApprox~DTW + (1|POLYGON_ID), data=df)
}

general_output<-general_model(df)
summary(general_output)
r.squaredGLMM(general_output) #outputs are marginal R squared (R2m) and conditional R squared (R2c)

ols_model<-function(df){
  lm(NDVI_LinApprox~DTW, data=df)
}
ols_output<-ols_model(df)
summary(ols_output)


##### SI Figures - NDVI vs DTW by Hydrologic Region & Season #####
#Compare the seasonal slopes for all vegetation within each hydrologic region
#model specifies polygon id as a random effect -
#this corrects for nonindependence that may occur since 
#some polygon ids have more data than (eg.1 point total vs. 10 (1 each season for all 5 years))
Global_model<- function(df) {
  lmer(NDVI_LinApprox~DTW + SEASON + DTW:SEASON + (1|POLYGON_ID), data=df)
}


AllVeg_SeasonalMedian$SEASON<-as.factor(AllVeg_SeasonalMedian$SEASON)

# Sacramento River -All Veg #
global_SR<- Global_model(AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="Sacramento River"),])
summary(global_SR)
plot(global_SR, main="Sacramento River")

# San Joaquin River -All Veg #
global_SJR<- Global_model(AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="San Joaquin River"),])
summary(global_SJR)
plot(global_SJR, main="San Joaquin River")

# South Coast -All Veg #
global_SC<- Global_model(AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Coast"),])
summary(global_SC)
plot(global_SC, main="South Coast")

# South Lahontan -All Veg #
global_SL<- Global_model(AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$HR=="South Lahontan"),])
summary(global_SL)
plot(global_SL, main="South Lahontan")
