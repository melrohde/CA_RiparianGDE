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

##  Load GEE-GW Work Environment ##
load("~/Dropbox/SUNY/Scripts/WorkEnvironments/NCdataset_NDVItimeseries_2015_2020.RData") # All NDVI data
## Load Paired Data ##
AllVeg_SeasonalMedian<- read.csv("~/Dropbox/SUNY/Data/Modified/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
climate <- read.csv("~/Dropbox/SUNY/Data/Modified/NCdataset_climate.csv", header=TRUE, sep=",")
## Load Combined River Data
SeasonalCombo_NDVI_FlowAlt<- read.csv("~/Dropbox/SUNY/Scripts/GEE-GW Sentinel Research/Final/Revised/Data/SeasonalCombo_NDVI_FlowAlt.csv", header = TRUE)

#Load Vegetation PolygonIDs with paired groundwater, NDVI, and River Data
Veg_match<-read.csv("~/Dropbox/SUNY/Scripts/GEE-GW Sentinel Research/Final/Revised/Data/Veg_match.csv", header=TRUE,sep=",")
Veg_match[1]<-NULL

dat<- select(SeasonalCombo_NDVI_FlowAlt,c(2,3,9))
Veg_match<-left_join(Veg_match,dat, by="POLYGON_ID")
Veg_match$SEASON[which(Veg_match$season=="Summer")]<-3
Veg_match$SEASON[which(Veg_match$season=="Spring")]<-2

#write.csv(Veg_match,file="~/Dropbox/SUNY/Data/Modified/Veg_match.csv",row.names = TRUE)

##################################################################
###                    MAIN PAPER                              ##
##################################################################


#########  Figure 1b. NDVI TIME SERIES   ##########

Cottonwood_med$HR_NAME<-factor(Cottonwood_med$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                               "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                               "South Lahontan","Colorado River"))
Willow_med$HR_NAME<-factor(Willow_med$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                       "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                       "South Lahontan","Colorado River"))

ValleyOak_med$HR_NAME<-factor(ValleyOak_med$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                             "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                             "South Lahontan","Colorado River"))


p2<-ggplot(data=Cottonwood_med, aes(x=date,y=NDVI, color=HR_NAME)) + theme_bw()+
  geom_rect(xmin =as.Date("2016-04-01"), xmax = as.Date("2016-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2017-04-01"), xmax = as.Date("2017-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2018-04-01"), xmax = as.Date("2018-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2019-04-01"), xmax = as.Date("2019-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2020-04-01"), xmax = as.Date("2020-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_ma(ma_fun=SMA, n=15, linetype="solid") + 
  ylim(0.15,0.65) + xlab("") + scale_x_date(limits=as.Date(c("2015-10-01","2021-01-01")),
         breaks= as.Date(c("2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")),
         labels=c("2016","2017","2018","2019","2020","2021")) +
  scale_colour_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44")) +
  theme( legend.position="none", legend.title =element_blank(), legend.text=element_text(size=16),
         axis.title.y = element_text(size=18, face="bold"), strip.text.x= element_text(size=18, face="bold"), axis.text=element_text(size=18)) 



  
p1<-  ggplot(data=Willow_med, aes(x=date,y=NDVI, color=HR_NAME)) + theme_bw()+
  geom_rect(xmin =as.Date("2016-04-01"), xmax = as.Date("2016-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2017-04-01"), xmax = as.Date("2017-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2018-04-01"), xmax = as.Date("2018-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2019-04-01"), xmax = as.Date("2019-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2020-04-01"), xmax = as.Date("2020-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_ma(ma_fun=SMA, n=15, linetype="solid") + 
  ylim(0.15,0.65)  + xlab("") + scale_x_date(limits=as.Date(c("2015-10-01","2021-01-01")), 
        breaks= as.Date(c("2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")),
        labels=c("2016","2017","2018","2019","2020","2021")) +
  scale_colour_manual(values=c("#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#9E1C44")) +
  theme(legend.position="none", legend.title =element_blank(), legend.text=element_text(size=16),
        axis.title.y = element_text(size=18, face="bold"), strip.text.x= element_text(size=18, face="bold"), axis.text=element_text(size=18))



p3<- ggplot(data=ValleyOak_med, aes(x=date,y=NDVI, color=HR_NAME)) + theme_bw()+
  geom_rect(xmin =as.Date("2016-04-01"), xmax = as.Date("2016-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2017-04-01"), xmax = as.Date("2017-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2018-04-01"), xmax = as.Date("2018-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2019-04-01"), xmax = as.Date("2019-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_rect(xmin =as.Date("2020-04-01"), xmax = as.Date("2020-09-30"), ymin = -Inf, ymax = Inf, color=NA, fill = "gray80") +
  geom_ma(ma_fun=SMA, n=15, linetype="solid") + 
  theme(legend.position="none", legend.title =element_blank(), legend.text=element_text(size=16),
        axis.title.y = element_text(size=18, face="bold"), strip.text.x= element_text(size=18, face="bold"), axis.text=element_text(size=18)) + 
  ylim(0.15,0.65) + xlab("") + scale_x_date(limits=as.Date(c("2015-10-01","2021-01-01")),
        breaks= as.Date(c("2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")),
        labels=c("2016","2017","2018","2019","2020","2021")) +
  scale_colour_manual(values=c("#5E50A1","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F"))


grid.arrange(p1,p2,p3,nrow=3)



#########  Figure 1c - NDVI BOX PLOTS   ##########



#Cottonwood#
CottonwoodBox<-data.frame(gsNDVIdata$HR_NAME[which(gsNDVIdata$veg=="Cottonwood")],gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Cottonwood")])
colnames(CottonwoodBox)<- c("HR_NAME","NDVI")  

#vertical Box Plot
CottonwoodBox$HR_NAME<-factor(CottonwoodBox$HR_NAME,levels=c("Colorado River","South Lahontan",
                                                             "South Coast", "Central Coast","Tulare Lake","San Joaquin River","San Francisco Bay",
                                                             "Sacramento River","North Lahontan","North Coast"))

ggplot(data=CottonwoodBox, aes(x=HR_NAME, y=NDVI, fill=HR_NAME)) + geom_boxplot() +
  theme_bw() +ylab("NDVI") + xlab(" ") +ylim(0,1.05) +
  theme(axis.title.x = element_text(size=18, face="bold"), strip.text.y= element_text(size=20, face="bold"), axis.text=element_text(size=22),
        legend.position="none") + coord_flip() +
  scale_x_discrete(labels=c("CR","SL","SC","CC","TL","SJR","SFB","SR","NL","NC")) +
  scale_fill_manual(values=c("#9E1C44","#D53E4F","#F36D44","#FAAD61","#FDDF8B","#E5EB9A","#AED8A4","#68C3A5","#3288BD","#5E50A1"))


#Willow#
WillowBox<-data.frame(gsNDVIdata$HR_NAME[which(gsNDVIdata$veg=="Willow")],gsNDVIdata$NDVI[which(gsNDVIdata$veg=="Willow")])
colnames(WillowBox)<- c("HR_NAME","NDVI") 
WillowDummy<-data.frame(c("North Lahontan","North Coast","Central Coast","South Lahontan"),c(-1,-1,-1,-1))#Add dummy data for missing HRs
colnames(WillowDummy)<- c("HR_NAME","NDVI") 
WillowBox<-rbind(WillowBox,WillowDummy)
#vertical Box Plot
WillowBox$HR_NAME<-factor(WillowBox$HR_NAME,levels=c("Colorado River","South Lahontan",
                                                     "South Coast", "Central Coast","Tulare Lake","San Joaquin River","San Francisco Bay",
                                                     "Sacramento River","North Lahontan","North Coast"))

p2<-ggplot(data=WillowBox, aes(x=HR_NAME, y=NDVI, fill=HR_NAME)) + geom_boxplot() +
  theme_bw() +ylab("NDVI") + xlab(" ") +ylim(0,1.05) +
  theme(axis.title.x = element_text(size=18, face="bold"), strip.text.x= element_text(size=20, face="bold"), axis.text=element_text(size=22),
        legend.position="none") +  coord_flip() +
  scale_x_discrete(labels=c("CR","SL","SC","CC","TL","SJR","SFB","SR","NL","NC")) +
  scale_fill_manual(values=c("#9E1C44","#F36D44","#FDDF8B","#E5EB9A","#AED8A4","#68C3A5"))


#ValleyOak#
ValleyOakBox<-data.frame(gsNDVIdata$HR_NAME[which(gsNDVIdata$veg=="ValleyOak")],gsNDVIdata$NDVI[which(gsNDVIdata$veg=="ValleyOak")])
colnames(ValleyOakBox)<- c("HR_NAME","NDVI") 
ValleyOakDummy<-data.frame(c("North Lahontan","Colorado River"),c(-1,-1))#Add dummy data for missing HRs
colnames(ValleyOakDummy)<- c("HR_NAME","NDVI") 
ValleyOakBox<-rbind(ValleyOakBox,ValleyOakDummy)
ValleyOakBox$HR_NAME<-factor(ValleyOakBox$HR_NAME,levels=c("Colorado River","South Lahontan",
                                                           "South Coast", "Central Coast","Tulare Lake","San Joaquin River","San Francisco Bay",
                                                           "Sacramento River","North Lahontan","North Coast"))

p3<-ggplot(data=ValleyOakBox, aes(x=HR_NAME, y=NDVI, fill=HR_NAME)) + geom_boxplot() +
  theme_bw() +ylab("NDVI") + xlab(" ") +ylim(0,1.05) +
  theme(axis.title.x = element_text(size=18, face="bold"), strip.text.x= element_text(size=20, face="bold"), axis.text=element_text(size=22),
        legend.position="none") + coord_flip() +
  scale_x_discrete(labels=c("CR","SL","SC","CC","TL","SJR","SFB","SR","NL","NC")) +
  scale_fill_manual(values=c("#D53E4F","#F36D44","#FAAD61","#FDDF8B","#E5EB9A","#AED8A4","#68C3A5","#5E50A1"))

Fig1c<-grid.arrange(p1,p2,p3,nrow=3)
aspect_ratio <- 0.5
ggsave("~/Dropbox/SUNY/Chapters/Chapter 2/Figures/Fig1c.png",plot=Fig1c, 
       height = 10 , width = 10 * aspect_ratio, dpi=300)

#########  Figure 2 - NDVI vs. DTW all data   ##########

#Cottonwood
AllVeg_SeasonalMedian<- read.csv("~/Dropbox/SUNY/Data/Modified/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
df<- AllVeg_SeasonalMedian
df$HR<-factor(df$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                             "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                             "South Lahontan","Colorado River"))
df$veg<-factor(df$veg,levels=c("Cottonwood","Willow","Valley Oak"))

p2<-ggplot(data=df[which(df$veg=="Cottonwood"),], aes(x=DTW, y=NDVI_LinApprox,)) + 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  geom_segment(aes(x=0, y=0.49, xend=25, yend=(-0.0089*25)+0.49)) + 
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,25)+ 
  annotate("text",x=20,y=0.95, label = "y = 0.49 - 0.009x", size=5) +
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=5)+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("(b) Cottonwood") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = -0.25, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 


#Willow
p1<-ggplot(data=df[which(df$veg=="Willow"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  geom_point(aes(fill=HR),colour="black",shape=24) +
  geom_segment(aes(x=0, y=0.54, xend=20, yend=(-0.0048*20)+0.54)) + 
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,25)+ 
  annotate("text",x=20,y=0.95, label = "y = 0.54 - 0.005x", size=5) +
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=5)+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("(a) Willow") +
  scale_fill_manual(values=c("#68C3A5","#E5EB9A","#F36D44")) +
  theme(plot.title = element_text(hjust = -0.20, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

#Valley Oak
p3<-ggplot(data=df[which(df$veg=="Valley Oak"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  geom_point(aes(fill=HR),colour="black",shape=22) +
  geom_segment(aes(x=0, y=0.65, xend=25, yend=(-0.0126*25)+0.65)) +
  annotate("text",x=20,y=0.95, label = "y = 0.65 - 0.013x", size=5) + 
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=5)+
  ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,25)+ 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("(c) Oak") +
  scale_fill_manual(values=c("#68C3A5","#AED8A4","#E5EB9A")) +
  theme(plot.title = element_text(hjust = -0.19, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

grid.arrange(p1,p2,p3, nrow=3)


#########   Figure 3 - NDVI v DTW by Stream Flow Alteration   ##########

#########   Figure 3c  ##########

SeasonalCombo_NDVI_FlowAlt<- na.omit(SeasonalCombo_NDVI_FlowAlt)
SeasonalCombo_NDVI_FlowAlt$FlowAlt_ratio<-factor(SeasonalCombo_NDVI_FlowAlt$FlowAlt_ratio, levels=c("inflated","depleted"))

fun_length <- function(x){
  return(data.frame(y=median(x),label= paste("n=",length(x),sep="")))
}
ggplot(data=SeasonalCombo_NDVI_FlowAlt,aes(x=season,y=seasonalNDVI, fill=FlowAlt_ratio)) + 
  geom_violin(draw_quantiles = 0.5, size=1) +
  geom_violin(draw_quantiles = c(0.25,0.75),linetype="dashed", alpha=0.1) + 
  scale_fill_manual(values=c("#816687","#D3BECF")) + theme_bw() +
  xlab(" ") + theme(axis.title.x = element_text(size=28, face="bold"), axis.title.y = element_text(size=28, face="bold"),
                    strip.text.y= element_text(size=28, face="bold"), axis.text=element_text(size=28),
                    legend.position="none") +ylab("NDVI") + xlab(" ") + ylim(0,1)+
                    stat_summary(aes(x=factor(season), fill = factor(FlowAlt_ratio)), position=position_dodge(0.9),
                    fun.data = fun_length, geom = "text", vjust = -8.5, size = 6)


# 
# #Figure 3b but by HR----> THIS WILL LIKELY BECOME A SI PLOT
# 
# fun_length <- function(x){
#   return(data.frame(y=median(x),label= paste0(length(x))))
# }
# #Add dummy variables into each HR
# HR_NAME<- c("Colorado River","Colorado River","Colorado River","Colorado River","North Coast","North Coast","North Coast","North Coast",
#             "San Francisco Bay","South Lahontan","South Lahontan","Tulare Lake","Tulare Lake", "Tulare Lake")
# FlowAlt<- c("inflated","unaltered","inflated","unaltered","inflated","unaltered","inflated","unaltered", 
#             "unaltered","depleted","depleted","depleted","inflated","depleted")
# season<-c("Spring","Spring","Summer","Summer","Spring","Spring","Summer","Summer",
#           "Summer","Spring","Summer","Spring","Summer","Summer")
# dummy<-data.frame(season,HR_NAME,FlowAlt)
# dummy$seasonalNDVI<-0
# 
# select<- data.frame(SeasonalCombo_NDVI_FlowAlt$season,SeasonalCombo_NDVI_FlowAlt$HR_NAME,SeasonalCombo_NDVI_FlowAlt$FlowAlt,SeasonalCombo_NDVI_FlowAlt$seasonalNDVI)
# colnames(select)<-c("season","HR_NAME","FlowAlt","seasonalNDVI")
# SeasonalCombo_NDVI_FlowAlt_withDummy<-rbind(select,dummy)
# 
# # 
# # ggplot(data=SeasonalCombo_NDVI_FlowAlt,aes(x=season,y=seasonalNDVI, fill=FlowAlt, drop=FALSE)) + geom_boxplot() + scale_fill_manual(values=c("#d53e4f","#3288bd","white")) + theme_bw() +
# #   xlab(" ") + theme(axis.title.x = element_text(size=16, face="bold"), axis.title.y = element_text(size=16, face="bold"),
# #                     strip.text.y= element_text(size=16, face="bold"), axis.text=element_text(size=16),
# #                     legend.position="bottom") +ylab("NDVI") + xlab(" ") + ylim(0,1) +facet_wrap(~HR_NAME,nrow=3) +
# #                     stat_summary(aes(x=factor(season), fill = factor(FlowAlt)), position=position_dodge(1),
# #                         fun.data = fun_length, geom = "text", vjust = -3.5, size = 4)
# 
# SeasonalCombo_NDVI_FlowAlt$HR_NAME<-factor(SeasonalCombo_NDVI_FlowAlt$HR_NAME,levels=c("North Coast","Sacramento River","San Francisco Bay",
#                              "San Joaquin River","Tulare Lake","Central Coast","South Coast",
#                              "South Lahontan","Colorado River"))
# ggplot(data=SeasonalCombo_NDVI_FlowAlt,aes(x=season,y=seasonalNDVI, fill=FlowAlt, stat="identity")) + geom_boxplot()  + theme_bw() +
#   xlab(" ") + theme(axis.title.x = element_text(size=16, face="bold"), axis.title.y = element_text(size=16, face="bold"),
#   strip.text.y= element_text(size=16, face="bold"), axis.text=element_text(size=16),
#   legend.position="bottom") +ylab("NDVI") + xlab(" ") + ylim(0,1) +facet_wrap(~HR_NAME,nrow=3) +
#   scale_fill_manual(values=c("#3288bd","white","#d53e4f"))




#########   Figure 3b  ##########

df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]
df<- left_join(df,Veg_match, by="POLYGON_ID")
df$HR<-factor(df$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                             "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                             "South Lahontan","Colorado River"))
df$veg<-factor(df$veg,levels=c("Cottonwood","Willow","Valley Oak"))

p2<- ggplot(data=df[which(df$NHD=="Altered"),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,30)+ 
  geom_segment(aes(x=0, y=0.53, xend=25, yend=(-0.0065*25)+0.53)) +
  annotate("text",x=22,y=0.95, label = "y = 0.53 - 0.007x", size=7) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Altered Regime") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = 0, size=18, face="bold"),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

p1<- ggplot(data=df[which(df$NHD=="Natural" & df$SEASON==3),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  geom_segment(aes(x=0, y=0.63, xend=27, yend=(-0.015*27)+0.63)) +
  ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,30)+ 
  annotate("text",x=22,y=0.95, label = "y = 0.63 - 0.02x", size=7) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face="bold")) + ggtitle("Natural Regime") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = 0, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

grid.arrange(p1,p2,nrow=1)


# 
# #### SI - FIGURE??
# ## NDVI vs. DTG for flow alteration
# df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]
# df<- left_join(Veg_match,df, by=c("POLYGON_ID","SEASON"))
# df<-na.omit(df)
# df$HR<-factor(df$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
#                              "San Joaquin River","Tulare Lake","Central Coast","South Coast",
#                              "South Lahontan","Colorado River"))
# 
# p1<-ggplot(data=df[which(df$FlowAlt=="inflated"),],aes(x=DTW,y=NDVI_LinApprox, shape=veg))+ 
#   geom_point(aes(fill=HR),colour="black",shape=21) + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Inflated") +
#   ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,30) +
#   scale_fill_manual(values=c("#5E50A1","#68C3A5","#AED8A4","#FAAD61","#F36D44"))+
#   theme(plot.title = element_text(hjust = 0, size=18),legend.position="none",
#         axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
#         axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 
# 
# 
# p2<-ggplot(data=df[which(df$FlowAlt=="unaltered"),],aes(x=DTW,y=NDVI_LinApprox, shape=veg))+ 
#   geom_point(aes(fill=HR),colour="black",shape=21) +theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Unaltered") +
#   ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,30) +   
#   scale_fill_manual(values=c("#68C3A5","#AED8A4","#E5EB9A","#F36D44","#D53E4F"))+
#   theme(plot.title = element_text(hjust = 0, size=18),legend.position="none",
#         axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
#         axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 
# 
# 
# p3<-ggplot(data=df[which(df$FlowAlt=="depleted"),],aes(x=DTW,y=NDVI_LinApprox, shape=veg))+ 
#   geom_point(aes(fill=HR),colour="black",shape=21) + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Depleted") +
#   ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,30)+
#   scale_fill_manual(values=c("#FDDF8B","#FAAD61"))+
#   theme(plot.title = element_text(hjust = 0, size=18),legend.position="none",
#         axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
#         axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 
# 




##################################################################
###                    SI APPENDIX                              ##
##################################################################                                                              

##################
##  FIGURE S1 ###
#################

#Plot ETp vs. P and Seasonality vs. P

climate$HR_NAME<-factor(climate$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                 "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                 "South Lahontan","Colorado River"))
climate$year<-as.character(climate$year)
climate$PPET<-climate$P-climate$ETp
climate$PETPratio <- climate$ETp/climate$P
climate$POLYGON_ID<-as.character(climate$POLYGON_ID)
climate$HR_NAME<-factor(climate$HR_NAME,levels=c("Colorado River","South Lahontan",
                                                             "South Coast", "Central Coast","Tulare Lake","San Joaquin River","San Francisco Bay",
                                                             "Sacramento River","North Lahontan","North Coast"))
# Fig S1b #
ggplot(data=climate, aes(x=HR_NAME, y=PPET, fill=HR_NAME)) + geom_boxplot() +
  theme_bw() +ylab("P - PET (mm/yr)") + xlab(" ") + ylim(-2200,1200) +
  theme(axis.title.x = element_text(size=24, face="bold",family="Arial"), strip.text.y= element_text(size=24, face="bold"), axis.text=element_text(size=24),
        legend.position="none") + coord_flip() +
  scale_x_discrete(labels=c("CR","SL","SC","CC","TL","SJR","SFB","SR","NL","NC")) +
  scale_fill_manual(values=c("#9E1C44","#D53E4F","#F36D44","#FAAD61","#FDDF8B","#E5EB9A","#AED8A4","#68C3A5","#3288BD","#5E50A1"))

# Fig S1c #
climate$HR_NAME<-factor(climate$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                 "San Joaquin River","Tulare Lake","Central Coast","South Coast","South Lahontan","Colorado River"))

ggplot(data=climate, aes(x=PPET, y=P, color=HR_NAME)) + geom_point(size=0.25)  + theme_bw() +
  ylab("P (mm/yr)") + xlab("P - PET (mm/yr)") + xlim(-2100,1100) + ylim(0,2000) +
  theme(axis.title.y = element_text(size=24, face="bold"), strip.text.x= element_text(size=24, face="bold"), axis.text=element_text(size=24),
        strip.text.y= element_text(size=24, face="bold"), axis.title.x = element_text(size=24, face="bold"), legend.position="none") +
  scale_colour_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44"))

# Fig S1d #
Data<- left_join(Seasonality,climate, by=c("POLYGON_ID","year"))#Join NDVI seasonality data with climate data
Data$year[which(Data$year=="2015")]<-NA
Data<- na.omit(Data)
Data$ETpPratio<- Data$ETp/Data$P
Data$HR_NAME.y<- NULL
Data$X<-NULL
colnames(Data)<- c("POLYGON_ID","HR_NAME","veg","year","seasonality","P","ETp","PPET","PETPratio","ETpPratio")

ggplot(data=Data, aes(x=PPET, y=seasonality, color=HR_NAME)) + geom_point(size=0.25) + theme_bw() +
  xlab("P - PET (mm/yr)") + ylab("NDVI Seasonality") + ylim(0,1)+ xlim(-2100,1100) + 
  theme(axis.title.y = element_text(size=24, face="bold"), strip.text.x= element_text(size=24, face="bold"), axis.text=element_text(size=24),
        strip.text.y= element_text(size=24, face="bold"), axis.title.x = element_text(size=24, face="bold"), legend.position="nine") +
  scale_colour_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44"))

# ggplot(data=Data, aes(x=PETPratio, y=seasonality, color=HR_NAME)) + geom_point(size=0.25) + theme_bw() +
#   xlab("PET / P ") + ylab("NDVI Seasonality") + # ylim(0,1)+ xlim(-2100,1100) + 
#   theme(axis.title.y = element_text(size=24, face="bold"), strip.text.x= element_text(size=24, face="bold"), axis.text=element_text(size=24),
#         strip.text.y= element_text(size=24, face="bold"), axis.title.x = element_text(size=24, face="bold"), legend.position="nine") +
#   scale_colour_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44"))

# Fig S1e #
##CDF for Shallow Groundwater wells with data between June 23, 2015 and Sept 30, 2020  
## (n=803)

#Load Groundwater Monitoring Well Data
df<- read.csv('~/Dropbox/SUNY/Data/Modified/CASGEM_SelectElev.csv', header=TRUE, sep =",")
df$date <- as.Date(df$date, format = '%Y-%m-%d')
df$SITE_CODE <-as.character(df$SITE_CODE)
df$HR<- as.character(df$HR)
df$X<-NULL
#Convert CASGEM data from US-units to metric
df$WSE_m <- df$WSE*0.3048 #Water Surface Elevation at well
df$GSE_WSE_m <-df$GSE*0.3048 #Depth to Water at well

df$WSE <-NULL
df$GSE_WSE <-NULL
CASGEM<-na.omit(df)
CASGEM <- CASGEM[order(CASGEM$SITE_CODE,CASGEM$date),]


CASGEM_cdf<-CASGEM
CASGEM_cdf[which(CASGEM_cdf$HR==""),]<-NA
CASGEM_cdf<- na.omit(CASGEM_cdf)
CASGEM_cdf$MonthYear <-format(as.Date(CASGEM_cdf$date), "%m-%Y")

CASGEM_cdf_monthlyavg <-ddply(CASGEM_cdf, .(MonthYear,SITE_CODE,HR), summarize, avgDTW=mean(GSE_WSE_m)) 


#determine number of wells within each HR
length(unique(CASGEM_cdf_monthlyavg$SITE_CODE[which(CASGEM_cdf_monthlyavg$HR=="Tulare Lake")]))

CASGEM_cdf_monthlyavg$HR<-factor(CASGEM_cdf_monthlyavg$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                                   "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                                   "South Lahontan","Colorado River"))

ggplot(CASGEM_cdf_monthlyavg, aes(x=avgDTW, color=HR))+ xlim(0,50) + theme_bw()+ ylab("Cumulative Frequency") + 
  xlab("DTG (m)") + stat_ecdf(geom = "point")+ stat_ecdf(size=1) +  
  theme(legend.position="none",
        axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
        axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) +
  scale_colour_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44"))

# FigS1f #

# PPET_HR<-ddply(climate, .(HR_NAME), summarize, avgPPET=mean(PPET))
# PPET_HR$HR_NAME<- as.character(PPET_HR$HR_NAME)
# DTG_HR<-ddply(CASGEM_cdf_monthlyavg, .(HR), summarize, avgDTG=mean(avgDTW))
# colnames(DTG_HR)<-c("HR_NAME","avgDTG")
# HR_combo<-left_join(PPET_HR, DTG_HR, by="HR_NAME")
# 
# 
# PPET_HR_annual <- ddply(climate, .(HR_NAME,year), summarize, annualPPET=mean(PPET))
# colnames(PPET_HR)<-c("HR","year","avgPPET")
# PPET_HR_annual$HR_NAME<-as.character(PPET_HR_annual$HR_NAME)
CASGEM_cdf_monthlyavg$year<-substring(CASGEM_cdf_monthlyavg$MonthYear,4,10)

DTG_HR_annual <- ddply(CASGEM_cdf_monthlyavg, .(HR, year), summarize, annualDTG=mean(avgDTW))
colnames(DTG_HR_annual)<- c("HR_NAME","year","annualDTG")
HR_combo2<-left_join(DTG_HR_annual,PPET_HR_annual,by="HR_NAME")
HR_combo2$year.y<-NULL
colnames(HR_combo2)<- c("HR_NAME","year","annualDTG","annualPPET")

# 
# colnames(PPET_HR_annual)<-c("HR","year","avgPPET")
# HR_combo3 <- left_join(CASGEM_cdf_monthlyavg,PPET_HR_annual, by=c("HR","year"))
# HR_combo3$HR<-as.factor(HR_combo3$HR)

HR_combo2$HR_NAME<-factor(HR_combo2$HR_NAME,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                                   "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                                   "South Lahontan","Colorado River"))
ggplot(data=HR_combo2, aes(x=annualPPET,y=annualDTG, fill=HR_NAME)) + geom_boxplot()+
#+geom_point(size=5)+
  theme_bw()+ ylab("Average DTG (m/yr)") + xlab("Average P - PET (mm/yr)") + ylim(40,0) +
  theme(legend.position="none",
        axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
        axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) +
  scale_fill_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44"))






##################
##  FIGURE S2 ###
#################


AllVeg_SeasonalMedian<- read.csv("~/Dropbox/SUNY/Data/Modified/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
AllVeg_SeasonalMedian$HR<-factor(AllVeg_SeasonalMedian$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                             "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                             "South Lahontan","Colorado River"))
AllVeg_SeasonalMedian$veg<-factor(AllVeg_SeasonalMedian$veg,levels=c("Willow","Cottonwood","Valley Oak"))


anno<- data.frame(x1=c(NA,NA,0,NA,NA,NA,NA,0,NA,NA,NA,0,0,NA,NA,NA,0,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA),x2=c(NA,NA,23,NA,NA,NA,NA,15,NA,NA,NA,23,20,NA,NA,NA,20,NA,NA,NA,NA,NA,25,NA,NA,NA,NA,NA,NA,NA),
y1=c(NA,NA,0.590,NA,NA,NA,NA,0.644,NA,NA,NA,0.303,0.585,NA,NA,NA,0.383,NA,NA,NA,NA,NA,0.637,NA,NA,NA,NA,NA,NA,NA),m=c(NA,NA,-0.00892,NA,NA,NA,NA,-0.0158,NA,NA,NA,0.00973,-0.00715,NA,NA,NA,-0.00700,NA,NA,NA,NA,NA,-0.0103,NA,NA,NA,NA,NA,NA,NA))

anno$y2<- (anno$m*anno$x2)+anno$y1

lab=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast",
"South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast",
"South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast","South Lahontan","Colorado River")
HR=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast",
"South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast",
"South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
"San Joaquin River","Tulare Lake","Central Coast","South Coast","South Lahontan","Colorado River")
veg=c("Willow","Willow","Willow","Willow","Willow","Willow","Willow","Willow","Willow","Willow",
      "Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood","Cottonwood",
      "Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak","Valley Oak")

anno<- cbind(anno, lab,HR,veg)
anno$equation<-paste("y = ",anno$m,"x + ",anno$y1, sep="") 
anno$equation[which(anno$equation=="y = NAx + NA")]<- " "
anno$R2m <- c(NA,NA,0.0924,NA,0.0330,NA,NA,0.358,NA,NA,0.00635,0.568,0.0469,NA,0.00586,0.149,0.0531,0.0740,0.000115,NA,NA,NA,0.227,0.00780,0.0592,NA,NA,NA,NA,NA)
anno$R2c <- c(NA,NA,0.386,NA,0.237,NA,NA,0.608,NA,NA,0.00635,0.568,0.303,NA,0.252,0.343,0.469,0.986,0.667,NA,NA,NA,0.455,0.720,0.548,NA,NA,NA,NA,NA)
anno$R2<- paste("R2m = ",anno$R2m,", R2c = ",anno$R2c, sep="")
anno$R2[which(anno$R2=="R2m = NA, R2c = NA")]<- " "
anno$R2mLab <- paste("R2m = ",anno$R2m,sep="")
anno$R2mLab[which(anno$R2mLab=="R2m = NA")]<- " "
anno$R2cLab <- paste("R2c = ",anno$R2c,sep="")
anno$R2cLab[which(anno$R2cLab=="R2c = NA")]<- " "

anno$HR<-factor(anno$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                                   "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                                   "South Lahontan","Colorado River"))
anno$veg<-factor(anno$veg,levels=c("Willow","Cottonwood","Valley Oak"))

ggplot(data=AllVeg_SeasonalMedian, aes(x=DTW, y=NDVI_LinApprox,)) + 
  geom_point(aes(fill=HR),colour="black",shape=21) + 
  geom_segment(data=anno,aes(x=x1,xend=x2, y=y1, yend = y2), size=1)+ 
  ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,25)+ 
  geom_text(data=anno,aes(label=equation),x=0,y=0.95,parse=FALSE, hjust=0, size=3.5) +
  geom_text(data=anno,aes(label=R2mLab),x=25,y=0.95,parse=FALSE, hjust=1, size=3.5) +
  geom_text(data=anno,aes(label=R2cLab),x=25,y=0.75,parse=FALSE, hjust=1, size=3.5) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44")) +
  facet_grid(HR~veg, labeller = labeller(HR = label_wrap_gen(10))) +
  theme(plot.title = element_text(hjust = 0, size=14),legend.position="none",
        axis.text.x = element_text(size = 9), axis.text.y=element_text(size=9),
        axis.title.x = element_text(size=12), axis.title=element_text(size=12),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold")) 





##################
##  FIGURE S3 ###
#################

AllVeg_SeasonalMedian<- read.csv("~/Dropbox/SUNY/Data/Modified/AllVeg_SeasonalMedian.csv",header=TRUE,sep=',')
AllVeg_SeasonalMedian$HR<-factor(AllVeg_SeasonalMedian$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                                                   "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                                                   "South Lahontan","Colorado River"))
AllVeg_SeasonalMedian$NHD <- factor(AllVeg_SeasonalMedian$NHD,levels=c("Natural","Altered"))
AllVeg_SeasonalMedian<- na.omit(AllVeg_SeasonalMedian)
unique(AllVeg_SeasonalMedian$NHD)



anno<- data.frame(x1=c(NA,NA,0,7,NA,NA,0,NA,NA,NA,NA,NA,0,NA,0,0,0,0,NA,NA),x2=c(NA,NA,25,13,NA,NA,20,NA,NA,NA,NA,NA,20,NA,10,25,20,15,NA,NA),
                  y1=c(NA,NA,0.648,0.730,NA,NA,0.457,NA,NA,NA,NA,NA,0.589,NA,0.450,0.310,0.371,0.634,NA,NA),m=c(NA,NA,-0.0135,-0.0457,NA,NA,-0.0110,NA,NA,NA,NA,NA,-0.00516,NA,-0.00655,-0.00387,-0.00609,-0.0157,NA,NA))

anno$y2<- (anno$m*anno$x2)+anno$y1


lab=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
      "San Joaquin River","Tulare Lake","Central Coast","South Coast",
      "South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
      "San Joaquin River","Tulare Lake","Central Coast","South Coast",
      "South Lahontan","Colorado River")
HR=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
     "San Joaquin River","Tulare Lake","Central Coast","South Coast",
     "South Lahontan","Colorado River","North Coast","North Lahontan","Sacramento River","San Francisco Bay",
     "San Joaquin River","Tulare Lake","Central Coast","South Coast",
     "South Lahontan","Colorado River")
NHD=c("Natural","Natural","Natural","Natural","Natural","Natural","Natural","Natural","Natural","Natural",
      "Altered","Altered","Altered","Altered","Altered","Altered","Altered","Altered","Altered","Altered")
anno<- cbind(anno, lab,HR,NHD)

anno$equation<-paste("y = ",anno$m,"x + ",anno$y1, sep="") 
anno$equation[which(anno$equation=="y = NAx + NA")]<- " "

#Edit SFBay Natural
anno$y2[which(anno$HR=="San Francisco Bay" & anno$NHD=="Natural")]<- (-0.0457*13)+1.05
anno$equation[which(anno$HR=="San Francisco Bay" & anno$NHD=="Natural")]<- "y = -0.0457x + 1.05"


anno$R2m <- c(NA,NA,0.406,NA,0.231,0.0296,0.224,NA,0.0602,NA,0.00635,NA,0.0282,0.00361,0.0242,0.785,0.0389,0.320,0.00145,NA)
anno$R2c <- c(NA,NA,0.653,NA,0.606,0.391,0.529,NA,0.626,NA,0.00635,NA,0.293,0.731,0.317,0.990,0.442,0.643,0.673,NA)
anno$R2<- paste("R2m = ",anno$R2m,", R2c = ",anno$R2c, sep="")
anno$R2[which(anno$R2=="R2m = NA, R2c = NA")]<- " "
anno$R2mLab <- paste("R2m = ",anno$R2m,sep="")
anno$R2mLab[which(anno$R2mLab=="R2m = NA")]<- " "
anno$R2cLab <- paste("R2c = ",anno$R2c,sep="")
anno$R2cLab[which(anno$R2cLab=="R2c = NA")]<- " "
anno$R2Lab <- " "
anno$R2Lab[which(anno$HR=="San Francisco Bay" & anno$NHD=="Natural")]<-"R2 = 0.96"


anno$HR<-factor(anno$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                                 "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                                 "South Lahontan","Colorado River"))
anno$NHD<-factor(anno$NHD,levels=c("Natural","Altered"))

ggplot(data=AllVeg_SeasonalMedian, aes(x=DTW, y=NDVI_LinApprox,)) + 
  geom_point(aes(fill=HR),colour="black",shape=21) + 
 geom_segment(data=anno,aes(x=x1,xend=x2, y=y1, yend = y2), size=1)+ 
  ylab("NDVI") + xlab("DTG (m)") + ylim(0,1)  +xlim(0,25)+ 
  geom_text(data=anno,aes(label=equation),x=0,y=0.95,parse=FALSE, hjust=0, size=3.5) +
  geom_text(data=anno,aes(label=R2mLab),x=25,y=0.95,parse=FALSE, hjust=1, size=3.5) +
  geom_text(data=anno,aes(label=R2cLab),x=25,y=0.75,parse=FALSE, hjust=1, size=3.5) +
  geom_text(data=anno,aes(label=R2Lab),x=25,y=0.95,parse=FALSE, hjust=1, size=3.5) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("#5E50A1","#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#FAAD61","#F36D44","#D53E4F","#9E1C44")) +
  facet_grid(HR~NHD, labeller = labeller(HR = label_wrap_gen(10))) +
  theme(plot.title = element_text(hjust = 0, size=14),legend.position="none",
        axis.text.x = element_text(size = 9), axis.text.y=element_text(size=9),
        axis.title.x = element_text(size=12), axis.title=element_text(size=12),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold")) 







#########  Figures S3 and S4 - NDVI vs. DTW by region   ##########

df<- AllVeg_SeasonalMedian

ggplot(data=df, aes(x=DTW, y=NDVI_LinApprox, color=season)) + facet_wrap(~HR) +geom_point() +ylim(0,1)  + xlim(0,50)



#Sac River
p1<-ggplot(data=df[which(df$SEASON==2 & df$HR=="Sacramento River"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  geom_point(aes(shape=veg), fill="#68C3A5") + scale_shape_manual(values=c(21,24,22)) + 
  geom_segment(aes(x=0, y=0.63, xend=20, yend=(-0.0035*20)+0.63), size=1) +
  ylab("NDVI") + xlab("DTG(m)") + ylim(0,1)  + xlim(0,25) +
  theme_bw() + ggtitle ("Spring") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) 

p2<-ggplot(data=df[which(df$SEASON==3 & df$HR=="Sacramento River"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  geom_point(aes(shape=veg), fill="#68C3A5") + scale_shape_manual(values=c(21,24,22)) + 
  geom_abline(intercept = 0.56, slope = -0.0077, size=1) +  
  annotate("text",x=20,y=0.95, label = "Interaction", size=7) +
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=7) +
  ylab("NDVI") + xlab("DTG(m)") + ylim(0,1)  + xlim(0,25) + 
  theme_bw() + ggtitle ("Summer") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20))

grid.arrange(p1,p2,nrow=1)

#S. Lahontan
p1<-ggplot(data=df[which(df$SEASON==2 & df$HR=="South Lahontan"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  ylab("NDVI") + xlab("DTG(m)") + ylim(0,1)  + xlim(0,25) + 
  geom_point(aes(shape=veg), fill="#D53E4F") + scale_shape_manual(values=c(21,24,22)) + 
  theme_bw() + ggtitle ("Spring") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) 


p2<- ggplot(data=df[which(df$SEASON==3 & df$HR=="South Lahontan"),], aes(x=DTW, y=NDVI_LinApprox)) +
  ylab("NDVI") + xlab("DTG(m)") + ylim(0,1)  + xlim(0,25) +
  annotate("text",x=20,y=0.95, label = "Interaction", size=7) +
  annotate("text",x=20,y=0.85, label = "n.s.", size=7) +
  geom_point(aes(shape=veg), fill="#D53E4F") + scale_shape_manual(values=c(21,24,22)) + 
  theme_bw() + ggtitle ("Summer") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20))

grid.arrange(p1,p2,nrow=1)




#####################################
##         OMITTED FIGURES        ###
#####################################

#########  Figure 3 - NDVI vs. DTW by region   ##########
#South Coast
df<- AllVeg_SeasonalMedian
#SCoastPlot<- df[which(df$HR=="South Coast"),]
#SCoastPlot$veg[which(SCoastPlot$veg=="Cottonwood")] <- NA
#SCoastPlot<-na.omit(SCoastPlot)

p1<-ggplot(data=df[which(df$SEASON==2 & df$HR=="South Coast"),], aes(x=DTW, y=NDVI_LinApprox)) +
  ylab("NDVI") + xlab("DTW(m)") + ylim(0,1)  + xlim(0,25) + 
  geom_point(aes(shape=veg), fill="#F36D44") + scale_shape_manual(values=c(21,24,22)) + 
  #geom_segment(aes(x=0, y=0.58, xend=25, yend=(-0.0028*25)+0.58), size=1) +
  theme_bw() + ggtitle ("Spring") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) 

p2<- ggplot(data=df[which(df$SEASON==3 & df$HR=="South Coast"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  ylab("NDVI") + xlab("DTW(m)") + ylim(0,1)  + xlim(0,25) + geom_point(aes(shape=veg), fill="#F36D44") + 
  scale_shape_manual(values=c(21,24,22)) + 
  annotate("text",x=20,y=0.95, label = "Interaction", size=7) +
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=7) +
  geom_segment(aes(x=0, y=0.61, xend=15, yend=(-0.015*15)+0.61), size=1) +
  theme_bw() + ggtitle ("Summer") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20))

grid.arrange(p1,p2,nrow=1)

#San Joaquin
p1<-ggplot(data=df[which(df$SEASON==2 & df$HR=="San Joaquin River"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  ylab("NDVI") + xlab("DTW(m)") + ylim(0,1)  + xlim(0,25) + 
  geom_segment(aes(x=0, y=0.50, xend=15, yend=(0.009*15)+0.50), size=1) +
  geom_point(aes(shape=veg), fill="#AED8A4") + scale_shape_manual(values=c(21,24,22)) + 
  theme_bw() + ggtitle ("Spring") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20)) 

p2<- ggplot(data=df[which(df$SEASON==3 & df$HR=="San Joaquin River"),], aes(x=DTW, y=NDVI_LinApprox)) + 
  ylab("NDVI") + xlab("DTW(m)") + ylim(0,1)  + xlim(0,25) + 
  #geom_segment(aes(x=0, y=0.37, xend=15, yend=(0.013*15)+0.37), size=1) +
  annotate("text",x=20,y=0.95, label = "Interaction", size=7) +
  annotate("text",x=20,y=0.85, label = "p = 0.16", size=7) +
  geom_point(aes(shape=veg), fill="#AED8A4") + scale_shape_manual(values=c(21,24,22)) + 
  theme_bw() + ggtitle ("Summer") + theme(plot.title = element_text(hjust = 0.5, size=22),legend.position="none",
                                          axis.text.x = element_text(size = 18,), axis.text.y=element_text(size=18),
                                          axis.title.x = element_text(size=20,), axis.title=element_text(size=20))
grid.arrange(p1,p2,nrow=1)


#########  Figure S2 - Cottonwood NDVI Timeseries by region   ##########

p1<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="North Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="North Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("North Coast")

p2<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="North Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="North Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("North Lahontan") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p3<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="San Francisco Bay"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="San Francisco Bay"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("San Francisco Bay") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p4<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="Sacramento River"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="Sacramento River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("Sacramento River") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p5<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="Central Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="Central Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("Central Coast") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p6<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="San Joaquin River"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="San Joaquin River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("San Joaquin River") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p7<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="South Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="South Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("South Coast") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p8<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="Tulare Lake"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="Tulare Lake"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("Tulare Lake") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p9<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="South Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="South Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("South Lahontan") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

p10<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Cottonwood[which(Cottonwood$HR_NAME=="Colorado River"),], color="gray") + theme_bw() + 
  geom_ma(data=Cottonwood_med[which(Cottonwood_med$HR_NAME=="Colorado River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1) + ggtitle ("Colorado River") + scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01")))

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow=5)


#########  Figure S3 - Willow NDVI Timeseries by region   ##########

p1<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="North Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="North Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("North Coast")

p2<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="North Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="North Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("North Lahontan")

p3<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="San Francisco Bay"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="San Francisco Bay"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("San Francisco Bay")

p4<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="Sacramento River"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="Sacramento River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Sacramento River")

p5<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="Central Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="Central Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Central Coast")

p6<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="San Joaquin River"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="San Joaquin River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("San Joaquin River")

p7<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="South Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="South Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("South Coast")

p8<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="Tulare Lake"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="Tulare Lake"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Tulare Lake")

p9<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="South Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="South Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("South Lahontan")

p10<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=Willow[which(Willow$HR_NAME=="Colorado River"),], color="gray") + theme_bw() + 
  geom_ma(data=Willow_med[which(Willow_med$HR_NAME=="Colorado River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Colorado River")

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow=5)


#########  Figure S4 - Valley Oak NDVI Timeseries by region   ##########

p1<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="North Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="North Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("North Coast")

p2<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="North Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="North Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("North Lahontan")

p3<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="San Francisco Bay"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="San Francisco Bay"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("San Francisco Bay")

p4<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="Sacramento River"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="Sacramento River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Sacramento River")

p5<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="Central Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="Central Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Central Coast")

p6<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="San Joaquin River"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="San Joaquin River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("San Joaquin River")

p7<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="South Coast"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="South Coast"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("South Coast")

p8<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="Tulare Lake"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="Tulare Lake"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Tulare Lake")

p9<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="South Lahontan"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="South Lahontan"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("South Lahontan")

p10<- ggplot(data=NULL, aes(x=date,y=NDVI, group=POLYGON_ID)) + 
  geom_line(data=ValleyOak[which(ValleyOak$HR_NAME=="Colorado River"),], color="gray") + theme_bw() + 
  geom_ma(data=ValleyOak_med[which(ValleyOak_med$HR_NAME=="Colorado River"),], aes(x=date, y=NDVI), ma_fun=SMA, n=8, linetype="solid", colour="black") +
  xlab("") + ylim(0,1)+ scale_x_date(limits=as.Date(c("2015-06-01","2020-01-01"))) + ggtitle ("Colorado River")

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow=5)


#########   Figure 3 - NDVI v DTW by Stream Flow Alteration & Season  ##########

#### Figure 3c ####
#Altered
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]
df$HR<-factor(df$HR,levels=c("North Coast","North Lahontan","Sacramento River","San Francisco Bay",
                             "San Joaquin River","Tulare Lake","Central Coast","South Coast",
                             "South Lahontan","Colorado River"))
df$veg<-factor(df$veg,levels=c("Cottonwood","Willow","Valley Oak"))

p1<- ggplot(data=df[which(df$NHD=="Altered" & df$SEASON==2),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,30)+ 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Altered - Spring") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = -0.28, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

p2<- ggplot(data=df[which(df$NHD=="Altered" & df$SEASON==3),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  geom_segment(aes(x=0, y=0.47, xend=30, yend=(-0.0038*30)+0.47)) +
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,30)+ 
  annotate("text",x=20,y=0.95, label = "Interaction", size=5) +
  annotate("text",x=20,y=0.85, label = "p < 0.001", size=5)+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Altered - Summer") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = -0.28, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

p3<- ggplot(data=df[which(df$NHD=="Natural" & df$SEASON==2),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  geom_segment(aes(x=0, y=0.63, xend=30, yend=(-0.0076*30)+0.63)) +
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,30)+ 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Natural - Spring") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = -0.28, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

p4<- ggplot(data=df[which(df$NHD=="Natural" & df$SEASON==3),],aes(x=DTW,y=NDVI_LinApprox))+ 
  geom_point(aes(fill=HR),colour="black",shape=21) +
  #geom_segment(aes(x=0, y=0.49, xend=25, yend=(-0.0089*25)+0.49)) +
  ylab("NDVI") + xlab(" ") + ylim(0,1)  +xlim(0,30)+ 
  annotate("text",x=20,y=0.95, label = "Interaction", size=5) +
  annotate("text",x=20,y=0.85, label = "n.s.", size=5)+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Natural - Summer") +
  scale_fill_manual(values=c("#3288BD","#68C3A5","#AED8A4","#E5EB9A","#FDDF8B","#F36D44","#D53E4F","#9E1C44")) +
  theme(plot.title = element_text(hjust = -0.28, size=18),legend.position="none",
        axis.text.x = element_text(size = 16,), axis.text.y=element_text(size=16),
        axis.title.x = element_text(size=16,), axis.title=element_text(size=16)) 

grid.arrange(p1,p2,p3,p4,nrow=2)


