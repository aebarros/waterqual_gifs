#data cleaning
#the purpose of this scripts is to prepare water quality data to be mapped
rm(list=ls()) #clear env
library(tidyverse)
library(readxl)
StationMetaData <- read_excel("data/StationMetaData_20190813.xlsx", 
                              sheet = "Data")
sn_waterinfo<-read.delim("data/sn_tblWaterTowInfo.txt")
ot_waterinfo<-read.delim("data/ot_tblTowWaterQual.txt")

#need just dates, lat/long, location info, and water qual info
sn_waterinfo<-dplyr::select(sn_waterinfo,polstn,towdate,16:25,28:39)
ot_waterinfo<-dplyr::select(ot_waterinfo,polstn,towdate,12:29,53:56)
ncol(sn_waterinfo)
ncol(ot_waterinfo)

#rowbind both datasets
wq<-sn_waterinfo%>%
  bind_rows(ot_waterinfo)

#fix dates
wq$towdate<-as.Date(wq$towdate,"%m/%d/%Y")
wq$year<-as.numeric(format(wq$towdate,'%Y'))
wq$month<-as.numeric(format(wq$towdate,'%m'))

#calculate averages for what I want
#calc mean water qualities
wq$m_secchi<-rowMeans(subset(wq, select = c("bsecchi","esecchi")), na.rm = TRUE)
wq$m_sal<-rowMeans(subset(wq, select = c("bssal","bbsal","essal","ebsal")), na.rm = TRUE)
wq$m_doc<-rowMeans(subset(wq, select = c("bsdoc","bbdoc","esdoc","ebdoc")), na.rm = TRUE)
wq$m_temp<-rowMeans(subset(wq, select = c("bstemp","bbtemp","estemp","ebtemp")), na.rm = TRUE)

#join station data
wq<-wq%>%
  inner_join(StationMetaData)

#select just what I want
wq<-dplyr::select(wq,towdate,polstn,m_secchi,m_sal,m_doc,m_temp,year,month,tribmarsh,region,loc,habitat,lat,lon)

#next, group by month, polygon, and calculate averages
wq<-wq%>%
  dplyr::group_by(towdate,month,year,lat,lon,tribmarsh,region,loc,habitat)%>%
  dplyr::summarise(m_secchi=mean(m_secchi),m_sal=mean(m_sal),m_doc=mean(m_doc),m_temp=mean(m_temp))

#filter out any data without lat/lon
wq<-wq[complete.cases(wq$lat), ]
#filter out high sal values
wq<-wq%>%
  filter(m_sal<42)
