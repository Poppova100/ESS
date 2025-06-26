library(raster)
library(rgdal)
library(data.table)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(dplyr)

mQrundta_tg <- readRDS("./data/mQrundta_tg.rds")
DTA <- readRDS("./data/DTA.rds")
mQrundta_p <- readRDS("./data/mQrundta_p.rds")

DTA[, LON:=NULL]
DTA[, LAT:=NULL]
DTA[, nday:=NULL]
DTA[,month:=month(ymd(DTMtg))]
DTA[,m_pet :=sum(PE, na.rm = T),by=.(month, charLONLAT, YEAR)]
DTA[,m_tg :=mean(dtg, na.rm = T),by=.(month, charLONLAT, YEAR)]
DTA[,year_tg :=mean(dtg, na.rm = T),by=.(charLONLAT, YEAR)]
DTA[, Re:=NULL]
#saveRDS(DTA, "./data/DTA2_daily.rds")

##### precipitation
mQrundta_p[,m_p :=sum(dp, na.rm = T),by=.(monthp, charLONLAT, yearp)]
mQrundta_p[,year_P :=sum(dp, na.rm = T),by=.(charLONLAT, yearp)]
setnames(mQrundta_p, c('DTMp', 'yearp', 'monthp'), c('DTM', 'year', 'month'))

#setwd("C:/_czu/PHD/FD_paper/data/")
#saveRDS(mQrundta_p, "./data/mQrundta_p2_daily.rds")

setnames(DTA, c('YEAR'), c('year'))
setnames(DTA, c('DTMtg'), c('DTM'))
DTA[, year_pet:=NULL]
DTA[, year_tg:=NULL]
DTA[, m_pet:=NULL]
DTA[, m_tg:=NULL]
mQrundta_p[, m_p:=NULL]
mQrundta_p[, year_P:=NULL]

DTA[, c("LON", "LAT") := tstrsplit(charLONLAT, " ", fixed=TRUE)]
DTA <- DTA[, LON:=as.numeric(LON)]
DTA <- DTA[, LAT:=as.numeric(LAT)]

DTA<-DTA %>% mutate_at(vars(LON, LAT), funs(round(., 3)))
mQrundta_p<-mQrundta_p %>% mutate_at(vars(LON, LAT), funs(round(., 3)))

mer=merge(DTA,mQrundta_p,by=c("LON", "LAT", "DTM"))
mer$charLONLAT <- paste(mer$LON, mer$LAT, sep=" ")
mer[, charLONLAT.y:=NULL]
mer[, charLONLAT.x:=NULL]
mer[, year.x:=NULL]
mer[, month.x:=NULL]
setnames(mer, c('year.y','month.y'), c('year', 'month'))
mer[charLONLAT=='8.0499 48.0499' & year==1950,]
#saveRDS(mer, "./data/dta_final_daily.rds")
