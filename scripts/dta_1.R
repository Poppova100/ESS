library(raster)
library(data.table)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(insol)

f <- "./data/tg_ens_mean_0.1deg_reg_v28.0e.nc"

b <- brick(f, var="tg")

ex<-extent(2, 16, 48, 55)
b = crop(b, ex)
extent(b)

nam_grun=names(b)
nam_grunDT=data.table(nam_grun)
nam_grunDT[,DTM:= tstrsplit(nam_grun,"X")[[2]]]
nam_grunDT[,DTM:= as.Date(tstrsplit(nam_grun,"X")[[2]], format="%Y.%m.%d")]

dtaList=list()
for (i in 1:nrow(nam_grunDT)){
  GrunEUm_res_extDF=as.data.frame(b[[i]],xy=TRUE)
  names(GrunEUm_res_extDF) <-c("LON","LAT","mtg")
  GrunEUm_res_extDT = data.table(GrunEUm_res_extDF)
  GrunEUm_res_extDT[,DTMtg:=nam_grunDT$DTM[i]]
  GrunEUm_res_extDT[,yeartg:=year(ymd(DTMtg))]
  GrunEUm_res_extDT[,monthtg:=month(ymd(DTMtg))]
  GrunEUm_res_extDT[, xx:=as.character(LON)]
  GrunEUm_res_extDT[, yy:=as.character(LAT)]
  GrunEUm_res_extDT[, charLONLAT:=paste(LON,LAT)]
  GrunEUm_res_extDT[,xx:=NULL]
  GrunEUm_res_extDT[,yy:=NULL]

  dtaList[[i]] = copy(GrunEUm_res_extDT)
  
}
mQrundta_tg = rbindlist(dtaList)
setnames(mQrundta_tg, 'mtg', 'dtg')

#saveRDS(mQrundta_tg, "./data/mQrundta_tg.rds")

################## calculation of PET

#a <- readRDS("./data/mQrundta_tg.rds")
a=na.omit(a)

a[,'yeartg']<-NULL
a[,'monthtg']<-NULL
a[,YEAR:=year(ymd(DTMtg))]

a3<-a[!(format(DTMtg, "%d")=="29" & format(DTMtg, "%m")=="02"),]
a2<-a[format(DTMtg, "%d")=="29" & format(DTMtg, "%m")=="02",]
roky_leap<-unique(a2[,YEAR])
a3[,nday:=yday(a3[,DTMtg])]
p<-a3[format(DTMtg, "%d")>="01" & format(DTMtg, "%m")>="03",]
AA=format(as.Date(p[,DTMtg], format="%d/%m/%Y"),"%Y")
p<-p[,year:=AA]
p = p[year %in% c(roky_leap)]
p=p[,nday:=nday-1]
p[,year:=NULL]
l<-a3[!(format(DTMtg, "%d")>="01" & format(DTMtg, "%m")>="03"),]
l2<-a3[(format(DTMtg, "%d")>="01" & format(DTMtg, "%m")>="03"),]
AA=format(as.Date(l2[,DTMtg], format="%d/%m/%Y"),"%Y")
l2<-l2[,year:=AA]
l2 = l2[!year %in% c(roky_leap)]
l2[,year:=NULL]
a<-NULL
clean_year<-rbind(l,l2)
clean_year<-rbind(clean_year,p)
a3<-clean_year

#### calculation of Re
a2<-NULL
p<-NULL
clean_year<-NULL
l<-NULL
l2<-NULL

LAT=(c(a3[,LAT]))
Temp=a3[,dtg]
nday=a3[,nday]

dtaList=list()
for( i in 1:length(LAT)){
    Fi <- pi*LAT[i]/180
  den = nday[i]
  delta = 23.45* sin ((pi/180)*360/365*(284+den))
  omegaSS <- acos(-tan(Fi)*tan(delta* (pi/180) )) 
  
  j = den*2*pi/365.2422
  epsilon=0.03344*cos(j-0.049)
  distance_ratio=sqrt(1+epsilon) 
  
  solar_constant=1367*(distance_ratio)^2
  
  H  = (24/pi)* solar_constant * (cos(Fi)*cos(delta*pi/180)*sin(omegaSS) + omegaSS*sin(Fi)*sin(delta*pi/180)  )  # [ Wh m-2]
  E = H/24
  
  
  Re = H * 3600 /1000000  #  [MJ/den]
  dtaList[[i]] = copy(Re)
  
}

Re=as.data.table(unlist(dtaList))
#saveRDS(Re, "./data/Re.rds")

########
dtaList<-NULL
a3_2<-cbind(a3,Re)

#saveRDS(a3_2, "./data/a3_2.rds")

NIGHT<-a3_2[V1=='NaN',]

######### calculation of Re for February 29
a3<-NULL
#a <- readRDS("./data/mQrundta_tg.rds")
a[,YEAR:=year(ymd(DTMtg))]
#a3_2 <- readRDS("./data/a3_2.rds")

a2<-a[format(DTMtg, "%d")=="29" & format(DTMtg, "%m")=="02",]
roky_leap<-unique(a2[,YEAR])
b28<-a3_2[format(DTMtg, "%d")=="28" & format(DTMtg, "%m")=="02"& YEAR%in%roky_leap,]
b1<-a3_2[format(DTMtg, "%d")=="01" & format(DTMtg, "%m")=="03"& YEAR%in%roky_leap,]

b<-rbind(b28,b1)
b<-unique(b, by=c('charLONLAT', 'YEAR', 'DTMtg'))
b_re<-b[,Re2:= mean(V1), by=.(charLONLAT,YEAR)]

s=b_re[,unique(Re2), by=.(YEAR, charLONLAT)]
s<-unique(s,by=c('YEAR', 'charLONLAT'))

#saveRDS(a2, "./data/a2.rds")
#saveRDS(s, "./data/s.rds")

LONLAT<-unique(s[,charLONLAT])
roky_leap<-unique(a2[,YEAR])

a2 <- a2 %>%
  complete(charLONLAT=LONLAT, YEAR=roky_leap,
           fill = list(incidents = 'NA')) %>%
  as.data.table()

s <- s %>%
  complete(charLONLAT=LONLAT, YEAR=roky_leap,
           fill = list(incidents = 'NA')) %>%
  as.data.table()

h=cbind(a2, s)

setnames(h,'V1', 'Re')
setnames(a3_2,'V1', 'Re')

h<-h[,nday:=rep(59.5, length.out=nrow(h))]
h[,c(1, 2, 7,8)]<-NULL
setcolorder(h, c('LON', 'LAT', 'dtg', 'DTMtg', 'charLONLAT', 'YEAR', 'nday', 'Re'))

DTA<-rbind(a3_2,h)
DTA<-DTA[order(as.Date(DTA$DTMtg, format="%Y-%m-%d")),]
DTA<-unique(DTA, by=c('charLONLAT', 'YEAR', 'DTMtg'))
DTA<-na.omit(DTA)

############# calculation of PET
get_PE=function(TG, Re){
  if((TG+5)>0) 
    PE=(0.408*Re*(TG+5))/100
  else
    PE=0
} 
get_PE2 <- Vectorize(get_PE, vectorize.args = c('TG', 'Re'))

DTA[,PE:=get_PE2(dtg, Re), by=DTMtg]
DTA[,year_pet :=sum(PE, na.rm = T),by=.(YEAR, charLONLAT)]

#saveRDS(DTA, "./data/DTA.rds")

########################## P

f <- './data/rr_ens_mean_0.1deg_reg_v28.0e.nc'
b <- brick(f, var="rr")
ex<-extent(2, 16, 48, 55)
b = crop(b, ex)
extent(b)

nam_grun=names(b)
nam_grunDT=data.table(nam_grun)
nam_grunDT[,DTM:= tstrsplit(nam_grun,"X")[[2]]]
nam_grunDT[,DTM:= as.Date(tstrsplit(nam_grun,"X")[[2]], format="%Y.%m.%d")]

dtaList=list()
for (i in 1:nrow(nam_grunDT)){
  GrunEUm_res_extDF=as.data.frame(b[[i]],xy=TRUE)
  names(GrunEUm_res_extDF) <-c("LON","LAT","dp")
  GrunEUm_res_extDT = data.table(GrunEUm_res_extDF)
  GrunEUm_res_extDT[,DTMp:=nam_grunDT$DTM[i]]
  GrunEUm_res_extDT[,yearp:=year(ymd(DTMp))]
  GrunEUm_res_extDT[,monthp:=month(ymd(DTMp))]
  GrunEUm_res_extDT[, xx:=as.character(LON)]
  GrunEUm_res_extDT[, yy:=as.character(LAT)]
  GrunEUm_res_extDT[, charLONLAT:=paste(LON,LAT)]
  GrunEUm_res_extDT[,xx:=NULL]
  GrunEUm_res_extDT[,yy:=NULL]

  dtaList[[i]] = copy(GrunEUm_res_extDT)
}
mQrundta_p = rbindlist(dtaList)
#saveRDS(mQrundta_p, "./data/mQrundta_p.rds")

