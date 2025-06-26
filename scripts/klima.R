lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dta <- readRDS("./data/dta_final_daily.rds")
dta<-dta[year>=1979 & year<=2022,]
dta<-dta[, count := .N, by = .(charLONLAT)] 
max(dta[, count])
dta<-dta[count==16071,]
dta<-dta[month %in% c(6, 7,8,9),]
dta<-dta[,mean_TG:=mean(dtg)]
klima<-dta[1,mean_TG]
#saveRDS(klima, "./data/klima.rds")
