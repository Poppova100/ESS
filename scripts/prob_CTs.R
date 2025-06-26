lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dta <- readRDS("./data/dta_final_daily.rds")
dta<-dta[year>=1979 & year<=2022,]
dta<-dta[, count := .N, by = .(charLONLAT)] 
max(dta[, count])
dta<-dta[count==16071,]

WF_types <- as.data.table(readRDS('./data/cir_types.rds'))
WF_types_join<-join(dta, WF_types, by = "DTM")

WF_types_join1<-unique(WF_types_join, by='DTM')
WF_types_join1[DTM=='1980-04-01',]
WF_types_join1<-WF_types_join1[CT%in%c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW') , CT2 := 'A']
WF_types_join1<-WF_types_join1[CT%in%c('C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW') , CT2 := 'C']
WF_types_join1<-WF_types_join1[CT%in%c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW') , CT2 := 'DIR']
WF_types_join1<-WF_types_join1[CT%in%c('U') , CT2 := 'U']

AMJJAS_5_50<-WF_types_join1[month %in% c(6, 7,8,9),]
AMJJAS_5_50<-AMJJAS_5_50[,count:=ave(CT2, CT2,FUN=length)]
AMJJAS_5_50[CT2=='A',]
AMJJAS_5_50<-unique(AMJJAS_5_50, by = "CT2")
AMJJAS_5_50 <- AMJJAS_5_50[ , count := as.numeric(count)]
AMJJAS_5_50 <- AMJJAS_5_50[ , sum_count := sum(count)]
AMJJAS_5_50<-AMJJAS_5_50[,prob:=round((count/sum_count)*100, 1)]

AMJJAS_5_50<-AMJJAS_5_50[,c('CT2', 'prob')]
AMJJAS_5_50<-setnames(AMJJAS_5_50, c('CT','PROB'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
AMJJAS_5_50<-AMJJAS_5_50[match(x, make.unique(CT))]
AMJJAS_5_50<-AMJJAS_5_50 %>% mutate_if(is.numeric, ~round(., 2))
PROB<-AMJJAS_5_50
#saveRDS(PROB, "./data/prob_4.rds")
