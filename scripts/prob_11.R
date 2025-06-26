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

WF_types_join2<-WF_types_join[month %in% c(6, 7,8,9),] 

WF_types_join2 <- WF_types_join2[ , mean_dtg := mean(dtg)] 
WF_types_join2 <- WF_types_join2[ , mean_dp := mean(dp)]
WF_types_join2 <- WF_types_join2[ , mean_dPE := mean(PE)]

WF_types_join2<-WF_types_join2[CT%in%c('S', 'CS', 'AS') , CT2 := 'S']
WF_types_join2<-WF_types_join2[CT%in%c('N', 'CN', 'AN') , CT2 := 'N']
WF_types_join2<-WF_types_join2[CT%in%c('W', 'CW', 'AW') , CT2 := 'W']
WF_types_join2<-WF_types_join2[CT%in%c('E', 'CE', 'AE') , CT2 := 'E']
WF_types_join2<-WF_types_join2[CT%in%c('NE', 'ANE', 'CNE') , CT2 := 'NE']
WF_types_join2<-WF_types_join2[CT%in%c('SE', 'ASE', 'CSE') , CT2 := 'SE']
WF_types_join2<-WF_types_join2[CT%in%c('SW', 'ASW', 'CSW') , CT2 := 'SW']
WF_types_join2<-WF_types_join2[CT%in%c('NW', 'ANW', 'CNW') , CT2 := 'NW']
WF_types_join2<-WF_types_join2[CT%in%c('A') , CT2 := 'A']
WF_types_join2<-WF_types_join2[CT%in%c('C') , CT2 := 'C']
WF_types_join2<-WF_types_join2[CT%in%c('U') , CT2 := 'U']

WF_types_join2 <- WF_types_join2[ , TG_JS_ct := mean(mean_dtg), by='CT2']
WF_types_join2 <- WF_types_join2[ , P_JS_ct := mean(mean_dp), by='CT2']
WF_types_join2 <- WF_types_join2[ , PE_JS_ct := mean(mean_dPE), by='CT2']

WF_types_join2 <- WF_types_join2[ , TG_JS := mean(dtg), by='CT2']
WF_types_join2 <- WF_types_join2[ , P_JS := mean(dp), by='CT2']
WF_types_join2 <- WF_types_join2[ , PE_JS := mean(PE), by='CT2']

WF_types_join2<-unique(WF_types_join2, by='DTM')

AMJJAS_5_50<-WF_types_join2[,count:=ave(CT2, CT2,FUN=length)]
AMJJAS_5_50[CT2=='NE',]
AMJJAS_5_50<-unique(AMJJAS_5_50, by = "CT2")
AMJJAS_5_50 <- AMJJAS_5_50[ , count := as.numeric(count)]
AMJJAS_5_50 <- AMJJAS_5_50[ , sum_count := sum(count)]
AMJJAS_5_50<-AMJJAS_5_50[,prob:=round((count/sum_count)*100, 1)]

AMJJAS_5_50<-AMJJAS_5_50[,c('CT2', 'prob', 'TG_JS', 'P_JS', 'PE_JS', 'TG_JS_ct', 'P_JS_ct', 'PE_JS_ct')]
AMJJAS_5_50<-setnames(AMJJAS_5_50, c('CT','PROB', 'TG_JS', 'P_JS', 'PE_JS', 'TG_JS_ct', 'P_JS_ct', 'PE_JS_ct'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
AMJJAS_5_50<-AMJJAS_5_50[match(x, make.unique(CT))]
AMJJAS_5_50<-AMJJAS_5_50 %>% mutate_if(is.numeric, ~round(., 2))
PROB<-AMJJAS_5_50 

#saveRDS(PROB, "./data/prob_11.rds")

