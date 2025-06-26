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

WF_types <- as.data.table(readRDS('./data/cir_types.rds'))
WF_types_join<-join(dta, WF_types, by = "DTM")
mer<-WF_types_join[month %in% c(6, 7,8,9),]

mer<-mer[, mean_region_pe_p:=mean(PE-dp),by='DTM']
mer<-mer[,mean_TG_region:=mean(dtg), by='DTM']
mer<-mer[,mean_P_region:=mean(dp), by='DTM']

WF_types_join<-mer
WF_types_join1<-unique(WF_types_join, by='DTM')

WF_types_join1<-as.data.table(WF_types_join1)
WF_types_join1<-WF_types_join1[CT%in%c('S', 'CS', 'AS') , CT2 := 'S']
WF_types_join1<-WF_types_join1[CT%in%c('N', 'CN', 'AN') , CT2 := 'N']
WF_types_join1<-WF_types_join1[CT%in%c('W', 'CW', 'AW') , CT2 := 'W']
WF_types_join1<-WF_types_join1[CT%in%c('E', 'CE', 'AE') , CT2 := 'E']
WF_types_join1<-WF_types_join1[CT%in%c('NE', 'ANE', 'CNE') , CT2 := 'NE']
WF_types_join1<-WF_types_join1[CT%in%c('SE', 'ASE', 'CSE') , CT2 := 'SE']
WF_types_join1<-WF_types_join1[CT%in%c('SW', 'ASW', 'CSW') , CT2 := 'SW']
WF_types_join1<-WF_types_join1[CT%in%c('NW', 'ANW', 'CNW') , CT2 := 'NW']
WF_types_join1<-WF_types_join1[CT%in%c('A') , CT2 := 'A']
WF_types_join1<-WF_types_join1[CT%in%c('C') , CT2 := 'C']
WF_types_join1<-WF_types_join1[CT%in%c('U') , CT2 := 'U']

mer2<-WF_types_join1
mer2<-mer2[,pet_p:=mean(mean_region_pe_p), by=c('CT2')] 
mer2<-mer2[,tg:=mean(mean_TG_region), by=c('CT2')]
mer2<-mer2[,p:=mean(mean_P_region), by=c('CT2')] 
mer2[CT2=='A',]

AMJJAS_5_50<-mer2
AMJJAS_5_50<-AMJJAS_5_50[,count:=ave(CT2, CT2,FUN=length)]
AMJJAS_5_50[CT2=='A',]
AMJJAS_5_50<-unique(AMJJAS_5_50, by = "CT2")
AMJJAS_5_50 <- AMJJAS_5_50[ , count := as.numeric(count)]
AMJJAS_5_50 <- AMJJAS_5_50[ , sum_count := sum(count)]
AMJJAS_5_50<-AMJJAS_5_50[,prob:=round((count/sum_count)*100, 1)]

tab_HW<-AMJJAS_5_50

tab_HW[,CT:=NULL]
setnames(tab_HW, c('CT2'), c('CT'))
tab_HW<-tab_HW[,c('CT', 'pet_p', 'tg', 'p' , 'prob')]
klima <- readRDS("./data/klima.rds")

tab_HW<-tab_HW[, prob_klim:=prob] 
tab_HW<-tab_HW[, pet_p_klim:=pet_p]
tab_HW<-tab_HW[, tg_klim:=tg-klima]
tab_HW<-tab_HW[, p_klim:=p] 
tab_HW<-tab_HW[, Cef_klim:=prob] 
tab_HW<-tab_HW[, c('pet_p', 'tg', 'p', 'prob'):=NULL]
tab_HW<-setnames(tab_HW, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HW<-tab_HW[match(x, make.unique(CT))]
tab_HW<-tab_HW %>% mutate_if(is.numeric, ~round(., 2))

setcolorder(tab_HW, c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))
colnames(tab_HW) <- c('CT', 'June-September   \n freq. [%]    P [mm/d]', 'June-September   \n Cef [-]    P [mm/d]', 'June-September   \n PET-P [mm/d]    P [mm/d]', 'June-September   \n dev. TG [°C]    P [mm/d]',  'June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')

DTA_PROB = tab_HW[,c('CT', 'June-September   \n freq. [%]    P [mm/d]')]
colnames(DTA_PROB) <- c('CT','June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PROB = melt(DTA_PROB[,c('CT','June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_PETP = tab_HW[,c('CT', 'June-September   \n PET-P [mm/d]    P [mm/d]')]
colnames(DTA_PETP) <- c('CT','June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PETP = melt(DTA_PETP[,c('CT','June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_TG = tab_HW[,c('CT', 'June-September   \n dev. TG [°C]    P [mm/d]')]
colnames(DTA_TG) <- c('CT', 'June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_TG = melt(DTA_TG[,c('CT', 'June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_P = tab_HW[,c('CT', 'June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')]
colnames(DTA_P) <- c('CT', 'June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_P = melt(DTA_P[,c('CT','June-September   \n freq. [%]      PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

position <- as.factor(c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))

DTA_HW_PROB<-as.data.table(DTA_PROB)
DTA_HW_PETP<-as.data.table(DTA_PETP)
DTA_HW_TG<-as.data.table(DTA_TG)
DTA_HW_P<-as.data.table(DTA_P)

#saveRDS(DTA_HW_PROB, "./data/dta_prob_11.rds")
#saveRDS(DTA_HW_PETP, "./data/dta_PETP_11.rds")
#saveRDS(DTA_HW_TG, "./data/dta_TG_11.rds")
#saveRDS(DTA_HW_P, "./data/dta_P_11.rds")
