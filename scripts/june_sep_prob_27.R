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

mer<-WF_types_join[month %in% c(6, 7,8,9),]
mer<-mer[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
mer<-mer[,mean_TG_region:=mean(dtg), by='DTM']
mer<-mer[,mean_P_region:=mean(dp), by='DTM']

WF_types_join<-mer
WF_types_join1<-unique(WF_types_join, by='DTM')
WF_types_join1[DTM=='1980-08-01',]
WF_types_join1<-as.data.table(WF_types_join1)
WF_types_join1<-WF_types_join1[, CT2 := CT]

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
tab_HW<-tab_HW[, prob_klim:=prob] 
tab_HW<-tab_HW[, pet_p_klim:=pet_p]
tab_HW<-tab_HW[, tg_klim:=tg]
tab_HW<-tab_HW[, p_klim:=p] 
tab_HW<-tab_HW[, Cef_klim:=prob] 
tab_HW<-tab_HW[, c('pet_p', 'tg', 'p', 'prob'):=NULL]
tab_HW<-setnames(tab_HW, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW', 'C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
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

position <- as.factor(c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW', 'C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))

DTA_HW_PROB<-as.data.table(DTA_PROB)
DTA_HW_PETP<-as.data.table(DTA_PETP)
DTA_HW_TG<-as.data.table(DTA_TG)
DTA_HW_P<-as.data.table(DTA_P)

#saveRDS(DTA_HW_PROB, "./data/dta_prob_27.rds")
#saveRDS(DTA_HW_PETP, "./data/dta_PETP_27.rds")
#saveRDS(DTA_HW_TG, "./data/dta_TG_27.rds")
#saveRDS(DTA_HW_P, "./data/dta_P_27.rds")


