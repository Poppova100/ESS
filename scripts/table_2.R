lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

klima <- readRDS("./data/klima.rds")
PROB <- readRDS("./data/prob_11.rds")

DTA_HW_PROB <- readRDS("./data/dta_prob_11.rds")
DTA_HW_PETP <- readRDS("./data/dta_PETP_11.rds")
DTA_HW_TG <- readRDS("./data/dta_TG_11.rds")
DTA_HW_P <- readRDS("./data/dta_P_11.rds")

dta <- readRDS("./data/dta_final_daily.rds")

dta<-dta[year>=1979 & year<=2022,]
dta<-dta[, count := .N, by = .(charLONLAT)] 
max(dta[, count])
dta<-dta[count==16071,]

WF_types <- as.data.table(readRDS('./data/cir_types.rds'))

database = fread('./data/databaze_3DHWs_ME.csv',sep=';')
database<-database[,.(Start, End, Length, Extremity, Type)]
database[,DTM:=as.Date(Start)]

database<-database[,ID:=c(1:.N)] 

database_missing<-database[, {lst1 <- Map(seq, as.IDate(Start, "%Y-%m-%d"),
                             as.IDate(End, "%Y-%m-%d"),
                             MoreArgs = list(by = "day"))
.(ID = rep(ID, lengths(lst1)), DTM = do.call(c, lst1))}]
database_missing<-join(database_missing, database, by=c('DTM', 'ID'))
database_missing<-database_missing %>% fill(Start, End, Length, Extremity, Type)
database_missing<-as.data.table(database_missing)
database<-database_missing

WF_types_join<-join(dta, WF_types,by = "DTM")
WF_types_join2<-join(WF_types_join, database,by = "DTM")

mer<-na.omit(WF_types_join2, by='ID') 
mer<-mer[,day_order:=1:.N, by=ID] 
max(mer[,day_order])

mer<-mer[, mean_region_pe_p:=mean(PE-dp),by='DTM']
mer<-mer[,mean_TG_region:=mean(dtg), by='DTM']
mer<-mer[,mean_P_region:=mean(dp), by='DTM']

mer<-unique(mer, by=c('DTM')) 

mer<-as.data.table(mer)
mer<-mer[CT%in%c('S', 'CS', 'AS') , CT2 := 'S']
mer<-mer[CT%in%c('N', 'CN', 'AN') , CT2 := 'N']
mer<-mer[CT%in%c('W', 'CW', 'AW') , CT2 := 'W']
mer<-mer[CT%in%c('E', 'CE', 'AE') , CT2 := 'E']

mer<-mer[CT%in%c('NE', 'ANE', 'CNE') , CT2 := 'NE']
mer<-mer[CT%in%c('SE', 'ASE', 'CSE') , CT2 := 'SE']
mer<-mer[CT%in%c('SW', 'ASW', 'CSW') , CT2 := 'SW']
mer<-mer[CT%in%c('NW', 'ANW', 'CNW') , CT2 := 'NW']

mer<-mer[CT%in%c('A') , CT2 := 'A']
mer<-mer[CT%in%c('C') , CT2 := 'C']
mer<-mer[CT%in%c('U') , CT2 := 'U']


mer2<-mer[,.(DTM, CT2, Start, End, Length, Extremity, Type, ID, day_order, mean_region_pe_p, mean_TG_region, mean_P_region)]
mer2<-mer2[,pet_p:=mean(mean_region_pe_p), by=c('CT2', 'Type')]
mer2<-mer2[,tg:=mean(mean_TG_region), by=c('CT2', 'Type')] 
mer2<-mer2[,p:=mean(mean_P_region), by=c('CT2', 'Type')]

mer2[CT2=='A' & Type=='HWH',]

tab_HWL<-mer2[Type=='HWL',]
tab_HWH<-mer2[Type=='HWH',]
tab_HWO<-mer2[Type=='HWO',]
tab_HWG<-mer2[Type=='HWG',]

####################### HWL table
tab_HWL<-tab_HWL[,count:=ave(CT2, CT2,FUN=length)]
tab_HWL[CT2=='A',]
tab_HWL<-unique(tab_HWL, by = "CT2")
tab_HWL <- tab_HWL[ , count := as.numeric(count)]
tab_HWL <- tab_HWL[ , sum_count := sum(count)] 
tab_HWL<-tab_HWL[,prob:=round((count/sum_count)*100, 2)] 

####################### HWG table
tab_HWG<-tab_HWG[,count:=ave(CT2, CT2,FUN=length)]
tab_HWG<-unique(tab_HWG, by = "CT2")
tab_HWG <- tab_HWG[ , count := as.numeric(count)]
tab_HWG <- tab_HWG[ , sum_count := sum(count)] 
tab_HWG<-tab_HWG[,prob:=round((count/sum_count)*100, 2)]

####################### HWO table
tab_HWO<-tab_HWO[,count:=ave(CT2, CT2,FUN=length)]
tab_HWO<-unique(tab_HWO, by = "CT2")
tab_HWO <- tab_HWO[ , count := as.numeric(count)]
tab_HWO <- tab_HWO[ , sum_count := sum(count)] 
tab_HWO<-tab_HWO[,prob:=round((count/sum_count)*100, 2)]

####################### HWH table
tab_HWH<-tab_HWH[,count:=ave(CT2, CT2,FUN=length)]
tab_HWH[CT2=='A',]
tab_HWH<-unique(tab_HWH, by = "CT2")
tab_HWH <- tab_HWH[ , count := as.numeric(count)]
tab_HWH <- tab_HWH[ , sum_count := sum(count)] 
tab_HWH<-tab_HWH[,prob:=round((count/sum_count)*100, 2)] 

x <- data.table(CT2=c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HWH<-full_join(tab_HWH, x, by='CT2')
tab_HWO<-full_join(tab_HWO, x, by='CT2')
tab_HWL<-full_join(tab_HWL, x, by='CT2')
tab_HWG<-full_join(tab_HWG, x, by='CT2')

######### HWH
setnames(tab_HWH, c('CT2'), c('CT'))
tab_HWH<-tab_HWH[,c('CT', 'pet_p', 'tg', 'p' , 'prob')]

tab_HWH<-join(tab_HWH, PROB[,1:2], by=c('CT'))
tab_HWH<-tab_HWH[, prob_klim:=prob] 
tab_HWH<-tab_HWH[, pet_p_klim:=pet_p]
tab_HWH<-tab_HWH[, tg_klim:=tg-klima]
tab_HWH<-tab_HWH[, p_klim:=p]
tab_HWH<-tab_HWH[, Cef_klim:=prob/PROB] 

tab_HWH<-tab_HWH[, c('pet_p', 'tg', 'p', 'prob', 'PROB'):=NULL]
tab_HWH<-setnames(tab_HWH, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))

tab_HWH_1<-tab_HWH

setcolorder(tab_HWH, c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))
colnames(tab_HWH) <- c('CT', 'HWH   \n freq. [%]    P [mm/d]', 'HWH   \n Cef [-]    P [mm/d]', 'HWH   \n PET-P [mm/d]    P [mm/d]', 'HWH   \n dev. TG [°C]    P [mm/d]',  'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')

DTA_PROB = tab_HWH[,c('CT', 'HWH   \n freq. [%]    P [mm/d]')]
colnames(DTA_PROB) <- c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PROB = melt(DTA_PROB[,c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_PETP = tab_HWH[,c('CT', 'HWH   \n PET-P [mm/d]    P [mm/d]')]
colnames(DTA_PETP) <- c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PETP = melt(DTA_PETP[,c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_TG = tab_HWH[,c('CT', 'HWH   \n dev. TG [°C]    P [mm/d]')]
colnames(DTA_TG) <- c('CT', 'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_TG = melt(DTA_TG[,c('CT', 'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_Cef = tab_HWH[,c('CT', 'HWH   \n Cef [-]    P [mm/d]')]
colnames(DTA_Cef) <- c('CT', 'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_Cef = melt(DTA_Cef[,c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_P = tab_HWH[,c('CT', 'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')]
colnames(DTA_P) <- c('CT', 'HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_P = melt(DTA_P[,c('CT','HWH   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

position <- as.factor(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))

DTA_HWH_PROB<-as.data.table(DTA_PROB)
DTA_HWH_PETP<-as.data.table(DTA_PETP)
DTA_HWH_TG<-as.data.table(DTA_TG)
DTA_HWH_P<-as.data.table(DTA_P)
DTA_HWH_Cef<-as.data.table(DTA_Cef)

######### HWL
setnames(tab_HWL, c('CT2'), c('CT'))
tab_HWL<-tab_HWL[,c('CT', 'pet_p', 'tg', 'p' , 'prob')]

tab_HWL<-join(tab_HWL, PROB[,1:2], by=c('CT'))
tab_HWL<-tab_HWL[, prob_klim:=prob] 
tab_HWL<-tab_HWL[, pet_p_klim:=pet_p]
tab_HWL<-tab_HWL[, tg_klim:=tg-klima]
tab_HWL<-tab_HWL[, p_klim:=p]
tab_HWL<-tab_HWL[, Cef_klim:=prob/PROB] 

tab_HWL<-tab_HWL[, c('pet_p', 'tg', 'p', 'prob', 'PROB'):=NULL]
tab_HWL<-setnames(tab_HWL, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))

tab_HWL_1<-tab_HWL

setcolorder(tab_HWL, c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))
colnames(tab_HWL) <- c('CT', 'HWL   \n freq. [%]    P [mm/d]', 'HWL   \n Cef [-]    P [mm/d]', 'HWL   \n PET-P [mm/d]    P [mm/d]', 'HWL   \n dev. TG [°C]    P [mm/d]',  'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')

DTA_PROB = tab_HWL[,c('CT', 'HWL   \n freq. [%]    P [mm/d]')]
colnames(DTA_PROB) <- c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PROB = melt(DTA_PROB[,c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_PETP = tab_HWL[,c('CT', 'HWL   \n PET-P [mm/d]    P [mm/d]')]
colnames(DTA_PETP) <- c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PETP = melt(DTA_PETP[,c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_TG = tab_HWL[,c('CT', 'HWL   \n dev. TG [°C]    P [mm/d]')]
colnames(DTA_TG) <- c('CT', 'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_TG = melt(DTA_TG[,c('CT', 'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_Cef = tab_HWL[,c('CT', 'HWL   \n Cef [-]    P [mm/d]')]
colnames(DTA_Cef) <- c('CT', 'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_Cef = melt(DTA_Cef[,c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_P = tab_HWL[,c('CT', 'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')]
colnames(DTA_P) <- c('CT', 'HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_P = melt(DTA_P[,c('CT','HWL   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

position <- as.factor(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))

DTA_HWL_PROB<-as.data.table(DTA_PROB)
DTA_HWL_PETP<-as.data.table(DTA_PETP)
DTA_HWL_TG<-as.data.table(DTA_TG)
DTA_HWL_P<-as.data.table(DTA_P)
DTA_HWL_Cef<-as.data.table(DTA_Cef)

######### HWO
setnames(tab_HWO, c('CT2'), c('CT'))
tab_HWO<-tab_HWO[,c('CT', 'pet_p', 'tg', 'p' , 'prob')]

tab_HWO<-join(tab_HWO, PROB[,1:2], by=c('CT'))
tab_HWO<-tab_HWO[, prob_klim:=prob] 
tab_HWO<-tab_HWO[, pet_p_klim:=pet_p]
tab_HWO<-tab_HWO[, tg_klim:=tg-klima]
tab_HWO<-tab_HWO[, p_klim:=p] 
tab_HWO<-tab_HWO[, Cef_klim:=prob/PROB]

tab_HWO<-tab_HWO[, c('pet_p', 'tg', 'p', 'prob', 'PROB'):=NULL]
tab_HWO<-setnames(tab_HWO, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))

tab_HWO_1<-tab_HWO

setcolorder(tab_HWO, c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))
colnames(tab_HWO) <- c('CT', 'HWO   \n freq. [%]    P [mm/d]', 'HWO   \n Cef [-]    P [mm/d]', 'HWO   \n PET-P [mm/d]    P [mm/d]', 'HWO   \n dev. TG [°C]    P [mm/d]',  'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')

DTA_PROB = tab_HWO[,c('CT', 'HWO   \n freq. [%]    P [mm/d]')]
colnames(DTA_PROB) <- c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PROB = melt(DTA_PROB[,c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_PETP = tab_HWO[,c('CT', 'HWO   \n PET-P [mm/d]    P [mm/d]')]
colnames(DTA_PETP) <- c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PETP = melt(DTA_PETP[,c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_TG = tab_HWO[,c('CT', 'HWO   \n dev. TG [°C]    P [mm/d]')]
colnames(DTA_TG) <- c('CT', 'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_TG = melt(DTA_TG[,c('CT', 'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_Cef = tab_HWO[,c('CT', 'HWO   \n Cef [-]    P [mm/d]')]
colnames(DTA_Cef) <- c('CT', 'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_Cef = melt(DTA_Cef[,c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_P = tab_HWO[,c('CT', 'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')]
colnames(DTA_P) <- c('CT', 'HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_P = melt(DTA_P[,c('CT','HWO   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

position <- as.factor(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))

DTA_HWO_PROB<-as.data.table(DTA_PROB)
DTA_HWO_PETP<-as.data.table(DTA_PETP)
DTA_HWO_TG<-as.data.table(DTA_TG)
DTA_HWO_P<-as.data.table(DTA_P)
DTA_HWO_Cef<-as.data.table(DTA_Cef)

######### HWG
setnames(tab_HWG, c('CT2'), c('CT'))
tab_HWG<-tab_HWG[,c('CT', 'pet_p', 'tg', 'p' , 'prob')]

tab_HWG<-join(tab_HWG, PROB[,1:2], by=c('CT'))
tab_HWG<-tab_HWG[, prob_klim:=prob] 
tab_HWG<-tab_HWG[, pet_p_klim:=pet_p]
tab_HWG<-tab_HWG[, tg_klim:=tg-klima] 
tab_HWG<-tab_HWG[, p_klim:=p] 
tab_HWG<-tab_HWG[, Cef_klim:=prob/PROB]

tab_HWG<-tab_HWG[, c('pet_p', 'tg', 'p', 'prob', 'PROB'):=NULL]
tab_HWG<-setnames(tab_HWG, c('CT','prob', 'pet_p', 'tg', 'p', 'Cef'))

x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))

tab_HWG_1<-tab_HWG
setcolorder(tab_HWG, c('CT','prob', 'Cef', 'pet_p', 'tg', 'p'))
colnames(tab_HWG) <- c('CT', 'HWG   \n freq. [%]    P [mm/d]', 'HWG   \n Cef [-]    P [mm/d]', 'HWG   \n PET-P [mm/d]    P [mm/d]', 'HWG   \n dev. TG [°C]    P [mm/d]',  'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')

DTA_PROB = tab_HWG[,c('CT', 'HWG   \n freq. [%]    P [mm/d]')]
colnames(DTA_PROB) <- c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PROB = melt(DTA_PROB[,c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_PETP = tab_HWG[,c('CT', 'HWG   \n PET-P [mm/d]    P [mm/d]')]
colnames(DTA_PETP) <- c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_PETP = melt(DTA_PETP[,c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_TG = tab_HWG[,c('CT', 'HWG   \n dev. TG [°C]    P [mm/d]')]
colnames(DTA_TG) <- c('CT', 'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_TG = melt(DTA_TG[,c('CT', 'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_Cef = tab_HWG[,c('CT', 'HWG   \n Cef [-]    P [mm/d]')]
colnames(DTA_Cef) <- c('CT', 'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_Cef = melt(DTA_Cef[,c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

DTA_P = tab_HWG[,c('CT', 'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')]
colnames(DTA_P) <- c('CT', 'HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')
DTA_P = melt(DTA_P[,c('CT','HWG   \n freq. [%]      Cef [-]   PET-P [mm/d]      dev. TG [°C]      P [mm/d]')], id.vars = 'CT')

position <- as.factor(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'U'))

DTA_HWG_PROB<-as.data.table(DTA_PROB)
DTA_HWG_PETP<-as.data.table(DTA_PETP)
DTA_HWG_TG<-as.data.table(DTA_TG)
DTA_HWG_P<-as.data.table(DTA_P)
DTA_HWG_Cef<-as.data.table(DTA_Cef)


HW_TAB<-cbind(tab_HWG_1, tab_HWL_1, tab_HWH_1, tab_HWO_1)
HW_TAB<-as.data.frame(HW_TAB)

HW_TAB<-cbind(HW_TAB,PROB[,1:5])

#write.table(HW_TAB, file="./data/table_2.txt")