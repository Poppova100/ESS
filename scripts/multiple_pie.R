lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'gridExtra', 'grid')
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
WF_types_join2[DTM=='1979-01-01',]

WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']
WF_types_join2[DTM=='1979-07-25',]

mer<-unique(WF_types_join2, by=c('DTM')) 
mer<-mer[CT%in%c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW') , CT2 := 'A']
mer<-mer[CT%in%c('C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW') , CT2 := 'C']
mer<-mer[CT%in%c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW') , CT2 := 'DIR']
mer<-mer[CT%in%c('U') , CT2 := 'U']

mer_unique<-unique(mer, by='DTM')
mer_unique<-mer_unique[, c("pred1", "pred2", "pred3", "po1", "po2", "po3") := shift(CT2, n = c(1,2,3, -1, -2, -3))]

merging_Start<-mer_unique[DTM==Start,]
merging_End<-mer_unique[DTM==End,]

merging_Start1<-merging_Start[,before_Type:= (c(pred1)), by=Start] 
merging_Start2<-merging_Start[,before_Type2:= (c(pred2)), by=Start] 
merging_Start3<-merging_Start[,before_Type3:= (c(pred3)), by=Start] 
merging_Start<-rbind(merging_Start1[, .(Type, CT2, before_Type)], merging_Start2[, .(Type, CT2,before_Type2)], merging_Start3[, .(Type, CT2, before_Type3)], use.names=FALSE)

tab_HWL<-merging_Start[Type=='HWL',]
tab_HWH<-merging_Start[Type=='HWH',]
tab_HWO<-merging_Start[Type=='HWO',]
tab_HWG<-merging_Start[Type=='HWG',]

####################### HWL table
tab_HWL<-tab_HWL[,count:=ave(before_Type, before_Type,FUN=length)] 
tab_HWL[before_Type=='A',]
tab_HWL<-unique(tab_HWL, by = "before_Type")
tab_HWL <- tab_HWL[ , count := as.numeric(count)]
tab_HWL <- tab_HWL[ , sum_count := sum(count)] 
tab_HWL<-tab_HWL[,prob:=round((count/sum_count)*100, 2)] 

####################### HWG table
tab_HWG<-tab_HWG[,count:=ave(before_Type, before_Type,FUN=length)] 
tab_HWG[before_Type=='A',]
tab_HWG<-unique(tab_HWG, by = "before_Type")
tab_HWG <- tab_HWG[ , count := as.numeric(count)]
tab_HWG <- tab_HWG[ , sum_count := sum(count)] 
tab_HWG<-tab_HWG[,prob:=round((count/sum_count)*100, 2)]  

####################### HWO table
tab_HWO<-tab_HWO[,count:=ave(before_Type, before_Type,FUN=length)] 
tab_HWO[before_Type=='A',]
tab_HWO<-unique(tab_HWO, by = "before_Type")
tab_HWO <- tab_HWO[ , count := as.numeric(count)]
tab_HWO <- tab_HWO[ , sum_count := sum(count)] 
tab_HWO<-tab_HWO[,prob:=round((count/sum_count)*100, 2)] 

####################### HWH table
tab_HWH<-tab_HWH[,count:=ave(before_Type, before_Type,FUN=length)] 
tab_HWH[before_Type=='A',]
tab_HWH<-unique(tab_HWH, by = "before_Type")
tab_HWH <- tab_HWH[ , count := as.numeric(count)]
tab_HWH <- tab_HWH[ , sum_count := sum(count)]
tab_HWH<-tab_HWH[,prob:=round((count/sum_count)*100, 2)] 


x <- data.table(before_Type=c('A', 'C', 'DIR', 'U'))
tab_HWH<-full_join(tab_HWH, x, by='before_Type')
tab_HWO<-full_join(tab_HWO, x, by='before_Type')
tab_HWL<-full_join(tab_HWL, x, by='before_Type')
tab_HWG<-full_join(tab_HWG, x, by='before_Type')

PROB <- readRDS("C:/_czu/PHD/FD_paper/data/PROB_4_NEW.rds")

######### HWH
tab_HWH[,CT:=NULL]
setnames(tab_HWH, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]

tab_HWH_1<-tab_HWH

######### HWL
tab_HWL[,CT:=NULL]
setnames(tab_HWL, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]

tab_HWL_1<-tab_HWL

######### HWO
tab_HWO[,CT:=NULL]
setnames(tab_HWO, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]

tab_HWO_1<-tab_HWO

######### HWG
tab_HWG[,CT:=NULL]
setnames(tab_HWG, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]

tab_HWG_1<-tab_HWG

HW_TAB<-cbind(tab_HWG_1[,prob], tab_HWL_1[,prob], tab_HWH_1[,prob], tab_HWO_1[,prob]) 
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-as.data.table(HW_TAB)
HWG_BARPLOT_TAB<-HWG_BARPLOT_TAB[,PROB:=NULL]

setnames(HWG_BARPLOT_TAB, c("V1", "V2", "V3", "V4"), c("HWG", "HWL", "HWH", "HWO"))

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT)

pie_BEFORE<-
  ggplot(transform(dat2,
                       Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+ 
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), labels = c("anticyclonic", "cyclonic", "directional", "indeterminate"))+
  theme(strip.text = element_text(size = 25, margin = margin(), face = 'bold'))+
  theme(legend.position = "none")

##############################
lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer')
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
WF_types_join2[DTM=='1979-01-01',]
WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']
WF_types_join2[DTM=='1979-07-25',]

mer<-unique(WF_types_join2, by=c('DTM')) 
mer<-mer[CT%in%c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW') , CT2 := 'A']
mer<-mer[CT%in%c('C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW') , CT2 := 'C']
mer<-mer[CT%in%c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW') , CT2 := 'DIR']
mer<-mer[CT%in%c('U') , CT2 := 'U']
mer_unique<-na.omit(mer, by='ID') 

tab_HWL<-mer_unique[Type=='HWL',]
tab_HWH<-mer_unique[Type=='HWH',]
tab_HWO<-mer_unique[Type=='HWO',]
tab_HWG<-mer_unique[Type=='HWG',]

####################### HWL table
tab_HWL<-tab_HWL[,count:=ave(CT2, CT2,FUN=length)] 
tab_HWL<-unique(tab_HWL, by = "CT2")
tab_HWL <- tab_HWL[ , count := as.numeric(count)]
tab_HWL <- tab_HWL[ , sum_count := sum(count)] 
tab_HWL<-tab_HWL[,prob:=round((count/sum_count)*100, 2)] 

####################### HWG table
tab_HWG<-tab_HWG[,count:=ave(CT2, CT2,FUN=length)] 
tab_HWG[CT2=='A',]
tab_HWG<-unique(tab_HWG, by = "CT2")
tab_HWG <- tab_HWG[ , count := as.numeric(count)]
tab_HWG <- tab_HWG[ , sum_count := sum(count)] 
tab_HWG<-tab_HWG[,prob:=round((count/sum_count)*100, 2)] 

####################### HWO table
tab_HWO<-tab_HWO[,count:=ave(CT2, CT2,FUN=length)] 
tab_HWO[CT2=='A',]
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


x <- data.table(CT2=c('A', 'C', 'DIR', 'U'))
tab_HWH<-full_join(tab_HWH, x, by='CT2')
tab_HWO<-full_join(tab_HWO, x, by='CT2')
tab_HWL<-full_join(tab_HWL, x, by='CT2')
tab_HWG<-full_join(tab_HWG, x, by='CT2')

PROB <- readRDS("./data/prob_4.rds")

######### HWH
tab_HWH[,CT:=NULL]
setnames(tab_HWH, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]

tab_HWH_1<-tab_HWH

######### HWL
tab_HWL[,CT:=NULL]
setnames(tab_HWL, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]

tab_HWL_1<-tab_HWL

######### HWO
tab_HWO[,CT:=NULL]
setnames(tab_HWO, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]

tab_HWO_1<-tab_HWO

######### HWG
tab_HWG[,CT:=NULL]
setnames(tab_HWG, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]

tab_HWG_1<-tab_HWG


HW_TAB<-cbind(tab_HWG_1[,prob], tab_HWL_1[,prob], tab_HWH_1[,prob], tab_HWO_1[,prob]) 
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-as.data.table(HW_TAB)
HWG_BARPLOT_TAB<-HWG_BARPLOT_TAB[,PROB:=NULL]

setnames(HWG_BARPLOT_TAB, c("V1", "V2", "V3", "V4"), c("HWG", "HWL", "HWH", "HWO"))
dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT)

pie_DURING<-
  ggplot(transform(dat2,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+ 
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), labels = c("anticyclonic", "cyclonic", "directional", "indeterminate"))+
  theme(strip.text = element_blank())+ 
  theme(legend.position = "none")

########################
lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer')
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
WF_types_join2[DTM=='1979-01-01',]

WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']
WF_types_join2[DTM=='1979-07-25',]

mer<-unique(WF_types_join2, by=c('DTM')) 
mer<-mer[CT%in%c('A', 'AN', 'ANE', 'AE', 'ASE', 'AS', 'ASW', 'AW', 'ANW') , CT2 := 'A']
mer<-mer[CT%in%c('C', 'CN', 'CNE', 'CE', 'CSE', 'CS', 'CSW', 'CW', 'CNW') , CT2 := 'C']
mer<-mer[CT%in%c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW') , CT2 := 'DIR']
mer<-mer[CT%in%c('U') , CT2 := 'U']
mer_unique<-unique(mer, by='DTM')
mer_unique<-mer_unique[, c("pred1", "pred2", "pred3", "po1", "po2", "po3") := shift(CT2, n = c(1,2,3, -1, -2, -3))] 

merging_Start<-mer_unique[DTM==Start,]
merging_End<-mer_unique[DTM==End,]

merging_End1<-merging_End[,after_Type:= (c(po1)), by=End]
merging_End2<-merging_End[,after_Type2:= (c(po2)), by=End]
merging_End3<-merging_End[,after_Type3:= (c(po3)), by=End]
merging_End<-rbind(merging_End1[, .(Type, CT2, after_Type)], merging_End2[, .(Type, CT2,after_Type2)], merging_End3[, .(Type, CT2, after_Type3)], use.names=FALSE)

tab_HWL<-merging_End[Type=='HWL',]
tab_HWH<-merging_End[Type=='HWH',]
tab_HWO<-merging_End[Type=='HWO',]
tab_HWG<-merging_End[Type=='HWG',]


####################### HWL table
tab_HWL<-tab_HWL[,count:=ave(after_Type, after_Type,FUN=length)] 
tab_HWL[after_Type=='A',]
tab_HWL<-unique(tab_HWL, by = "after_Type")
tab_HWL <- tab_HWL[ , count := as.numeric(count)]
tab_HWL <- tab_HWL[ , sum_count := sum(count)] 
tab_HWL<-tab_HWL[,prob:=round((count/sum_count)*100, 2)] 

####################### HWG table
tab_HWG<-tab_HWG[,count:=ave(after_Type, after_Type,FUN=length)]
tab_HWG[after_Type=='A',]
tab_HWG<-unique(tab_HWG, by = "after_Type")
tab_HWG <- tab_HWG[ , count := as.numeric(count)]
tab_HWG <- tab_HWG[ , sum_count := sum(count)] 
tab_HWG<-tab_HWG[,prob:=round((count/sum_count)*100, 2)]

####################### HWO table
tab_HWO<-tab_HWO[,count:=ave(after_Type, after_Type,FUN=length)] 
tab_HWO[after_Type=='A',]
tab_HWO<-unique(tab_HWO, by = "after_Type")
tab_HWO <- tab_HWO[ , count := as.numeric(count)]
tab_HWO <- tab_HWO[ , sum_count := sum(count)] 
tab_HWO<-tab_HWO[,prob:=round((count/sum_count)*100, 2)]

####################### HWH table
tab_HWH<-tab_HWH[,count:=ave(after_Type, after_Type,FUN=length)] 
tab_HWH[after_Type=='A',]
tab_HWH<-unique(tab_HWH, by = "after_Type")
tab_HWH <- tab_HWH[ , count := as.numeric(count)]
tab_HWH <- tab_HWH[ , sum_count := sum(count)] 
tab_HWH<-tab_HWH[,prob:=round((count/sum_count)*100, 2)] 

x <- data.table(after_Type=c('A', 'C', 'DIR', 'U'))
tab_HWH<-full_join(tab_HWH, x, by='after_Type')
tab_HWO<-full_join(tab_HWO, x, by='after_Type')
tab_HWL<-full_join(tab_HWL, x, by='after_Type')
tab_HWG<-full_join(tab_HWG, x, by='after_Type')

PROB <- readRDS("./data/prob_4.rds")

######### HWH
tab_HWH[,CT:=NULL]
setnames(tab_HWH, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]

tab_HWH_1<-tab_HWH

######### HWL
tab_HWL[,CT:=NULL]
setnames(tab_HWL, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]

tab_HWL_1<-tab_HWL

######### HWO
tab_HWO[,CT:=NULL]
setnames(tab_HWO, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]

tab_HWO_1<-tab_HWO

######### HWG
tab_HWG[,CT:=NULL]
setnames(tab_HWG, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'DIR', 'U'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]

tab_HWG_1<-tab_HWG


HW_TAB<-cbind(tab_HWG_1[,prob], tab_HWL_1[,prob], tab_HWH_1[,prob], tab_HWO_1[,prob]) 
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-as.data.table(HW_TAB)
HWG_BARPLOT_TAB<-HWG_BARPLOT_TAB[,PROB:=NULL]

setnames(HWG_BARPLOT_TAB, c("V1", "V2", "V3", "V4"), c("HWG", "HWL", "HWH", "HWO"))

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT)

pie_AFTER<-
  ggplot(transform(dat2,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+
  scale_fill_manual(name= "                   ",values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), labels = c(" anticyclonic"," cyclonic", " directional", " indeterminate"))+
  theme(legend.text=element_text(size=22))+
  theme(strip.text = element_blank())+
  theme(legend.text = element_text(margin = margin(r = 0.75, unit = 'in')))+ 
  guides(shape=guide_legend(title="New Legend Title"))+
  theme(legend.position = 'bottom', size=20)

common_theme <- theme_void() +
  theme(
    text = element_text(size = 25),
    legend.position = "none",
    plot.margin = margin(2, 10, 2, 10),
    strip.text = element_blank(),       
    strip.background = element_blank())

pie_BEFORE_mod <- pie_BEFORE + common_theme
pie_DURING_mod <- pie_DURING + common_theme
pie_AFTER_mod <- pie_AFTER + common_theme

pie_AFTER_legend <- pie_AFTER +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 22, margin = margin(r = 10, unit = "pt")),
    legend.key.width = unit(0.8, "cm"))+
  guides(fill = guide_legend(
      byrow = TRUE,
      ncol = 4))+
  theme(legend.text = element_text(margin = margin(r = 0.5, unit = 'in')))

legend <- get_legend(pie_AFTER_legend)

plots <- ggarrange(
  pie_BEFORE_mod, pie_DURING_mod, pie_AFTER_mod,
  ncol = 1,
  labels = c("a", "b", "c"),
  heights = c(1, 1, 1),
  align = "v",
  font.label = list(size = 25, face = "bold"),
  label.x = 0.00,
  label.y = 0.99)

final <- annotate_figure(
  ggarrange(plots, legend, ncol = 1, heights = c(9, 1)),
  top = text_grob("HWG                     HWL                     HWH                     HWO", size = 25, face = "bold")) 

#ggsave(final, file=paste0("./figures/","multiple_pie.png"), width = 30, height = 25, units = "cm", bg='white')


