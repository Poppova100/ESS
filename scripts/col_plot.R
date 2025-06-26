lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'grid')
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
WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']

mer<-unique(WF_types_join2, by=c('DTM')) 
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
mer<-mer[CT%in%c('U') , CT2 := 'IF']
mer_unique<-unique(mer, by='DTM')
mer_unique<-mer_unique[, c("pred1", "pred2", "pred3", "po1", "po2", "po3") := shift(CT2, n = c(1,2,3, -1, -2, -3))]

mer_unique[DTM=='1980-07-21',]
mer_unique[DTM=='1980-07-22',]
mer_unique[DTM=='1980-07-23',]
mer_unique[DTM=='1980-07-24',]

mer_unique[DTM=='1980-07-27',]
mer_unique[DTM=='1980-07-28',]
mer_unique[DTM=='1980-07-29',]

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

x <- data.table(before_Type=c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-full_join(tab_HWH, x, by='before_Type')
tab_HWO<-full_join(tab_HWO, x, by='before_Type')
tab_HWL<-full_join(tab_HWL, x, by='before_Type')
tab_HWG<-full_join(tab_HWG, x, by='before_Type')

PROB <- readRDS("./data/prob_11.rds")
PROB<-PROB[,CT:=c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')]

######### HWH
setnames(tab_HWH, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,CT2:=NULL]
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]

tab_HWH_1<-tab_HWH

######### HWL
setnames(tab_HWL, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,CT2:=NULL]
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]

tab_HWL_1<-tab_HWL

######### HWO
setnames(tab_HWO, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,CT2:=NULL]
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]

tab_HWO_1<-tab_HWO

######### HWG
setnames(tab_HWG, c('before_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
library(dplyr)
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,CT2:=NULL]
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]

tab_HWG_1<-tab_HWG

HW_TAB<-cbind(tab_HWG_1, tab_HWL_1, tab_HWH_1, tab_HWO_1)
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-HW_TAB[,c(2, 3, 6, 9, 12, 14)] 
setnames(HWG_BARPLOT_TAB, c("prob", "prob.1", "prob.2", "prob.3", "PROB"), c("HWG", "HWL", "HWH", "HWO", "June-Sep."))

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT)

dat2$Prob <- factor(dat2$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep."))
dat2$CT <- factor(dat2$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c(1.6, 1.8, 2.2,  2.8, 3.2, 3.6, 3.8, 4.0, 4.2,  6.0, 6.2,6.6, 6.8, 7.0, 8.6), y=rep(0, 15))

col_plot_11CT_BEFORE<-
  ggplot(dat2, aes(x = CT, y = Value, fill = Prob)) +
  geom_col(position = "dodge")+
  theme_bw()+
  ylim(0, 80)+
  scale_y_continuous(limits = c(0, 66.7), breaks = c(10, 20, 30, 40, 50, 60, 70))+
  theme(panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5), color='grey')+
  labs(y= " ", size=27)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=17, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=27))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.text=element_text(size=20))+
    geom_point(data = added_CT, mapping = aes(x = x, y = y,  fill='0 records'), shape=4, size=5, color='black')+
    scale_fill_manual(name="",
                      values = c("HWG" = "#E41A1C", "HWL" = "#377EB8",  "HWH" = "#4DAF4A", "HWO" = "#FF7F00", "June-Sep."= "#999999", "0 records"="white"),
                      breaks = c("HWG", "HWL", "HWH", "HWO", "June-Sep."))+
  theme(legend.position = c(.88,.75)) 
    
############################
lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr','dplyr', 'ggnewscale', 'RColorBrewer')
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
mer<-mer[CT%in%c('U') , CT2 := 'IF']
mer[CT2=='IF',]
mer_unique<-na.omit(mer, by='ID') 

tab_HWL<-mer_unique[Type=='HWL',]
tab_HWH<-mer_unique[Type=='HWH',]
tab_HWO<-mer_unique[Type=='HWO',]
tab_HWG<-mer_unique[Type=='HWG',]

####################### HWL table

tab_HWL<-tab_HWL[,count:=ave(CT2, CT2,FUN=length)] 
tab_HWL[CT2=='A',]
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

x <- data.table(CT2=c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-full_join(tab_HWH, x, by='CT2')
tab_HWO<-full_join(tab_HWO, x, by='CT2')
tab_HWL<-full_join(tab_HWL, x, by='CT2')
tab_HWG<-full_join(tab_HWG, x, by='CT2')

PROB <- readRDS("./data/prob_11.rds")

######### HWH
tab_HWH[,CT:=NULL]
setnames(tab_HWH, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]
tab_HWH_1<-tab_HWH

######### HWL
tab_HWL[,CT:=NULL]
setnames(tab_HWL, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]
tab_HWL_1<-tab_HWL

######### HWO
tab_HWO[,CT:=NULL]
setnames(tab_HWO, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]
tab_HWO_1<-tab_HWO

######### HWG
tab_HWG[,CT:=NULL]
setnames(tab_HWG, c('CT2'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]
tab_HWG_1<-tab_HWG

HW_TAB<-cbind(tab_HWG_1[,prob], tab_HWL_1[,prob], tab_HWH_1[,prob], tab_HWO_1[,prob]) 
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-HW_TAB
setnames(HWG_BARPLOT_TAB, c("V1", "V2", "V3", "V4", "PROB"), c("HWG", "HWL", "HWH", "HWO", "June-Sep."))

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT) 

dat2$Prob <- factor(dat2$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep.")) 
dat2$CT <- factor(dat2$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c(2.6, 3.8, 4.2, 5, 5.2, 8.6), y=rep(0, 6))

col_plot_11CT_DURING<-
  ggplot(dat2, aes(x = CT, y = Value, fill = Prob)) +
  geom_col(position = "dodge")+
  theme_bw()+
  ylim(0, 80)+
  scale_y_continuous(limits = c(0, 66.7), breaks = c(10, 20, 30, 40, 50, 60, 70))+
  theme(panel.grid.major.x = element_blank()) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5), color='grey')+
  labs(y= " ", size=27)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=17, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=27))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.text=element_text(size=20))+
  geom_point(data = added_CT, mapping = aes(x = x, y = y,  fill='0 records'), shape=4, size=5, color='black')+
  scale_fill_manual(name="",
                    values = c("HWG" = "#E41A1C", "HWL" = "#377EB8",  "HWH" = "#4DAF4A", "HWO" = "#FF7F00", "June-Sep."= "#999999", "0 records"="white"),
                    breaks = c("HWG", "HWL", "HWH", "HWO", "June-Sep."))+
  theme(legend.position = "none")


##################
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

WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM']
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']

mer<-unique(WF_types_join2, by=c('DTM'))
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
mer<-mer[CT%in%c('U') , CT2 := 'IF']
mer_unique<-unique(mer, by='DTM')
mer_unique<-mer_unique[, c("pred1", "pred2", "pred3", "po1", "po2", "po3") := shift(CT2, n = c(1,2,3, -1, -2, -3))] 

mer_unique[DTM=='1980-07-21',]
mer_unique[DTM=='1980-07-22',]
mer_unique[DTM=='1980-07-23',]
mer_unique[DTM=='1980-07-24',]

mer_unique[DTM=='1980-07-27',]
mer_unique[DTM=='1980-07-28',]
mer_unique[DTM=='1980-07-29',]

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

x <- data.table(after_Type=c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-full_join(tab_HWH, x, by='after_Type')
tab_HWO<-full_join(tab_HWO, x, by='after_Type')
tab_HWL<-full_join(tab_HWL, x, by='after_Type')
tab_HWG<-full_join(tab_HWG, x, by='after_Type')

PROB <- readRDS("./data/prob_11.rds")

######### HWH
setnames(tab_HWH, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,CT2:=NULL]
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]
tab_HWH_1<-tab_HWH

######### HWL
setnames(tab_HWL, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,CT2:=NULL]
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]
tab_HWL_1<-tab_HWL

######### HWO
setnames(tab_HWO, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,CT2:=NULL]
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]
tab_HWO_1<-tab_HWO

######### HWG
setnames(tab_HWG, c('after_Type'), c('CT'))
x <- make.unique(c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF'))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,CT2:=NULL]
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]
tab_HWG_1<-tab_HWG
HW_TAB<-cbind(tab_HWG_1, tab_HWL_1, tab_HWH_1, tab_HWO_1)
HW_TAB<-as.data.frame(HW_TAB)
HW_TAB<-cbind(HW_TAB,PROB[,1:2])

HWG_BARPLOT_TAB<-HW_TAB[,c(2, 3, 6, 9, 12, 14)] 
setnames(HWG_BARPLOT_TAB, c("prob", "prob.1", "prob.2", "prob.3", "PROB"), c("HWG", "HWL", "HWH", "HWO", "June-Sep."))

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT) 

dat2$Prob <- factor(dat2$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep.")) 
dat2$CT <- factor(dat2$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c( 3.8,  4.2, 4.8), y=rep(0, 3))

col_plot_11CT_AFTER<-
  ggplot(dat2, aes(x = CT, y = Value, fill = Prob)) +
  geom_col(position = "dodge")+
  theme_bw()+
  ylim(0, 80)+
  scale_y_continuous(limits = c(0, 66.7), breaks = c(10, 20, 30, 40, 50, 60, 70))+
  theme(panel.grid.major.x = element_blank()) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5), color='grey')+
  labs(y= " ", size=27)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=17, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=27))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.text=element_text(size=20))+
    geom_point(data = added_CT, mapping = aes(x = x, y = y,  fill='0 records'), shape=4, size=5, color='black')+
  scale_fill_manual(name="",
                    values = c("HWG" = "#E41A1C", "HWL" = "#377EB8",  "HWH" = "#4DAF4A", "HWO" = "#FF7F00", "June-Sep."= "#999999", "0 records"="white"),
                    breaks = c("HWG", "HWL", "HWH", "HWO", "June-Sep."))+
  theme(legend.position = "none")

arrange<-ggarrange(col_plot_11CT_BEFORE+theme(legend.key.width = unit(2, 'cm'))+
                     theme(axis.text.x = element_blank(),),
                   col_plot_11CT_DURING+theme(legend.key.width = unit(2, 'cm'))+
                     theme(axis.text.x = element_blank(),), 
                   col_plot_11CT_AFTER+theme(legend.key.width = unit(2, 'cm')),
                   nrow = 3, ncol=1,  heights = c(1, 1.0, 1.09), labels = c("a", "b", "c"),font.label = list(size = 25, color = "black"))+ 
  guides(colour = guide_legend(override.aes = list(size=5)))
arrange<-annotate_figure(arrange,left = text_grob("Frequency [%]", color = "black", rot = 90, size=25))
#ggsave(arrange, file=paste0("./figures/","col_plot.png"), width = 30, height = 35, units = "cm", bg='white')
