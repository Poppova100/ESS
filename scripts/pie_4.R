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
WF_types_join2[DTM=='1979-07-25',]

mer<-unique(WF_types_join2, by=c('DTM'))
mer<-mer[CT%in%c('A') , CT2 := 'anticyclonic']
mer<-mer[CT%in%c('U') , CT2 := 'indeterminate']
mer<-mer[CT%in%c('S', 'CS', 'AS', 'ASE', 'CSE', 'SE', 'ASW', 'CSW', 'SW') , CT2 := 'southerly']
mer<-mer[CT%in%c('W', 'CW', 'AW', 'E', 'CE', 'AE', 'ANE', 'CNE', 'NE', 'N', 'CN', 'AN', 'ANW','CNW', 'NW', 'C') , CT2 := 'other']
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


x <- data.table(CT2=c("anticyclonic", "southerly", "indeterminate", "other"))
tab_HWH<-full_join(tab_HWH, x, by='CT2')
tab_HWO<-full_join(tab_HWO, x, by='CT2')
tab_HWL<-full_join(tab_HWL, x, by='CT2')
tab_HWG<-full_join(tab_HWG, x, by='CT2')


######### HWH
tab_HWH[,CT:=NULL]
setnames(tab_HWH, c('CT2'), c('CT'))

x <- make.unique(c("anticyclonic", "southerly", "indeterminate", "other"))
tab_HWH<-tab_HWH[match(x, make.unique(CT))]
tab_HWH<-tab_HWH %>% mutate_if(is.numeric, ~round(., 2))
tab_HWH[,count:=NULL]
tab_HWH[,sum_count:=NULL]

tab_HWH_1<-tab_HWH

######### HWL
tab_HWL[,CT:=NULL]
setnames(tab_HWL, c('CT2'), c('CT'))
x <- make.unique(c("anticyclonic", "southerly", "indeterminate", "other"))
tab_HWL<-tab_HWL[match(x, make.unique(CT))]
tab_HWL<-tab_HWL %>% mutate_if(is.numeric, ~round(., 2))
tab_HWL[,count:=NULL]
tab_HWL[,sum_count:=NULL]
tab_HWL_1<-tab_HWL

######### HWO
tab_HWO[,CT:=NULL]
setnames(tab_HWO, c('CT2'), c('CT'))
x <- make.unique(c("anticyclonic", "southerly", "indeterminate", "other"))
tab_HWO<-tab_HWO[match(x, make.unique(CT))]
tab_HWO<-tab_HWO %>% mutate_if(is.numeric, ~round(., 2))
tab_HWO[,count:=NULL]
tab_HWO[,sum_count:=NULL]
tab_HWO_1<-tab_HWO

######### HWG
tab_HWG[,CT:=NULL]
setnames(tab_HWG, c('CT2'), c('CT'))
x <- make.unique(c("anticyclonic", "southerly", "indeterminate", "other"))
tab_HWG<-tab_HWG[match(x, make.unique(CT))]
tab_HWG<-tab_HWG %>% mutate_if(is.numeric, ~round(., 2))
tab_HWG[,count:=NULL]
tab_HWG[,sum_count:=NULL]
tab_HWG_1<-tab_HWG

HW_TAB<-cbind(tab_HWG_1[,prob], tab_HWL_1[,prob], tab_HWH_1[,prob], tab_HWO_1[,prob])
HW_TAB<-as.data.frame(HW_TAB)

HWG_BARPLOT_TAB<-as.data.table(HW_TAB)
setnames(HWG_BARPLOT_TAB, c("V1", "V2", "V3", "V4"), c("HWG", "HWL", "HWH", "HWO"))
HWG_BARPLOT_TAB<-HWG_BARPLOT_TAB[,CT:=c(c("anticyclonic", "southerly", "indeterminate", "other"))]

dat2 <- HWG_BARPLOT_TAB %>%
  gather(Prob, Value, -CT)

pie_4<-
  ggplot(transform(dat2,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=4.5) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+ 
  scale_fill_manual(name= '                   ', 
                    values = c("anticyclonic"="#E41A1C", "southerly"="yellow" ,"indeterminate"="#FF7F00", "other"="purple1"), 
                    labels = c("anticyclonic"=" anticyclonic", "southerly"=" southerly", "indeterminate"=" indeterminate", "other"=" other") , 
                    breaks = c('anticyclonic', 'southerly', 'indeterminate', 'other'))+
  theme(legend.position = "bottom")+
  theme(legend.text=element_text(size=14))+
  theme(strip.text = element_text(size = 17, face = 'bold'))+
  theme(legend.text = element_text(margin = margin(r = 0.75, unit = 'in')))+ 
  theme(legend.position = 'bottom')

#ggsave(pie_4, file="./figures/pie_4.png", height = 10 , width = 25, units = "cm", bg = 'white')
