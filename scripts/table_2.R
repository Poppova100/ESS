lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

tab_JS <- readRDS("./data/tab_June_Sep_11.rds")
tab_JS[, `:=`(CT = CT2, CT2 = NULL)]

tab_HW <- readRDS("./data/HW_TAB_11.rds")
klima <- readRDS("./data/klima.rds")

TABLE_2<-join(tab_HW, tab_JS,by = "CT")
TABLE_2<-TABLE_2[,c('pet_p_HWG', 'pet_p_HWL', 'pet_p_HWH', 'pet_p_HWO', 'pet_p_JS'):=NULL]
setcolorder(TABLE_2, c('CT', 'prob_HWG', 'Cef_HWG', 'tg_HWG', 'p_HWG', 'prob_HWL', 'Cef_HWL', 'tg_HWL', 'p_HWL',
                       'prob_HWH', 'Cef_HWH', 'tg_HWH', 'p_HWH', 'prob_HWO', 'Cef_HWO', 'tg_HWO', 'p_HWO', 'prob_JS', 'tg_JS', 'p_JS'))

TABLE_2<-TABLE_2[, tg_JS:=tg_JS-klima]
TABLE_2<-TABLE_2 %>% mutate_if(is.numeric, ~round(., 2))
#saveRDS(TABLE_2, "./data/table_2.rds")
