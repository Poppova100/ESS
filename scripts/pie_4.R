lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dta_4_CTs <- readRDS("./data/dta_4_CTs.rds")

pie_4<-
  ggplot(transform(dta_4_CTs,
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
