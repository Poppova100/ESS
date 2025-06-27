lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'grid')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dat2_prob_before <- readRDS("./data/dat2_prob_before.rds")

dat2_prob_before$Prob <- factor(dat2_prob_before$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep."))
dat2_prob_before$CT <- factor(dat2_prob_before$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c(1.6, 1.8, 2.2,  2.8, 3.2, 3.6, 3.8, 4.0, 4.2,  6.0, 6.2,6.6, 6.8, 7.0, 8.6), y=rep(0, 15))

col_plot_11CT_BEFORE<-
  ggplot(dat2_prob_before, aes(x = CT, y = Value, fill = Prob)) +
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
    
dat2_prob_during <- readRDS("./data/dat2_prob_during.rds")

dat2_prob_during$Prob <- factor(dat2_prob_during$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep.")) 
dat2_prob_during$CT <- factor(dat2_prob_during$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c(2.6, 3.8, 4.2, 5, 5.2, 8.6), y=rep(0, 6))

col_plot_11CT_DURING<-
  ggplot(dat2_prob_during, aes(x = CT, y = Value, fill = Prob)) +
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

dat2_prob_after <- readRDS("./data/dat2_prob_after.rds")

dat2_prob_after$Prob <- factor(dat2_prob_after$Prob, levels = c("HWG", "HWL", "HWH", "HWO", "June-Sep.")) 
dat2_prob_after$CT <- factor(dat2_prob_after$CT, levels = c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')) 
cols <- c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00", "#999999" , "white") 
added_CT<-data.table(x=c( 3.8,  4.2, 4.8), y=rep(0, 3))

col_plot_11CT_AFTER<-
  ggplot(dat2_prob_after, aes(x = CT, y = Value, fill = Prob)) +
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

###
arrange<-ggarrange(col_plot_11CT_BEFORE+theme(legend.key.width = unit(2, 'cm'))+
                     theme(axis.text.x = element_blank(),),
                   col_plot_11CT_DURING+theme(legend.key.width = unit(2, 'cm'))+
                     theme(axis.text.x = element_blank(),), 
                   col_plot_11CT_AFTER+theme(legend.key.width = unit(2, 'cm')),
                   nrow = 3, ncol=1,  heights = c(1, 1.0, 1.09), labels = c("a", "b", "c"),font.label = list(size = 25, color = "black"))+ 
  guides(colour = guide_legend(override.aes = list(size=5)))
arrange<-annotate_figure(arrange,left = text_grob("Frequency [%]", color = "black", rot = 90, size=25))
#ggsave(arrange, file=paste0("./figures/","col_plot.png"), width = 30, height = 35, units = "cm", bg='white')
