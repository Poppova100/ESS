lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'gridExtra', 'grid')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dat_before <- readRDS("./data/dat_before.rds")
dat_during <- readRDS("./data/dat_during.rds")
dat_after <- readRDS("./data/dat_after.rds")


pie_BEFORE<-
  ggplot(transform(dat_before,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+ 
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), 
                    labels = c("anticyclonic", "cyclonic", "directional", "indeterminate"))+
  theme(strip.text = element_text(size = 25, margin = margin(), face = 'bold'))+
  theme(legend.position = "none")

pie_DURING<-
  ggplot(transform(dat_during,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+ 
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), 
                    labels = c("anticyclonic", "cyclonic", "directional", "indeterminate"))+
  theme(strip.text = element_blank())+ 
  theme(legend.position = "none")

pie_AFTER<-
  ggplot(transform(dat_after,
                   Prob=factor(Prob,levels=c("HWG", "HWL", "HWH", "HWO"))), aes(x = "", y = Value, fill = CT)) + 
  geom_col(color = "black") +
  geom_text( aes(x=1.23,label=paste0(sprintf("%.1f",Value))),
             position = position_stack( vjust = 0.5) , color = 'black', angle=0, size=6) +
  coord_polar(theta = "y") +
  theme_void()+
  facet_wrap(~ Prob, ncol = 5)+
  scale_fill_manual(name= "                   ",values = c("#E41A1C", "#377EB8", "#4DAF4A" ,"#FF7F00"), 
                    labels = c(" anticyclonic"," cyclonic", " directional", " indeterminate"))+
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


