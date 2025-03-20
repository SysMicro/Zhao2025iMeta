library(data.table)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(raster)
library(colorspace)
library(vegan)
library(tidyverse)
library(ggsci)
library(stringr)
library(reshape2)
library(fitdistrplus)
library(sads)
library(tidyverse)

pal1 <- c("#C17360","#EAB88D","#207589","#525E80","#778B89","#DAC798","#7E7E7E")
pal2 <- c("#C17360","#009193","#EAB88D","#207589","#525E80","#DAC798","#7E7E7E")

sample_HA <- read.csv("sample_HA.csv")
sample_HA$LandcoverClass <- factor(sample_HA$LandcoverClass, 
                                    levels = c("Agricultural Land","Natural Land","Bare Land","Forest","Grassland",
                                               "Tundra","Wetland")[1:7])

ggplot(sample_HA, aes(x = LandcoverClass, y = HA))+
  geom_jitter(position = position_jitter(0.25), size = 2.5, alpha = 0.6, aes(color = LandcoverClass),shape=17)+
  geom_boxplot(alpha = 0, outlier.size = 0, size = 0.8, width = 0.7, outlier.colour = NA)+
   xlab("")+
  ylab("Risk index")+
  #ylim(0,7)+
  stat_summary(fun="mean",geom="point",shape=23,size=2.5,fill="white")+
  #scale_fill_npg()+
  scale_color_manual(values = pal2)+
  theme_bw()+
  theme(#plot.title = element_text(hjust = 0.05, vjust = -9, size = 10),
    axis.text.x = element_text( size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid = element_blank(),
    panel.border = element_rect(size=1,colour="black"),
    axis.title.y = element_text(size = 18),
    legend.position = "none",
    strip.background = element_rect(fill = "white",size = 1.0))+
  scale_y_log10("Risk Index")


ggsave("RI.pdf", 
       width=12, height=9, units="cm")



