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
library(tidyr)
library(ggalluvial)
library(stringr)
library(reshape2)
library(fitdistrplus)
library(sads)



sample_vert_present_rank3 <- read.csv("data/sample_detected.csv")
sample_vert_present_rank3$Land <- factor(sample_vert_present_rank3$Land, 
                                         levels = c("Agricultural","Natural")[1:2])

pal3 <- c("#d57360","#009193")

ggplot(sample_vert_present_rank3, aes(y = AI, x = Richness, color = Land, fill=Land))+
  geom_point(size = 2.5, alpha =0.6,shape=17) + 
  #stat_smooth(method = 'lm', colour = "black", linetype =2) +
  stat_smooth(method = 'lm', linetype =2, alpha=0.15) +
  # labs(title=expression(''~ R^2 ~' = 0.616, p < 0.001'))+
  scale_y_log10( "Abundance Index" ) +
  scale_x_log10( "Richness" )+
  #facet_grid(LandcoverClass~.)+
  theme_bw()+
  scale_colour_manual(values = pal3)+
  scale_fill_manual(values=pal3)+
  #stat_summary_bin(fun.y = "mean", geom = "point", color = "#C75E48", shape = 3, size = 4, stroke = 2, bins = 10 ) +
  theme(plot.title = element_text(hjust = 0.07, vjust = -11, size = 12),
        panel.grid = element_blank(),
        #legend.text = element_text(size = 12),
        legend.position = "none",
        #legend.background = element_rect(size = 0.5, color = "black"),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(size=1.0,colour="black"), 
        strip.background = element_rect(fill = "white",size = 1.0))

ggsave("abun_rich2.pdf", 
       width=10, height=8.5, units="cm")


fit.all <- lm(log10(sample_vert_present_rank3$AI) ~ log10(sample_vert_present_rank3$Richness))
summary(fit.all)

fit.agri <- lm(log10(sample_vert_present_rank3[which(sample_vert_present_rank3$Land == "Agricultural"),]$AI) ~ log10(sample_vert_present_rank3[which(sample_vert_present_rank3$Land == "Agricultural"),]$Richness))
summary(fit.agri)

fit.nat <- lm(log10(sample_vert_present_rank3[which(sample_vert_present_rank3$Land == "Natural"),]$AI) ~ log10(sample_vert_present_rank3[which(sample_vert_present_rank3$Land == "Natural"),]$Richness))
summary(fit.nat)



sample_vert_present_rank3$Land <- factor(sample_vert_present_rank3$Land, 
                                         levels = c("Agricultural","Natural")[2:1])
ggplot(sample_vert_present_rank3,aes((AI),colour=Land,fill=Land))+
  geom_density(adjust=1.5,alpha=0.85, size = 0)+
  theme_bw() + theme(panel.grid = element_blank(),
                     panel.border = element_rect(size=1,colour="black"), 
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     #  axis.text.y=element_text(angle = 90,hjust = 0.5),
                     legend.position="none")+
  #scale_y_continuous(labels=scaleFUN)+
  #scale_fill_npg()+
  #scale_color_npg()+
  scale_fill_manual(values = pal3)+
  scale_color_manual(values = pal3)+
  coord_flip()+
  scale_x_log10("Abundance index")

ggsave("Agri_Nat_ai.pdf", 
       width=4, height=7.4, units="cm")

ggplot(sample_vert_present_rank3,aes(Richness,colour=Land,fill=Land))+
  geom_density(adjust=1.5,alpha=0.85, size = 0)+
  theme_bw() + theme(panel.grid = element_blank(),
                     panel.border = element_rect(size=1,colour="black"), 
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     #  axis.text.y=element_text(angle = 90,hjust = 0.5),
                     legend.position="none")+
  #scale_y_continuous(labels=scaleFUN)+
  #scale_fill_npg()+
  #scale_color_npg()+
  scale_fill_manual(values = pal3)+
  scale_color_manual(values = pal3)+
  #coord_flip()+
  scale_x_log10("Richness")
ggsave("Agri_Nat_rich.pdf", 
       width=8.9, height=2.7, units="cm")

