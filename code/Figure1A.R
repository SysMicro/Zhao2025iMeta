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
library(reshape2)


# read shapefile
wmap <- readOGR(dsn="ne_110m_land", layer="ne_110m_land")

# convert to dataframe
wmap_df <- fortify(wmap)

# create a blank ggplot theme
theme_opts <-list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill="white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22,hjust = .5)))

# plot map
all <- read.csv("sample_all.csv", header = T)


ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "gray90") + 
  coord_equal() + 
  theme_opts+
  geom_point(data=all[which(all$all_len>90000000),],
             aes(lon, lat,size=log10(all_len),color=LandcoverClass, shape = Type),
             alpha=0.4,
             inherit.aes = FALSE)+
  scale_size_continuous(range = c(0,7.5))+
  scale_color_manual(values=c("#C17360","#EAB88D","#207589","#525E80","#778B89","#DAC798","#7E7E7E"))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  theme(legend.position = "none")+ 
  theme(axis.text.x=element_blank(),
        legend.position = "bottom")

ggsave("all.pdf", 
       width=36, height=18, units="cm")

detected <- read.csv("sample_detected.csv", header = T)

ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "gray90") + 
  coord_equal() + 
  theme_opts+
  geom_point(data=detected,
             aes(lon, lat,size=Richness,color=LandcoverClass, shape = Type),
             alpha=0.4,
             inherit.aes = FALSE)+
  scale_size_continuous(range = c(5,12))+
  scale_color_manual(values=c("#C17360","#EAB88D","#207589","#525E80","#778B89","#DAC798","#7E7E7E"))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  theme(legend.position = "none")+ 
  theme(axis.text.x=element_blank(),
        legend.position = "bottom")

ggsave("detected_rich.pdf", 
       width=36, height=18, units="cm")

ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "gray90") + 
  coord_equal() + 
  theme_opts+
  geom_point(data=detected,
             aes(lon, lat,size=log10(AI),color=LandcoverClass, shape = Type),
             alpha=0.4,
             inherit.aes = FALSE)+
  scale_size_continuous(range = c(1,10))+
  scale_color_manual(values=c("#C17360","#EAB88D","#207589","#525E80","#778B89","#DAC798","#7E7E7E"))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  theme(legend.position = "none")+ 
  theme(axis.text.x=element_blank(),
        legend.position = "bottom")

ggsave("detected_AI.pdf", 
       width=36, height=18, units="cm")
