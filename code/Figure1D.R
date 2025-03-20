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

otu_vert <- data.frame(t(read.csv("sample_virus_ai_vert.csv", row.names = 1)))

otu_vert_var <- NULL
otu_vert_mean <- NULL
for (i in 1:ncol(otu_vert)) {
  otu_vert_var <- c(otu_vert_var,var(otu_vert[,i]))
  otu_vert_mean <- c(otu_vert_mean,mean(otu_vert[,i]))
}

otu_vert_var_mean <- data.frame(cbind(Subject_id=colnames(otu_vert), var=otu_vert_var, mean=otu_vert_mean))
otu_vert_var_mean <- left_join(otu_vert_var_mean,taxa_vert,by="Subject_id")
otu_vert_var_mean$var <- as.numeric(otu_vert_var_mean$var)
otu_vert_var_mean$mean <- as.numeric(otu_vert_var_mean$mean)
fit.var.mean <- lm(log10(otu_vert_var_mean$var) ~ log10(otu_vert_var_mean$mean))
summary(fit.var.mean)

pal <- c("#C17360","#EAB88D","#207589","#525E80","#778B89","#7E7E7E")
otu_vert_var_mean$Host <- factor(otu_vert_var_mean$Host, levels = c("Birds","Human","Human_and_other_vertebrates","Non_human_primates","Ungulata","Others")[1:6])
write.csv(otu_vert_var_mean,"otu_vert_var_mean.csv")
otu_vert_var_mean <- read.csv("otu_vert_var_mean.csv", header = T)
ggplot(otu_vert_var_mean, aes(x = (mean), y = (var), color = Host))+
  geom_point(size = 3, alpha =0.75,shape=17) + 
  stat_smooth(method = 'lm', color="black", linetype =2) +
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  #labs(title=expression(''~ R^2 ~' = 0.9899 ***'))+
  xlab("Average abundance index")+
  ylab("Variance of abundance index")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.07, vjust = -11, size = 12),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(size=1.0,colour="black"), 
        strip.background = element_rect(fill = "white",size = 1.0)) +
  scale_y_log10( "Variance of abundance index" ) +
  scale_x_log10( "Average abundance index" )

ggsave("taylor.pdf", 
       width=10, height=8, units="cm")
