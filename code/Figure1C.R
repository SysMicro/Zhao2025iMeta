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


taxa <- read.csv("taxa_vert.csv")
datatax <- read.csv("sample_count_size.csv")

require(ggfortify)
require(lamW)
require(GGally)
require(ggcorrplot)
require(pracma)

### LOAD STATIC DATA
nreads_cutoff <- 10^-3
count_cutoff <- 0

summarydata <- datatax %>% filter(nreads > nreads_cutoff ) %>%  group_by(project_id, classification) %>%
  summarise( n_of_runs = n_distinct(run_id), mean_nreads = mean(nreads)) %>%  ungroup() %>% 
  as.data.frame() %>%  filter( n_of_runs > 00  ) %>% mutate(idall = paste(project_id, classification))
datatax <- datatax %>% filter( paste(project_id, classification) %in% paste(summarydata$project_id,summarydata$classification  )  )
summarydata
#names <- c("Agricultural Land", "Bare Land", "Grassland", "Forest", "Wetland", "Tundra","Shrubland")
#scat <- c( "Agricultural Land", "Natural Land", "Natural Land", "Natural Land", "Natural Land", "Natural Land", "Natural Land")
names <- c("soil")
scat <- c( "soil")
shortnames <- as.data.frame(list( idall = summarydata$idall, sname = names, scat = scat  ))
proj <- datatax %>% filter(  nreads > nreads_cutoff, count > count_cutoff ) %>% 
  group_by( project_id, classification, otu_id ) %>% mutate( tf = mean(count/nreads), o = n(), tvpf = mean( (count^2 - count)/nreads^2 ) ) %>% ungroup() %>%
  group_by( project_id, classification ) %>%  mutate(o = o / n_distinct(run_id) ) %>% 
  mutate( f = o*tf, vf = o*tvpf ) %>% mutate(vf = vf - f^2 ) %>%  ungroup() %>% 
  mutate(idall = paste(project_id, classification)) %>% select( -tf, -tvpf )


############ PARAMETERS OF THE GAMMA DISTRIBUTION
gamma_pars <- proj %>% select( idall, sname=classification, otu_id, o, f, vf ) %>% 
  mutate( cv = sqrt(vf/f^2) ) %>% distinct() %>% 
  mutate( beta = 1./cv^2, theta = f/beta )


gamma_pars %>%  ggplot() + mytheme +
  aes(
    x = f,
    y = vf
  ) + geom_point( alpha = 0.5 ) + #facet_wrap(  ~ sname ) +
  scale_x_log10() + scale_y_log10()


gamma_pars %>% filter(f > 2*10^-6) %>% group_by(sname) %>% summarise( cor(beta, log(f)) ) %>%  ungroup()
### think about this correlation (sampling) and variability of cv/beta (could be able to say something from the model)


############## LOGNORMAL PARAMETERS
#nbin <- 20
#cutoffs <- list( sname = c("Agricultural Land", "Bare Land", "Grassland", "Forest", "Wetland", "Tundra","Shrubland"),
#                 c = c(-18.8, -17, -15, -17.50000, -16.25, -17.9, -14.26616) ) %>% as.data.frame()
#cutoffs <- list( sname = c("soil"),
#                 c = c(-20) ) %>% as.data.frame()

fun_erf <- function( mu, c, m1, m2  ){
  sigma <- sqrt(-c*m1 + m2 + c*mu - m1*mu )
  x <- (c-mu)/sigma/sqrt(2.)
  f <- (mu-m1)*erfc(x) + exp(- x^2) * sqrt(2/pi) * sigma 
  return(f)
}

estimatemean_f <- function( c, m1, m2 ){
  mumin <- (c*m1 - m2)/(c - m1)
  muest <- uniroot(fun_erf, c = c, m1 = m1, m2 = m2, interval = c(-50,mumin-0.001), tol = 0.0001 )$root
  return( muest )
}

estimate_mean <- gamma_pars %>% 
  mutate(lf = log(f)) %>%
  left_join(cutoffs) %>%  ungroup() %>% filter( lf > c ) %>% 
  group_by(idall, sname) %>% 
  summarise( c = mean(c),  m1 = mean(lf) , m2 = mean(lf^2), ns_obs = n_distinct(otu_id), nf = sum(f)  )  %>%   ungroup() %>% rowwise() %>% 
  mutate( mu = estimatemean_f(c,m1,m2) ) %>% mutate( sigma = sqrt(-c*m1 + m2 + c*mu - m1*mu ) ) %>% 
  ungroup() %>%  as.data.frame() %>% mutate( stot = 2*ns_obs / erfc( (c-mu)/sigma/sqrt(2)  ) ) # , np_tot_r = 1./exp(mu+sigma^2/2.)/nf )


mean_pars <- estimate_mean %>% select( idall, sname, nf, c, mu, sigma, stot  ) %>% left_join(
  gamma_pars %>% filter(f > 10^-5, vf > 0 ) %>% group_by(sname) %>% summarise( mbeta = mean(beta) ) %>%  ungroup()
)


lm.pocc <-  proj %>% left_join(gamma_pars) %>% left_join(mean_pars ) %>%
  mutate( beta = ifelse(is.na(beta), mbeta, beta)  ) %>% mutate( theta = f/beta  ) %>%  
  group_by(idall, sname, otu_id) %>% 
  summarize( pocc = 1- mean((1.+theta*nreads)^(-beta ) ), o = mean(o)  ) %>% ungroup()



taxa2 <- data.frame(otu_id=taxa$Subject_id,Sum=taxa$Sum,Host=taxa$Host)
lm.pocc2 <- data.frame(otu_id = lm.pocc$otu_id, o = lm.pocc$o)
ai_occ <- left_join(lm.pocc2, taxa2, by = c("otu_id"))

pal <- c("#C17360","#EAB88D","#207589","#525E80","#778B89","#7E7E7E")
ai_occ$Host <- factor(ai_occ$Host, levels = c("Birds","Human","Human_and_other_vertebrates","Non_human_primates","Ungulata","Others")[1:6])

ggplot(ai_occ, aes(y = Sum, x = o,color=Host))+
  geom_point(size = 3, alpha =0.75, shape =17) + #c("#C17360","#6BB9D2","#009193")
  stat_smooth(method = 'lm', colour = "black", linetype =2) +
   #labs(title=expression(''~ R^2 ~' = 0.788, p < 0.001'))+
  scale_y_log10( "Abundance" ) +
  scale_x_log10( "Occupancy" )+
  theme_bw()+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
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

lm.ai_occ  <- lm(log10(ai_occ$Sum)~log10(ai_occ$o))
summary(lm.ai_occ)

ggsave("oAI_occ.pdf", 
       width=10, height=8, units="cm")






