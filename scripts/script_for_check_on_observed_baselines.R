

######################################################
# Project : CHECKS:: Drivers of food chain length across freshwater ecosystems 
# Script: Check for the potential spurrious correlstaions due to the use of predicted baselines
# Date: 2025-07-09
# Version: 1.0
# Description: This script performs various analyses related:
# - to computing FCL from observed baselines
# - to testing FCL univariate relationships to environmental descriptors,
# - to testing FCL relationships to Ecosystem size X Climate zones,
# - to testing the relationships underpinning the hypothesis of Ecosystem size X Climate zones on FCL
# ==> but using FCL computed from observed baselines rather than estimated
# The scripts also provides figures for the results.
# Purpose: checking that our correlations on FCL from predicted baselines is not spurrious
# Author: Marie-Elodie Perga, marie-elodie.perga@unil.ch
######################################################

######################################################
# 1. SET UP -------
######################################################
##  Set the working directory to the root of the project ------
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'/scripts')
figures.dir = paste0(root.dir,'/figures')

setwd(script.dir)

## libraries
library(dplyr)
library(ggplot2)
library(segmented)
library(forcats)
library(sjPlot)
library(ggpubr)
library(rprojroot)
library(cowplot)


## fixing colors for climate zones 
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")

## datasets
### to load and to fix
load("~/Documents/R/CESAB_paperB_v5/databases and wranglings/DATA_ISO_BASELINE.RData")
load("~/Documents/R/CESAB_paperB_v5/databases and wranglings/DATA_ISO_FISH.RData")
load("/Users/mperga/Documents/R/FoodWeb_Environmental_drivers-main/data/Env.RData")
colnames(Env)[1]<-"Food.web_ID"

corr_site<-read.csv("/Users/mperga/Documents/R/FoodWeb_Environmental_drivers-main/data/corr_sites.csv",header = TRUE,sep = ";")
df_corr<-data.frame(FW_ID=corr_site$Old_FW_ID,
                    "Food web_ID"=corr_site$FoodWeb_ID)

### to wrangle to get TP at the species scale (optimizing number of sites)
df_observed <- DATA_ISO_BASELINE %>%
  group_by(FW_ID, Resource_trophic_group) %>%
  summarize(mean_d15N_baseline = mean(d15N_baseline, na.rm = TRUE),.groups = "drop")%>%
  left_join(df_corr, by = "FW_ID" ) %>%
  left_join(DATA_ISO_FISH, by = "FW_ID" ) 


View(df_observed)

  df_observed$TP= ifelse(df_observed$Resource_trophic_group.x == "Primary producer", 
                         1+(df_observed$d15N-df_observed$mean_d15N_baseline)/3.4,
                         2+(df_observed$d15N-df_observed$mean_d15N_baseline)/3.4)


View(df_observed$TP)

### to wrangle to get FCL (optimizing number of sites)
df_FCL <- df_observed %>%
  group_by(Food.web_ID) %>%
  summarize(FCL = max(TP, na.rm = TRUE), .groups = "drop") %>% 
  left_join(Env,by="Food.web_ID") %>% 
  filter(is.finite(FCL))%>% filter(
    FCL >=2,
    FCL<=8,
    TP>=0,
    !is.na(Type))

View(df_FCL)
 colnames(df_FCL)

 df<-df_FCL
################################################
# 2. UNIVARIATE MODELS FOR FCL ---------
################################################
## 2.1 Ecosystem Type  hypothesis ---------

### Model ---------
mod_Ecosystem<-lm(FCL ~ Type ,df)
anova(mod_Ecosystem)
summary(mod_Ecosystem)
tab_model(mod_Ecosystem)

par(mfrow=c(2,2))
plot(mod_Ecosystem)

r2<- round(summary(mod_Ecosystem)$r.squared,digits = 3)
p<- round(anova(mod_Ecosystem)$`Pr(>F)`[1],digits=6)

### Plot ---------
G_Ecosystem<-ggplot(df,aes(y=FCL,x=Type))+
  geom_boxplot(alpha=0.8,col="darkgrey")+
  ggtitle("Ecosystem hypothesis")+
  labs(x="Ecosystem Type",y="FCL")+
  theme_classic()+
  annotate("text", x = 2.3, y = 6, 
           label = paste(expression("p="),p), 
           color = "black", size = 5)+
  theme(plot.title = element_text(face = "bold"))
G_Ecosystem

## 2.2 Productivity hypothesis ---------
### Model ------
mod_prodspace<-lm(FCL ~ poly(TP, 2, raw = TRUE) ,df)
anova(mod_prodspace)
summary(mod_prodspace)
tab_model(mod_prodspace)

par(mfrow=c(2,2))
plot(mod_prodspace)

r2<- round(summary(mod_prodspace)$r.squared,digits = 3)
X<-summary(mod_prodspace)
p<- round(X$coefficients[3,4],digits=3)

### Plot ------
my.formula_TP=y ~ poly(x, 2, raw = TRUE) 

G_prodspace<-ggplot(df,aes(y=FCL,x=TP))+
  geom_point(alpha=0.35,col="coral2")+
 #   geom_smooth(method = "lm",se = TRUE, 
   #            formula = my.formula_TP,col="coral2")+
  ggtitle("Productivity hypothesis")+
  labs(x=expression(paste("TP (log-scale [kg ", ha^-1, yr^-1,"])")),y="FCL")+
  theme_classic()+
  annotate("text", x = 6, y = 6, 
           label = paste("p=",p), 
           color = "black", size = 5)+
  theme(plot.title = element_text(face = "bold"))
G_prodspace

## 2.3 Disturbance hypothesis ---------
### Model ------
mod_disturbance<-lm(FCL ~ hydro_dis_z_scored,df)
summary(mod_disturbance)
anova(mod_disturbance)
tab_model(mod_disturbance)

r2<- round(summary(mod_disturbance)$r.squared,digits = 3)
p<- round(anova(mod_disturbance)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_disturbance)

### Plot ------
G_disturbance<-ggplot(df,aes(y=FCL,x=hydro_dis_z_scored))+
  geom_point(alpha=0.35,col="goldenrod2")+
  ggtitle("Hydrological disturbance hypothesis")+
  labs(x="Hydrological disturbance (log-scale [z-scored])",y="FCL")+
  theme_classic()+
  annotate("text", x = 2, y = 6, 
           label = paste("p=",p), 
           color = "black", size = 5)+
  theme(plot.title = element_text(face = "bold"))
G_disturbance

## 2.4 Human footprint hypothesis ---------
### Model ------
mod_hft<-lm(FCL ~ hft,df)
summary(mod_hft)
anova(mod_hft)
tab_model(mod_hft)

r2<- round(summary(mod_hft)$r.squared,digits = 3)
p<- round(anova(mod_hft)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_hft)

### Plot ------ 

G_footprint<-ggplot(df,aes(y=FCL,x=hft))+
  geom_point(alpha=0.35,col="brown3")+
  ggtitle("Human disturbance hypothesis")+
  labs(x="Human footprint (log-scale [unitless])",y="FCL")+
  theme_classic()+theme(plot.title = element_text(face = "bold"))+
  annotate("text", x = 400, y = 6, 
           label = paste("p=",p), 
           color = "black", size = 5)
G_footprint

## 2.5 Size hypothesis ---------
### Model ---------
mod_size_1<-lm(FCL ~ size_z_scored,df)
summary(mod_size_1)
anova(mod_size_1)
tab_model(mod_size_1)
r2<- round(summary(mod_size_1)$r.squared,digits = 3)
p<- round(anova(mod_size_1)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_size_1)

### Plot ---------
G_size_1<-ggplot(df,aes(y=FCL,x=size_z_scored))+
  geom_point(alpha=0.35,col="deepskyblue2")+
  geom_smooth(method = "lm",se = TRUE,col="deepskyblue2")+
  ggtitle("Size hypothesis")+
  labs(x="Ecosystem size (log-scale [z-scored])",y="FCL")+
  theme_classic()+
  annotate("text", x = 2, y = 6, 
           label = paste("p=",p), 
           color = "black", size = 5)+
  theme(plot.title = element_text(face = "bold"))
G_size_1

## Metabolic hypothesis ---------  

### Model-----
mod_climate_1<-aov(FCL ~ Climate_zone_e2,df)
summary(mod_climate_1)

anova(mod_climate_1) 
kruskal.test(FCL ~ Climate_zone_e2, data = df) # non-parametric test for climate zones
DescTools::DunnettTest(df$FCL ,df$Climate_zone_e2) # non-parametric test for climate zones

tab_model(mod_climate_1)
p<- round(anova(mod_climate_1)$`Pr(>F)`[1],digits=3)
### Plot---
#### a little tweak to reorder categories of climates in a logical order
df2<- df 
df2$Climate_zone_ord<-factor(df$Climate_zone_e2,
                             levels = c("Cold and wet/mesic","Cool and moist",
                                        "Cool temperate and dry/xeric", 
                                        "Warm temperate",
                                        "Hot and moist","Hot and dry"),
                             ordered = TRUE)

#### figure
G_climate_1<-ggplot(df2,aes(y=FCL,x=Climate_zone_ord,fill=Climate_zone_ord))+
  geom_boxplot(alpha=0.8,show.legend = FALSE)+
  ggtitle("Metabolic theory hypothesis")+
  labs(x="Climate Zone",y="FCL")+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  theme_classic()+
  xlim(2,8)+
  theme(plot.title = element_text(face = "bold"),
        axis.text.x=element_text(hjust=1,vjust=0.5,angle=90))+
  annotate("text", x = 5.7, y = 7, 
           label = paste("p=",p),
           color = "black", size = 5) +
  scale_x_discrete(labels=
                     c("Cold and wet/mesic" = "Cold and \nwet/mesic",
                       "Cool and moist" = "Cool and \nmoist",
                       "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
                       "Warm temperate"="Warm \ntemperate",
                       "Hot and moist"="Hot and \nmoist",
                       "Hot and dry"="Hot and \ndry",angle=90))

G_climate_1


## Final figure ------

jpeg("../figures/FigureUnivariate_FCL_Check.jpeg",height=23,width=20,units="cm",res=300)
G1_3_check<-ggdraw() +
  draw_plot(G_Ecosystem, x = 0, y = 0.9*2/3, width = 0.5, height = 0.75*1/3) +
  draw_plot(G_prodspace, x = 0.5, y = 0.9*2/3, width = 0.5, height = 0.75*1/3) +
  draw_plot(G_disturbance, x = 0, y = 1/3, width = 0.5, height = 0.75*1/3) +
  draw_plot(G_footprint, x = 0.5, y = 1/3, width = 0.5, height = 0.75*1/3) +
  draw_plot(G_size_1, x = 0, y = 0.25*1/3, width = 0.5, height = 0.75*1/3) +  
  draw_plot(G_climate_1, x = 0.5, y = 0, width = 0.5, height = 1/3) +
  draw_plot_label(label = c("a)", "b)", "c)", "d)", "e)", "f)"), 
                  x = c(0, 0.475, 0, 0.475, 0, 0.475), 
                  y = c(0.852*3/3, 0.852*3/3, 0.875*2/3, 0.875*2/3, 1/3, 1/3),
                  size = 14)
G1_3_check
dev.off()