

######################################################
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Analyses of the relationships between FCL and various environmental descriptors
# Date: 2026-01-20
# Version: 1.0
# Description: This script performs various analyses related:
# - to testing FCL univariate relationships to environmental descriptors,
# - to testing FCL relationships to Ecosystem size X Climate zones,
# - to testing the relationships underpinning the hypothesis of Ecosystem size X Climate zones on FCL
# The scripts also provides figures for the results.
# Purpose: explaining FCL across aquatic ecosystems by a combination of the Ecosystem size and Metabolic theory
# Author: Marie-Elodie Perga, marie-elodie.perga@unil.ch
######################################################

######################################################
# 1. SET UP -------
######################################################
## libraries---------

library(dplyr)
library(ggplot2)
library(segmented)
library(forcats)
library(sjPlot)
library(ggpubr)
library(rprojroot)
library(cowplot)

##  Set the working directory to the root of the project ------
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'/scripts')
figures.dir = paste0(root.dir,'/figures')

setwd(script.dir)

### choice of color palette---------
col_pal<-c("darkgrey","deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")

### fixing colors for climate zones 
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")


## load datasets---------
load ("../data/Env.RData")
load("../data/dataset_with_metrics_community_May25.RData")
load("../data/df_fish_length_weights.RData")

### filtering dataset for extreme values or outliers ---------
#df<- dataset_with_metrics_community_May25 %>% 
 # left_join(Env,by="Food web_ID") %>% 
 # filter(is.finite(FCL))%>% filter(
 #   FCL >=2,
  #  FCL<=8)

df<- dataset_with_metrics_community_May25 %>% 
  left_join(Env,by="Food web_ID") %>% 
  filter(is.finite(FCL))%>% filter(
 #   CD<=5,
    FCL >=2,
    FCL<=8,
#    NND<=2.5,
    TP>=0
)



df<- df %>% filter( !is.na(size_class))
save(df,file="../data/complete_metrics_env_May25.RData")





################################################
# 2. UNIVARIATE MODELS FOR FCL ---------
################################################
## 2.1 Ecosystem Type  hypothesis ---------

### Model ---------
mod_Ecosystem<-lm(FCL ~ Ecosystem ,df)
anova(mod_Ecosystem)
summary(mod_Ecosystem)
tab_model(mod_Ecosystem)

par(mfrow=c(2,2))
plot(mod_Ecosystem)

r2<- round(summary(mod_Ecosystem)$r.squared,digits = 3)
p<- round(anova(mod_Ecosystem)$`Pr(>F)`[1],digits=3)

### Plot ---------
G_Ecosystem<-ggplot(df,aes(y=FCL,x=Ecosystem))+
  geom_boxplot(alpha=0.8,col="darkgrey")+
 # ggtitle("Ecosystem type hypothesis")+
  labs(x="Ecosystem Type",y="Food Chain Length")+
  theme_classic()+
  annotate("text", x = 0.65, y = 7, 
           label = paste(expression("p="),p), 
           color = "black", size = 3)+
  theme(plot.title = element_text(face = "bold"))
G_Ecosystem

## 2.2 Productivity hypothesis ---------
### Model ------
mod_prodspace<-lm(FCL ~ TP ,df)
anova(mod_prodspace)
summary(mod_prodspace)
tab_model(mod_prodspace)


library(mgcv)
mod_TP_gam<-gam(FCL ~ s(TP,k=3), data = df)

summary(mod_TP_gam)

par(mfrow=c(2,2))
plot(mod_prodspace)

r2<- round(summary(mod_prodspace)$r.squared,digits = 3)
p<- round(anova(mod_prodspace)$`Pr(>F)`[1],digits=3)

### Plot ------
my.formula_TP=y ~ poly(x, 2, raw = TRUE) 

G_prodspace<-ggplot(df,aes(y=FCL,x=TP))+
  geom_point(alpha=0.35,col="coral2")+
 # ggtitle("Productivity hypothesis")+
  labs(x=expression(paste("P yield (log-scale [kg ", ha^-1, yr^-1,"])")),y="Food Chain Length")+
  theme_classic()+
  annotate("text", x = 0.5, y = 7, 
           label = paste("p=",p), 
           color = "black", size = 3)+
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
#    ggtitle("Hydrological disturbance hypothesis")+
    labs(x="Flow variability (log-scale [z-scored])",y="Food Chain Length")+
    theme_classic()+
    annotate("text", x = -2, y = 7, 
             label = paste("p=",p), 
             color = "black", size = 3)+
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
    geom_point(alpha=0.45,col="brown3")+
  ggtitle("Human disturbance hypothesis")+
    labs(x="Human footprint (log-scale [unitless])",y="Food Chain Length")+
    theme_classic()+theme(plot.title = element_text(face = "bold"))+
    annotate("text", x = 50, y = 7, 
             label = paste("p=",p), 
             color = "black", size = 3)
    G_footprint

    ## 2.5 Ecosystem Size hypothesis ---------
    ### Model ---------
    mod_size_1<-lm(FCL ~ size_z_scored,df)
    summary(mod_size_1)
    anova(mod_size_1)
    tab_model(mod_size_1)
    r2<- round(summary(mod_size_1)$r.squared,digits = 3)
    p<- round(anova(mod_size_1)$`Pr(>F)`[1],digits=6)
    
    par(mfrow=c(2,2))
    plot(mod_size_1)
    
    ### Plot ---------
    G_size_1<-ggplot(df,aes(y=FCL,x=size_z_scored))+
      geom_point(alpha=0.35,col="deepskyblue2")+
      geom_smooth(method = "lm",se = TRUE,col="deepskyblue2")+
 #           ggtitle("Ecosystem size hypothesis")+
      labs(x="Ecosystem size (log-scale [z-scored])",y="Food Chain Length")+
      theme_classic()+
      annotate("text", x = -2, y = 7, 
               label = expression(paste("p<4.",10^-4)), 
               color = "black", size = 3)+
      theme(plot.title = element_text(face = "bold"))
    G_size_1
    
    ## Climate hypothesis ---------  
    
    ### Model-----
    mod_climate_1<-aov(FCL ~ Climate_zone_e2,df)
    summary(mod_climate_1)
    
    anova(mod_climate_1) 
    kruskal.test(FCL ~ Climate_zone_e2, data = df) # non-parametric test for climate zones
    DescTools::DunnettTest(df$FCL ,df$Climate_zone_e2) # non-parametric test for climate zones
    
    tab_model(mod_climate_1)
    
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
 #     ggtitle("Climate hypothesis")+
      labs(x="Climate Zone",y="Food Chain Length")+
      scale_fill_manual("Climate zone",values = fixed_colors)+
      theme_classic()+
      xlim(2,8)+
      theme(plot.title = element_text(face = "bold"),
            axis.text.x=element_text(hjust=1,vjust=0.5,angle=90))+
      annotate("text", x = 1, y = 7, 
               label = expression(paste("p<",10^-2)), 
               color = "black", size = 3) +
      scale_x_discrete(labels=
         c("Cold and wet/mesic" = "Cold and \nwet/mesic",
           "Cool and moist" = "Cool and \nmoist",
           "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
           "Warm temperate"="Warm \ntemperate",
           "Hot and moist"="Hot and \nmoist",
           "Hot and dry"="Hot and \ndry",angle=90))
    
    G_climate_1
    
    
    ## Final figure ------
    
   jpeg("../figures/FigureUnivariate_FCL_2.jpeg",height=23,width=20,units="cm",res=300)
    G1_3<-ggarrange(G_Ecosystem,
                    G_prodspace,
                    G_disturbance,
                    G_size_1,G_climate_1,
                    nrow=3,ncol=2,labels = c("A", "B", "C", "D", "E"),label.x=0.9,label.y=0.9, font.label = list(size = 12, face = "bold"))

    G1_3
    dev.off()
    
    ################################################
    # 3. BIVARIATE MODELS FOR FCL ---------
    ################################################
    
    ## 3.1 the Size X Metabolic hypothesis ---------
    
    ### Model --------- 
    mod_size_2<-lm(FCL ~   Climate_zone_ord + size_z_scored:Climate_zone_ord,df2)
    
    summary(mod_size_2)
    anova(mod_size_2)

    
    r2<- round(summary(mod_size_2)$r.squared,digits = 3)
    p<- round(anova(mod_size_2)$`Pr(>F)`[1],digits=5)
    
    par(mfrow=c(2,2))
    plot(mod_size_2)
    
    ### Plot of the Model --------- 

    G_size_2<-ggplot(df2,aes(y=FCL,x=size_z_scored,col=Climate_zone_ord,fill=Climate_zone_ord))+
      geom_point(alpha=0.10)+
      geom_smooth(method = "lm",se = TRUE,linewidth=1)+
      labs(x="Ecosystem size (log-scale [z-scored])",y="Food Chain Length")+
      scale_color_manual("Climate zone",values = fixed_colors)+
      scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
       annotate("text", x = 1, y = 6, 
               label = expression(paste(r^2,"=",0.059,", p<",10^-7)), 
               color = "black", size = 3)+
         theme(legend.position = "right",
               legend.title = element_text(size=8),
               legend.text = element_text(size=6),
               axis.title = element_text(size=10),
               axis.text = element_text(size=10))+
      guides(color = guide_legend(ncol = 1))
    G_size_2
  
    ### Plot of the slopes per climate zone --------- 
    
        ####Extracting slopes values-------------  
    X<-summary(mod_size_2)
    slopes_mean<-as.vector(X$coefficients[7:12,1])
    slopes_names<-c("Cold and wet/mesic","Cool and moist", 
                    "Cool temperate and dry/xeric",
                    "Warm temperate",
                    "Hot and moist",
                    "Hot and dry")
    slopes_std<-as.vector(X$coefficients[7:12,2])
    slopes_p<-as.vector(X$coefficients[7:12,4])
    df_slopes<-data.frame(slopes_names,slopes_mean,slopes_std,slopes_p)
    
    #### Plot the slopes values------------- 
   G_slope<- df_slopes %>%
     mutate(slopes_names = fct_relevel(slopes_names, 
                                       "Cold and wet/mesic","Cool and moist",
                                       "Cool temperate and dry/xeric","Warm temperate",
                                       "Hot and moist","Hot and dry")) %>%
      ggplot(aes(y=slopes_mean,x=slopes_names, 
                        color=slopes_names )) +
      geom_point(show.legend = FALSE) +
      geom_errorbar(aes(ymin=slopes_mean-slopes_std,ymax=slopes_mean+slopes_std), width=.2,show.legend = FALSE)+
      scale_color_manual(values = fixed_colors) +
      labs(y="Slopes",x="Climate zone") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
     annotate("text", x = c(1,2,3),y = c(0.3,0.4,0.5), 
              label = "*", 
              color = "black", size = 6)+
      scale_x_discrete(labels=
                         c("Cold and wet/mesic" = "Cold and \nwet/mesic",
                           "Cool and moist" = "Cool and \nmoist",
                           "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
                           "Warm temperate"="Warm \ntemperate",
                           "Hot and moist"="Hot and \nmoist",
                           "Hot and dry"="Hot and \ndry"))
   G_slope

    ### Final figure of the Size X Metabolic hypothesis ---------
  X<-ggplot() + theme_void()
 G2<- ggarrange(G_size_2, 
                ggarrange(X,G_slope, 
                heights=c(1,3),
                nrow = 2, ncol = 1,
                labels=c("","b)")),
                ncol = 2, nrow = 1, widths=c(1.5,1),labels=c("a)",""))

jpeg("../figures/FigureMetabXSize_FCL.jpeg",height=15,width=20,units="cm",res=300)
G2
dev.off()

## 3.2 Productivity X Climate hypothesis ---------
## Model --------- 
mod_TP_2<-lm(FCL ~   TP*Climate_zone_ord,df2)
summary(mod_TP_2)
anova(mod_TP_2)
r2<- round(summary(mod_TP_2)$r.squared,digits = 3)
p<- round(anova(mod_TP_2)$`Pr(>F)`[3],digits=3)

## Plot of the Model --------- 
my.formula_TP=y ~ poly(x, 2, raw = TRUE) 


G_TP_2<-ggplot(df2,aes(y=FCL,x=TP,col=Climate_zone_ord,fill=Climate_zone_ord))+
  geom_point(alpha=0.15)+
   geom_smooth(method = "lm",se = TRUE)+
#  ggtitle("Productivity X Climate theory")+
  labs(x=expression(paste("P yield (log-scale [kg ", ha^-1, yr^-1,"])")),y="Food Chain Length")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 5, y = 6, 
           label = paste("p<",p), 
           color = "black", size = 3)+
  theme(legend.position = "none",plot.title =element_text(face = "bold"),
        legend.title.position = "none" )+
  guides(color = guide_legend(ncol = 2))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
G_TP_2

jpeg("../figures/FigureMetabXProd_FCL.jpeg",height=15,width=15,units="cm",res=300)
G_TP_2
dev.off()

#####################################################
# 4. HOW MAY ALL OF THIS BE LINKED TO BODY SIZE? --------- 
#####################################################

## 4.1 Merging the isotope-env dataset with the apex species body mass dataset--- 

df_3<- df %>% 
  left_join(df_fish_length_weights, by = c("Food web_ID" = "Food web_ID")) %>%
  filter(Lmax<=200) %>%
  mutate(Lmax_mm=10*Lmax) 



## 4.2 Relationships of apex body size to the Ecosystem size across climate zones ------

### Plot the model apex body size to the Ecosystem size across climate zones---------

#### a little tweak to reorder categories of climates in a logical order

df3<- df_3
df3$Climate_zone_ord<-factor(df_3$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)


### Model . apex body size to the Ecosystem size across climate zones---------
G_mod_fish_size_2<-lm(Lmax_mm ~ Climate_zone_e2+size_z_scored:Climate_zone_e2, data = df_3)
summary(G_mod_fish_size_2)
anova(G_mod_fish_size_2)

tab_model(G_mod_fish_size_2)


#### Figure :apex body size to the Ecosystem size across climate zones
G_length_size_2<-ggplot(df3,aes(y=Lmax_mm,x=size_z_scored,col=Climate_zone_ord,fill=Climate_zone_ord))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  #   ggtitle("Size hypothesis X Metabolic theory")+
  labs(x="Ecosystem size (log-scale [z-scored])",y="Apex predator maximum length [mm]")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 2, y = 1500, 
           label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
           color = "black", size = 3)+
  theme(legend.position = "none",plot.title =element_text(face = "bold"),
        legend.title.position = "top" )+
  guides(color = guide_legend(ncol = 2))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
G_length_size_2

#### Figure :produc to the Ecosystem size across climate zones
G_prod_clim_2<-ggplot(df3,aes(y=Lmax_mm,x=size_z_scored,col=Climate_zone_ord,fill=Climate_zone_ord))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  #   ggtitle("Size hypothesis X Metabolic theory")+
  labs(x="Ecosystem size (log-scale [z-scored])",y="Apex predator maximum length [mm]")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 2, y = 1500, 
           label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
           color = "black", size = 3)+
  theme(legend.position = "none",plot.title =element_text(face = "bold"),
        legend.title.position = "top" )+
  guides(color = guide_legend(ncol = 2))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
G_length_size_2

### 4.3 Plot the intercepts ---------
#### Extracting the intercept values .apex body size to the Ecosystem size across climate zones
T<-summary(G_mod_fish_size_2)
intercept_estimate<-as.vector(T$coefficients[2:6,1])
ref_intercept<-T$coefficients[1,1]
intercept_mean<-c(ref_intercept,intercept_estimate + ref_intercept)
intercept_std<-as.vector(T$coefficients[1:6,2])
intercept_p<-as.vector(T$coefficients[1:6,4])
intercept_names<-c("Cool and moist","Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")
df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std,intercept_p)

#### Plot the intercept values : apex body size to the Ecosystem size across climate zones
G_intercept<- df_intercept %>%      
  mutate(intercept_names = fct_relevel(intercept_names,  
                                       "Cold and wet/mesic","Cool and moist",
                                       "Cool temperate and dry/xeric","Warm temperate",
                                       "Hot and moist","Hot and dry" )) %>%
ggplot(aes(y=intercept_mean,x=intercept_names, 
                               color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=intercept_mean-intercept_std, ymax=intercept_mean+intercept_std), width=.2,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (mm)",x="Climate zone") +
scale_x_discrete(labels=
                     c("Cold and wet/mesic" = "Cold and \nwet/mesic",
                       "Cool and moist" = "Cool and \nmoist",
                       "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
                       "Warm temperate"="Warm \ntemperate",
                       "Hot and moist"="Hot and \nmoist",
                       "Hot and dry"="Hot and \ndry"))+  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1,angle=90))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

G_intercept

## 4.4 Relationships of apex body size to the FCL  ------

### Model of apex body size to the FCL---------
mod_Lmax<-lm(FCL ~ Lmax_mm, data = df3)
summary(mod_Lmax)
tab_model(mod_Lmax)

par(mfrow=c(2,2))
plot(mod_Lmax)
modsizeclim<-lm(FCL ~ Lmax_mm*Climate_zone_e2, data = df_3)
summary(modsizeclim) # check for lack of interaction with climate zone
tab_model(modsizeclim)

### Plot of apex body size to the FCL---------
#### Figure
G_FCL_length_2<-ggplot(df3,aes(x=Lmax_mm,y=FCL,col=Climate_zone_ord,fill=Climate_zone_ord))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  #   ggtitle("Size hypothesis X Metabolic theory")+
  labs(x="Apex predator maximum length [mm]",y="Food Chain Length")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 1250, y = 7, 
           label = expression(paste(r^2,"=",0.023,", p<",0.005)), 
           color = "black", size = 3)+
  theme(legend.position = "none",plot.title =element_text(face = "bold"),
        legend.title.position = "top" )+
  guides(color = guide_legend(ncol = 2))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
G_FCL_length_2




### 4.3 Plot the intercepts of apex body size to the FCL---------
#### Extracting the intercept values
T<-summary(modsizeclim)

intercept_estimate<-as.vector(T$coefficients[2:6,1])
ref_intercept<-T$coefficients[1,1]
intercept_mean<-c(ref_intercept,intercept_estimate + ref_intercept)
intercept_std<-as.vector(T$coefficients[1:6,2])
intercept_p<-as.vector(T$coefficients[1:6,4])
intercept_names<-c("Cool and moist","Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")
df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std,intercept_p)

#### Plot the intercept values of apex body size to the FCL
G_intercept_size<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  
                                       "Cold and wet/mesic","Cool and moist",
                                       "Cool temperate and dry/xeric","Warm temperate",
                                       "Hot and moist","Hot and dry" )) %>%
  ggplot(aes(y=intercept_mean,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=intercept_mean-intercept_std, ymax=intercept_mean+intercept_std), width=.2,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (trophic position)",x="Climate zone") +
  scale_x_discrete(labels=
                     c("Cold and wet/mesic" = "Cold and \nwet/mesic",
                       "Cool and moist" = "Cool and \nmoist",
                       "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
                       "Warm temperate"="Warm \ntemperate",
                       "Hot and moist"="Hot and \nmoist",
                       "Hot and dry"="Hot and \ndry"))+  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1,angle=90)) +
  ylim(3,5)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

G_intercept_size



## 4.5 Final figure of the relationships between FCL, Ecosystem size and Metabolic theory ---------
  G5<-ggarrange(G_length_size_2, G_intercept,G_FCL_length_2,G_intercept_size,
            ncol = 2, nrow = 2,heights=c(1,1),widths=c(1.5,1),
            labels=c("a)","b)","c)","d)"))

  
  jpeg("../figures/Figure_Apex_size_FCL.jpeg",height=19,width=24,units="cm",res=300)
  G5
  dev.off()
  
  ##########################################
  
  #### Supplement for TP/CLimate/Metabolic theory
  ######## Model
  ### Model ---------
  G_mod_fish_size_TP<-lm(Lmax ~ Climate_zone_e2+TP:Climate_zone_e2, data = df_3)
  summary(G_mod_fish_size_TP)
  anova(G_mod_fish_size_TP)
  
  tab_model(G_mod_fish_size_TP)
  
  ### Plot the model ---------
  
  
  G_length_size_3<-ggplot(df3,aes(y=Lmax,x=TP,col=Climate_zone_ord,fill=Climate_zone_ord))+
    geom_point(alpha=0.15)+
    geom_smooth(method = "lm",se = TRUE)+
    #   ggtitle("Size hypothesis X Metabolic theory")+
    labs(x=expression(paste("TP (log-scale [kg ", ha^-1, yr^-1,"])")),y="Max length of apex predator [cm]")+
    scale_color_manual("Climate zone",values = fixed_colors)+
    scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
    # annotate("text", x = 2, y = 150, 
    #        label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
    #      color = "black", size = 4)+
    theme(legend.position = "top",plot.title =element_text(face = "bold"),
          legend.title.position = "top" )+
    guides(color = guide_legend(ncol = 2))
  G_length_size_3
  
  G_mod_fish_size_TP<-lm(Lmax ~ Climate_zone_e2+TP:Climate_zone_e2, data = df_3)
  summary(G_mod_fish_size_TP)
  anova(G_mod_fish_size_TP)
  
  tab_model(G_mod_fish_size_TP)
  
  
  G_FCL_disturbance<-ggplot(df3,aes(x=hydro_dis_z_scored,y=FCL))+
    geom_point(alpha=0.15)+
    geom_smooth(method = "lm",se = TRUE)+
    #   ggtitle("Size hypothesis X Metabolic theory")+
    labs(x="Hydrological disturbance [z-scored]",y="Food Chain Length")+
 #   scale_color_manual("Climate zone",values = fixed_colors)+
 #   scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
    annotate("text", x = 1, y = 7, 
             label = expression(paste("p=",0.07)), 
             color = "black", size = 3)+
    theme(legend.position = "none",plot.title =element_text(face = "bold"),
          legend.title.position = "top" )+
    guides(color = guide_legend(ncol = 2))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
    theme_classic()
  G_FCL_disturbance
  
  
  G_FCL_richness<-ggplot(df3,aes(x=hydro_dis_z_scored,y=FCL))+
    geom_point(alpha=0.15)+
    geom_smooth(method = "lm",se = TRUE)+
    #   ggtitle("Size hypothesis X Metabolic theory")+
    labs(x="Hydrological disturbance [z-scored]",y="Food Chain Length")+
    #   scale_color_manual("Climate zone",values = fixed_colors)+
    #   scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
    annotate("text", x = 1, y = 7, 
             label = expression(paste("p=",0.07)), 
             color = "black", size = 3)+
    theme(legend.position = "none",plot.title =element_text(face = "bold"),
          legend.title.position = "top" )+
    guides(color = guide_legend(ncol = 2))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
    theme_classic()
  G_FCL_disturbance
  
  