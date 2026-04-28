
# Description ------------------------------
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Analyses of the relationships between FCL and various environmental descriptors
# Date: 2025-06-11
# Version: 1.0
# Description: This script performs various analyses related:
# - to testing FCL univariate relationships to environmental descriptors,
# - to testing FCL relationships to Ecosystem size X Climate zones,
# - to testing the relationships underpinning the hypothesis of Ecosystem size X Climate zones on FCL
# #
# Purpose: explaining FCL across aquatic ecosystems by a combination of the Ecosystem size and Metabolic theory
# Author: Marie-Elodie Perga
# #

# data preps-------------

##### load datasets---------
getwd()
load ("../data/Env.RData")
load("../data/dataset_with_metrics_community_May25.RData")
load("../data/df_fish_length_weights.RData")
#####libraries---------

library(dplyr)
library(ggplot2)
library(segmented)
library(forcats)
library(sjPlot)
library(ggpubr)

#####choice of color palette---------
col_pal<-c("deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")

#####filtering dataset for extreme values or outliers ---------
df<- dataset_with_metrics_community_May25 %>% 
  left_join(Env,by="Food web_ID") %>% 
  filter(is.finite(FCL))%>% filter(
    CD<=5,
    FCL >=2,
    FCL<=8,
    NND<=2.5,
    TP>=0)

save(df,file="complete_metrics_env_May25.RData")
df<- df %>% filter( !is.na(size_class))

# Univariate models for FCL ---------
## Ecosystem Type  hypothesis ---------

### Model ---------
mod_Ecosystem<-lm(FCL ~ Ecosystem ,df)
anova(mod_Ecosystem)
summary(mod_Ecosystem)
tab_model(mod_Ecosystem)

par(mfrow=c(2,2))
plot(mod_Ecosystem)

r2<- round(summary(mod_Ecosystem)$r.squared,digits = 3)
p<- round(anova(mod_Ecosystem)$`Pr(>F)`[1],digits=6)

### Plot ---------
G_Ecosystem<-ggplot(df,aes(y=FCL,x=Ecosystem))+
  geom_boxplot(alpha=0.15,col="black")+
  ggtitle("Ecosystem hypothesis")+
  labs(x="Ecosystem Type",y="FCL")+
  theme_classic()+
  annotate("text", x = 2.3, y = 6, 
           label = expression(paste("p<",10^-3)), 
           color = "black", size = 5)+theme(plot.title = element_text(face = "bold"))
G_Ecosystem

## Productive space hypothesis ---------
### Model ------
mod_prodspace<-lm(FCL ~ poly(TP, 2, raw = TRUE) ,df)
anova(mod_prodspace)
summary(mod_prodspace)
tab_model(mod_prodspace)

par(mfrow=c(2,2))
plot(mod_prodspace)

r2<- round(summary(mod_prodspace)$r.squared,digits = 3)
p<- round(anova(mod_prodspace)$`Pr(>F)`[1],digits=3)

### Plot ------
my.formula_TP=y ~ poly(x, 2, raw = TRUE) 

G_prodspace<-ggplot(df,aes(y=FCL,x=TP))+
  geom_point(alpha=0.15,col="coral2")+
#  geom_smooth(method = "lm",se = TRUE, 
 #             formula = my.formula_TP,col="coral2")+
  ggtitle("Productive space hypothesis")+
  labs(x="TP (log-scale _unit)",y="FCL")+
  theme_classic()+
  annotate("text", x = 6, y = 6, 
           label = expression(paste("p=",0.122)), 
           color = "black", size = 5)+theme(plot.title = element_text(face = "bold"))
  G_prodspace

  ## Disturbance hypothesis ---------
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
    geom_point(alpha=0.5,col="grey")+
#    geom_point(alpha=0.15)+
 #   geom_smooth(method = "lm",se = TRUE, col="goldenrod2")+
#    ggtitle("Hydrological disturbance hypothesis")+
    labs(x="Hydrological disturbance (log-scale _z-scored)",y="FCL")+
    theme_classic()+
    annotate("text", x = 2, y = 6, 
             label = expression(paste(r^2,"=0.029,", "p=0.09")), 
             color = "black", size = 3)+
  theme(plot.title = element_text(face = "bold"))+
    theme(axis.title = element_text(size = 10),axis.text = element_text(size = 10))
  
  G_disturbance
  
  ## Human footprint hypothesis ---------
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
    geom_point(alpha=0.15,col="brown3")+
    #    geom_point(alpha=0.15)+
#    geom_smooth(method = "lm",se = TRUE, 
 #               formula = my.formula_TP,col="goldenrod2")+
    ggtitle("Human disturbance hypothesis")+
    labs(x="Human footprint (log-scale _unitless)",y="FCL")+
    theme_classic()+theme(plot.title = element_text(face = "bold"))+
    annotate("text", x = 400, y = 6, 
             label = expression(paste("p=",0.555)), 
             color = "black", size = 3)
    G_footprint

    ## Size hypothesis ---------
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
      geom_point(alpha=0.15,col="deepskyblue2")+
      geom_smooth(method = "lm",se = TRUE,col="deepskyblue2")+
            ggtitle("Size hypothesis")+
      labs(x="Ecosystem size (log and z_scored)",y="FCL")+
      theme_classic()+
     annotate("text", x = 2, y = 6, 
               label = expression(paste(r^2,"=",0.014,", p<",10^-3)), 
               color = "black", size = 5)+theme(plot.title = element_text(face = "bold"))
    G_size_1
    
    ## Metabolic hypothesis ---------  
    
    ### Model-----
    mod_climate_1<-aov(FCL ~ Climate_zone_e2,df)
    summary(mod_climate_1)
    
    anova(mod_climate_1) 
    kruskal.test(FCL ~ Climate_zone_e2, data = df) # non-parametric test for climate zones
    DescTools::DunnettTest(df$FCL ,df$Climate_zone_e2) # non-parametric test for climate zones
    
    tab_model(mod_climate_1)
    
    ### Plot----
    #### fixing colors for climate zones
    fixed_colors=c("Cool and moist" = "darkolivegreen4",
                   "Warm temperate" = "brown1",
                   "Hot and moist" = "darkgoldenrod1",
                   "Hot and dry" = "darkgoldenrod3",
                   "Cold and wet/mesic"="deepskyblue1",
                   "Cool temperate and dry/xeric"="darkolivegreen2")
    
    
    
    #### a little tweak to reorder categories of climates in a logical order
    df2<- df 
    df2$Climate_zonee3<-factor(df$Climate_zone_e2,
                               levels = c("Cold and wet/mesic","Cool and moist",
                                          "Cool temperate and dry/xeric", 
                                          "Warm temperate",
                                          "Hot and moist","Hot and dry"),
                               ordered = TRUE)
    
    #### figure
    G_climate_1<-ggplot(df2,aes(x=FCL,y=Climate_zonee3,fill=Climate_zonee3))+
      geom_boxplot(alpha=0.8,show.legend = FALSE)+
      ggtitle("Metabolic theory hypothesis")+
      labs(y="Climate Zone",x="FCL")+
      scale_fill_manual("Climate zone",values = fixed_colors)+
      theme_classic()+
      xlim(2,8)+
      theme(plot.title = element_text(face = "bold",hjust = -1, vjust=2.12))+
      annotate("text", x = 7, y = 1, 
               label = expression(paste("p<",10^-2)), 
               color = "black", size = 5)
    
    G_climate_1
    
    
    ## Final figure ------
    G1<- ggarrange(G_Ecosystem, G_prodspace, G_size_1, 
                   G_footprint, G_disturbance,G_climate_1, 
                   ncol = 3, nrow = 2,widths=c(1,1,1.3),labels=c("a)","b)","c)","d)","e)","f)"))
    
    jpeg("../Figures/Figure1_FCL.jpeg",height=18,width=31,units="cm",res=300)
    G1
    dev.off()
    
    # FCL : testing the Size X Metabolic hypothesis ---------
    
    ## Model --------- 
    mod_size_2<-lm(FCL ~   Climate_zonee3 + Climate_zonee3:size_z_scored,df2)
    
    summary(mod_size_2)
    anova(mod_size_2)
    tab_model(mod_size_2)
    
    r2<- round(summary(mod_size_2b)$r.squared,digits = 3)
    p<- round(anova(mod_size_2b)$`Pr(>F)`[1],digits=3)
    
    par(mfrow=c(2,2))
    plot(mod_size_2b)
    
    ## Plot of the Model --------- 

    G_size_2 <- ggplot(df2, aes(y = FCL, x = size_z_scored,
                                col = Climate_zonee3, fill = Climate_zonee3)) +
      geom_point(alpha = 0.15) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(x = "Ecosystem size (zscored & log)", y = "FCL") +
      scale_color_manual("Climate zone", values = fixed_colors) +
      scale_fill_manual("Climate zone", values = fixed_colors) +
      theme_classic() +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.margin = margin(b = -5),
        plot.title = element_text(face = "bold")
      ) +
      annotate("text", x = 2, y = 6,
               label = expression(paste(r^2, "=", 0.059, ", p<", 10^-7)),
               color = "black", size = 4) +
      guides(color = guide_legend(ncol = 3))
    G_size_2
    
    
    G_P_2 <- ggplot(df2, aes(y = FCL, x = TP,
                                col = Climate_zonee3, fill = Climate_zonee3)) +
      geom_point(alpha = 0.15) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(x="TP (log-scale _unit)",y="FCL")+
      scale_color_manual("Climate zone", values = fixed_colors) +
      scale_fill_manual("Climate zone", values = fixed_colors) +
      theme_classic() +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.margin = margin(b = -5),
        plot.title = element_text(face = "bold")
      ) +
      annotate("text", x = 2, y = 6,
               label = expression(paste(r^2, "=", 0.0, ", p<", 0)),
               color = "black", size = 4) +
      guides(color = guide_legend(ncol = 3))
    G_P_2
    ## Plot of the slopes per climate zone --------- 
    
        ### Extracting slopes values-------------  
    X<-summary(mod_size_2b)
    slopes_mean<-as.vector(X$coefficients[7:12,1])
    slopes_names<-c("Cool and moist", "Warm temperate",
                    "Hot and moist",
                    "Hot and dry","Cold and wet/mesic","Cool temperate and dry/xeric")
    slopes_std<-as.vector(X$coefficients[7:12,2])
    slopes_p<-as.vector(X$coefficients[7:12,4])
    df_slopes<-data.frame(slopes_names,slopes_mean,slopes_std,slopes_p)
    
    ### Plot the slopes values------------- 
   G_slope<- df_slopes %>%
     mutate(slopes_names = fct_relevel(slopes_names, 
                                       "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
     ggplot(aes(x=slopes_mean,y=slopes_names, 
                        color=slopes_names )) +
      geom_point(show.legend = FALSE) +
      geom_errorbar(aes(xmin=slopes_mean-slopes_std, xmax=slopes_mean+slopes_std), width=.2,show.legend = FALSE)+
      scale_color_manual(values = fixed_colors) +
      labs(x="Slopes (TP/z-scored size)",y="Climate zone") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
     annotate("text", y = c(6,5,4),x = c(0.3,0.4,0.5), 
              label = "*", 
              color = "black", size = 6)
   G_slope

   library(patchwork)
   

   G_size_f<-G_size_2 +
     inset_element(
       G_slope,
       left = 0.01, bottom = 0.5,
       right = 0.48, top = 0.98
     )
    ## Final figure of the Size X Metabolic hypothesis ---------
   
 G2<- ggarrange(G_size_2, G_slope, 
                heights=c(2,1),
                nrow = 2, ncol = 1,
                labels=c("a)","b)"))

jpeg("../Figures/Figure2_FCL.jpeg",height=15,width=15,units="cm",res=300)
G2
dev.off()


# How all of this may be linked to the species body size ? --------- 

### Merging the isotope-env dataset with the apex species body mass dataset 

df_3<- df %>% 
  left_join(df_fish_length_weights, by = c("Food web_ID" = "Food web_ID")) %>%
  filter(Lmax<=200)

## Relationships of apex body size to the Ecosystem size across climate zones ------

### Model ---------
G_mod_fish_size_2<-lm(Lmax ~ Climate_zone_e2+size_z_scored:Climate_zone_e2, data = df_3)
summary(G_mod_fish_size_2)
anova(G_mod_fish_size_2)

tab_model(G_mod_fish_size_2)


### Plot the model ---------

#### a little tweak to reorder categories of climates in a logical order

df3<- df_3
df3$Climate_zonee3<-factor(df_3$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)

#### Figure
G_length_size_2<-ggplot(df3,aes(y=Lmax,x=size_z_scored,col=Climate_zonee3,fill=Climate_zonee3))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  labs(x="Ecosystem size (log and z_scored)",y="Length of the top fish predator species (cm)")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 2, y = 150, 
           label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
           color = "black", size = 3)+
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 1))
G_length_size_2

### Plot the intercepts ---------
#### Extracting the intercept values
T<-summary(G_mod_fish_size_2)
intercept_mean<-as.vector(T$coefficients[2:7,1])
intercept_std<-as.vector(T$coefficients[2:7,2])
intercept_p<-as.vector(T$coefficients[2:7,4])
intercept_names<-c("Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric",
                   "Cool and moist")
df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std,intercept_p)

#### Plot the intercept values
G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                                                     "Warm temperate","Cool temperate and dry/xeric",
                                                                     "Cool and moist","Cold and wet/mesic")) %>%
ggplot(aes(x=intercept_mean,y=intercept_names, 
                               color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(xmin=intercept_mean-intercept_std, xmax=intercept_mean+intercept_std), width=.2,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(x="Intercept (cm)",y="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1)) 

G_intercept

## Relationships of apex body size to the FCL  ------

### Model ---------
mod_Lmax<-lm(FCL ~ Lmax , data = df_3)
summary(mod_Lmax)
tab_model(mod_Lmax)

par(mfrow=c(2,2))
plot(mod_Lmax)

summary(lm(FCL ~ Lmax*Climate_zonee3, data = df3)) # check for lack of interaction with climate zone

### Plot ---------
G_length_FCL_2<-ggplot(df3,aes(y=FCL,x=Lmax))+
  geom_point(alpha=0.15,col="grey")+
  geom_smooth(method = "lm",se = TRUE,col="black")+
  labs(x="Length of the top fish predator species (cm)",y="FCL")+
theme_classic()+
    annotate("text", x = 150, y = 6, 
            label = expression(paste(r^2,"=",0.043,", p=",0.005)), 
            color = "black", size = 3)

G_length_FCL_2

G_length_f<-G_length_FCL_2 +
  inset_element(
    G_length_size_2,
    left = 0.01, bottom = 0.4,
    right = 0.68, top = 0.98
  )

## Final figure of the relationships between FCL, Ecosystem size and Metabolic theory ---------
  G5<-ggarrange(G_P_2,G_length_f,G_disturbance,
            ncol = 3, nrow = 1,labels=c("b","c","d"))
  G6<-ggarrange(G_size_f, G5, 
                ncol = 1, nrow = 2, heights = c(1.7,1.3),labels=c("a)","",""))
  
  figure_test_path <- paste0(figures.dir,'/Figure_test.jpeg')
  jpeg(figure_test_path,height=24,width=15,units="cm",res=300)
  G6
  dev.off()