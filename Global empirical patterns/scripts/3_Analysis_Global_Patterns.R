################################################################################
# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects 
# Script Title : Statistical analysis of main drivers of FCL across freshwater ecosystems 
# Description:  This scripts performs linear/additive modelling of FCL variation
# Date:         2026-04-27
# Version:      3.0
# Notes:        
# Dependencies: ggplot2, ggpubr, dplyr, readxl, forcats, rfishbase
# Author: Marie-Elodie Perga _ marie-elodie.perga@unil.ch
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Libraries
library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(sjPlot)
library(ggpubr)
library(patchwork)
library(mgcv)

## load datasets---------
load(here("Global empirical patterns","data", "Env.RData"))
load(here("Global empirical patterns","data", "FCL_dataset.RData"))
load(here("Global empirical patterns","data", "df_fish_length_weights.RData"))

path_icon_1<-here("Global empirical patterns","figures", "energy.png")
path_icon_2<-here("Global empirical patterns","figures", "bodysize.png")
path_icon_3<-here("Global empirical patterns","figures", "dist.png")

## fonctions
source(here("Global empirical patterns","scripts", "0_functions.R"))

## choice of color palette---------
col_pal<-c("deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")


################################################################################
# 2. DATA WRANGLING ----------
################################################################################


## filtering dataset for extreme values or outliers ---------
df<- FCL_dataset %>% 
  left_join(Env,by="Food web_ID") %>% 
  filter(is.finite(FCL))%>% filter(
    FCL >=2,
    FCL<=8,
    TP>=0)
complete_df<-df
save(complete_df, file = here("Global empirical patterns","data", "complete_df.RData"))


## fixing colors for climate zones
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")



## a little tweak to reorder categories of climates in a logical order
df2<- df 
df2$Climate_zonee3<-factor(df$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)

################################################################################
# 3. HYPOTHESES TESTING _STATS & FIGURES_ ----------
################################################################################

################################################################################
## 3.1 Ecosystem size X climate zone ---------
################################################################################

### Model --------- 
mod_size_2<-lm(FCL ~   Climate_zone_e2 + Climate_zone_e2:size_z_scored,df2)
mod_size_2b<-lm(FCL ~   Climate_zone_e2 + size_z_scored + Climate_zone_e2*size_z_scored,df2)
summary(mod_size_2)
summary(mod_size_2b)

anova(mod_size_2)
anova(mod_size_2b)

tab_model(mod_size_2)

r2<- round(summary(mod_size_2)$r.squared,digits = 3)
p<- round(anova(mod_size_2)$`Pr(>F)`[1],digits=3)

#### model's check
par(mfrow=c(2,2))
plot(mod_size_2)

### Plot of the Model --------- 

#### Main pannel _ linear model
G_size_2 <- ggplot(df2, aes(y = FCL, x = size_z_scored,
                            col = Climate_zonee3, fill = Climate_zonee3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ecosystem size (log & z-scored)", y = "FCL") +
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(b = -5),
    plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
    plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x  = element_text(size = 10),
      axis.text.y  = element_text(size = 10)
    ) +
  annotate("text", x = 2, y = 6,
           label = expression(paste(r^2, "=", 0.059, ", p<", 10^-7)),
           color = "black", size = 4) +
  guides(color = guide_legend(ncol = 1))
G_size_2

#### the slopes for insert


##### Extracting slopes values-------------  
X<-summary(mod_size_2)
slopes_mean<-as.vector(X$coefficients[7:12,1])
slopes_names<-c("Cool and moist","Warm temperate",# check that the order of climate zones match with table X
                "Hot and moist",
                "Hot and dry",
                "Cold and wet/mesic", "Cool temperate and dry/xeric")
slopes_std<-as.vector(X$coefficients[7:12,2])
slopes_p<-as.vector(X$coefficients[7:12,4])
df_slopes<-data.frame(slopes_names,slopes_mean,slopes_std,slopes_p)

#### Plot the slopes values------------- 
G_slope<- df_slopes %>%
  mutate(slopes_names = fct_relevel(slopes_names, 
                                    "Hot and dry","Hot and moist",
                                    "Warm temperate","Cool temperate and dry/xeric",
                                    "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=slopes_mean,x=slopes_names, 
             color=slopes_names )) +
  geom_point(show.legend = FALSE,size=1.5) +
  geom_errorbar(aes(ymin=slopes_mean-slopes_std, ymax=slopes_mean+slopes_std), width=.2,size=1.2,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Slopes",x="Climate zone") +
  theme_classic() +
  ylim(-0.2,0.5)+
  theme(axis.text.x = element_blank(),
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        plot.background = element_rect(fill="transparent",color=NA),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.8), colour = NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
 #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = c(6,5,4),y = c(0.22,0.4,0.48), 
           label = "*", 
           color = "black", size = 6,fontface="bold")
G_slope

#### insert the slopes

G_size_f<-G_size_2 +
  inset_element(
    G_slope,
    left = -0.15, bottom = 0.45,
    right = 0.55, top = 1.1
  )
G_size_f



##############################################################################
## 3.2 Energy limitation hypothesis ---------
################################################################################

### Model --------- 
#_____________________________________________
#### linear 
#_____________________________________________
mod_TP<-lm(FCL ~TP,df2) 
summary(mod_TP)

tab_model(mod_TP)

r2<- round(summary(mod_TP)$r.squared,digits = 3)
p<- round(anova(mod_TP)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2)) #model check
plot(mod_TP)


#_____________________________________________


### Plot of the linear model ---------

G_P <- ggplot(df2, aes(y = FCL, x = TP,
                       col = Climate_zonee3, fill = Climate_zonee3)) +
#  geom_point(alpha = 0.5,colour="grey") +
  geom_point(alpha = 0.25) +
#  geom_smooth(method = "lm", se = TRUE) +
  labs(x=expression(paste("P yield [kg ", ha^-1, yr^-1,"] (log)")),y="FCL")+
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
 #   legend.box = "horizontal",
 #   legend.margin = margin(b = -5),
 #   plot.title = element_text(face = "bold")
  ) +
  annotate("text", x = 6, y = 6,
           label = expression(paste(r^2, "=", 0.004, ", p=", 0.07)),
           color = "black", size = 4) +
  guides(color = guide_legend(ncol = 3))
G_P


#_____________________________________________
#### gam degree 3 to fit the hypothesis


#### model assesment
mod_TP_gam<-gam(FCL ~ Climate_zonee3 +s(TP,by=Climate_zonee3,k=3),data=df2,method="REML")
m0<-gam(FCL ~1,data=df2,method="REML")
m1<-gam(FCL ~Climate_zonee3,data=df2,method="REML")
summary(mod_TP_gam)
anova(m0,mod_TP_gam,test="F")
anova(m1,mod_TP_gam,test="F")

#### a degree three gam model does not explain more than just a univariate model with only climate zone

#### plot of the GAM model ---------

G_TP_gam <- ggplot(df2, aes(y = FCL, x = TP,
                            col = Climate_zonee3, fill = Climate_zonee3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    aes(group = Climate_zonee3),
    method = "gam",
    formula = y ~ s(x, k = 3),
    se = TRUE,alpha=0.5
  ) +
  labs(x = "Ecosystem size (zscored & log)", y = "FCL") +
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(b = -5),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
  ) +
  #  annotate("text", x = 2, y = 6,
  #          label = expression(paste(r^2, "=", 0.059, ", p<", 10^-7)),
  #          color = "black", size = 4) +
  guides(color = guide_legend(ncol = 1))
G_TP_gam
#_____________________________________________



###########################################################################
## 3.3 Disturbance hypothesis ---------
################################################################################

### Model ------
mod_disturbance<-lm(FCL ~ hydro_dis_z_scored,df)
summary(mod_disturbance)
anova(mod_disturbance)
tab_model(mod_disturbance)

r2<- round(summary(mod_disturbance)$r.squared,digits = 3)
p<- round(anova(mod_disturbance)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_disturbance) #check the model

### Plot ------
G_disturbance<-ggplot(df,aes(y=FCL,x=hydro_dis_z_scored))+
  geom_point(alpha=0.5,col="grey")+
  #    geom_point(alpha=0.15)+
  #   geom_smooth(method = "lm",se = TRUE, col="goldenrod2")+
  #    ggtitle("Hydrological disturbance hypothesis")+
  labs(x=expression(paste("Flow variability (log & z-scored))")),y="FCL")+
  theme_classic()+
  annotate("text", x = 2, y = 6, 
           label = expression(paste(r^2,"=0.004,", "p=0.069")), 
           color = "black", size = 4)+
  theme(plot.title = element_text(face = "bold"),
         axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10))


G_disturbance


################################################################################
## 3.4 Body size hypothesis ---------
################################################################################

### Model 1 FCL vs fish length------

df_3<- df %>%  # Merging the isotope-env dataset with the apex species body mass dataset 
  left_join(df_fish_length_weights, by = c("Food web_ID" = "Food web_ID")) %>%
  filter(Lmax<=250) #one outlier close to 280

df_3$Climate_zonee3<-factor(df_3$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)


mod_Lmax<-lm(FCL ~ Lmax , data = df_3)# Model --
summary(mod_Lmax)
tab_model(mod_Lmax)


par(mfrow=c(2,2))
plot(mod_Lmax)

anova(lm(FCL ~ Lmax*Climate_zonee3, data = df_3)) # check for lack of interaction with climate zone

### Plot model 1  ---------
G_length_FCL_2<-ggplot(df_3,aes(y=FCL,x=Lmax))+
  geom_point(alpha=0.15,col="grey")+
  geom_smooth(method = "lm",se = TRUE,col="black")+
  labs(x="Length of the top fish predator species (cm)",y="FCL")+
  theme_classic()+
  annotate("text", x = 125, y = 6, 
           label = expression(paste(r^2,"=",0.024,", p<",10^-4)), 
           color = "black", size = 4)+
  theme(
               plot.title = element_text(face = "bold"),
               axis.title.x = element_text(size = 10),
               axis.title.y = element_text(size = 10),
               axis.text.x  = element_text(size = 10),
               axis.text.y  = element_text(size = 10))

G_length_FCL_2


## Model 2: apex body size to the Ecosystem size across climate zones---------
G_mod_fish_size_2<-lm(Lmax ~ Climate_zone_e2+size_z_scored:Climate_zone_e2, data = df_3)
summary(G_mod_fish_size_2)
anova(G_mod_fish_size_2)

tab_model(G_mod_fish_size_2)


par(mfrow=c(2,2))
plot(mod_Lmax)


G_length_size_2<-ggplot(df_3,aes(y=Lmax,x=size_z_scored,col=Climate_zonee3,fill=Climate_zonee3))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  labs(x="Ecosystem size (log & z-scored)",y="Length of the top fish predator species (cm)")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 2, y = 150, 
           label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
           color = "black", size = 4)+
  theme(       legend.position = "none",
               plot.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10))+
  guides(color = guide_legend(ncol = 1))
G_length_size_2


### Plot the intercepts ---------
#### Extracting the intercept values
anova(G_mod_fish_size_2)
T<-summary(G_mod_fish_size_2)



intercept_names<-c("Cool and moist",# check order of climate zones in T
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")
intercept_mean<-c(T$coefficients[1,1],
                  T$coefficients[1,1]+T$coefficients[2:6,1])

u<-c(0,(as.vector(T$coefficients[2:6,2]))^2) # to compute intercept std
v<-rep((T$coefficients[1,2])^2,6)
intercept_std<-sqrt(u+v)

df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std)

#### Plot the intercept values

G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=intercept_mean,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(intercept_mean-intercept_std), ymax=(intercept_mean+intercept_std)), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (cm)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = 1:6 ,y = c(70,70,70,100,80,100), 
          label = c("a","a","a","b","c","b"), 
           color = "black", size = 3,fontface="italic")
G_intercept

##figure with insert
G_length_f<-G_length_size_2 +
  inset_element(
    G_intercept,
    left = -0.15, bottom = 0.4,
    right = 0.7, top = 1.1
  )
G_length_f

G_body_size<-ggarrange(G_length_FCL_2,G_length_f,
                        ncol=2, nrow=1, labels=c("B","C"),
                        label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                        widths=c(1.2,1.5),align="h")


################################################################################
## 3.5 final figure
################################################################################

G_P<-ggarrange(G_P,labels=c("D"),
              label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"))

G_energy<-decorate_plot(G_P, path_icon_1,
                        border_col = "purple4",
                        top_text_face="bold",
                        top_text = "ENERGY LIMITATION",
                        panel_label = "",
                        icon_w = 0.25, icon_h = 0.25,label_color="black",
                        border_lwd = 2,pad=0.01,
                        top_text_x = 0.5)

G_energy

G_disturbance<-ggarrange(G_disturbance,labels=c("E"),
               label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"))

G_dist<-decorate_plot(G_disturbance, path_icon_3,
                      border_col = "chartreuse4",
                      top_text_face="bold",
                      panel_label = "",
                      top_text = "DISTURBANCE",
                      icon_w = 0.25, icon_h = 0.25,label_color="black",
                      border_lwd = 2,pad=0.01,
                      top_text_x = 0.4)

G_dist

GDE<-ggarrange(G_energy,G_dist,align="h")

G_bodysize<-decorate_plot(G_body_size, path_icon_2,
                          border_col = "deepskyblue4",
                          top_text_face="bold",
                          panel_label = "",
                          top_text = "BODY-SIZE CONSTRAINTS",
                          icon_w = 0.15, icon_h = 0.2,label_color="black",
                          border_lwd = 2,pad=0.01,
                          top_text_x = 0.4)

G_bodysize



final_figure<-ggarrange(
  ggarrange(G_size_f,labels=c("A"),label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold")),
  G_bodysize,GDE,
  ncol=1, nrow=3,
  heights=c(1.8,1.8,1))


final_figure

ggsave(here("figures", "Figure_3_drivers.png"), final_figure,
       width = 220, height =300, units = "mm",
       dpi = 300, bg = "white")

################################################################################
# 5. ADDITIONNAL TESTS FOR SUPPLEMENTARIES
################################################################################

################################################################################
# 5.1 Is that linked to a richness effect? Model Richness versus climate and size
################################################################################

## Data wrangling
load(file = here("data", "df_rich.RData"))

colnames(df_rich)[1]<-"Food web_ID"

df_4<-df_3 %>% 
  left_join(df_rich, by="Food web_ID") %>%
  filter(richness<=40) # one outlier with 60 species, but it is a very small stream, so I think it is a mistake in the data and I remove it

df_4$Climate_zonee3<-factor(df_4$Climate_zone_e2,
                                 levels = c("Cold and wet/mesic","Cool and moist",
                                            "Cool temperate and dry/xeric", 
                                            "Warm temperate",
                                            "Hot and moist","Hot and dry"),
                                 ordered = TRUE)##  Model of Richness versus Ecosystem size across climate zones-----

## Model 
mod_richness<-lm(richness ~ Climate_zone_e2+Climate_zone_e2:size_z_scored, data = df_4)
mod_richness_b<-lm(richness ~ Climate_zone_e2*size_z_scored, data = df_4)
summary(mod_richness)
summary(mod_richness_b)
anova(mod_richness)
anova(mod_richness_b)
tab_model(mod_richness)

##  Figure : Richness versus Ecosystem size across climate zones-----


### Richness vs size and cliamte
G_Richness_2<-ggplot(df_4,aes(size_z_scored, richness,col=Climate_zonee3,fill=Climate_zonee3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  labs(x = "Ecosystem Size (log & z-scored)", y = "Fish richness") +
  theme_classic() +theme(legend.position = "right",
                         legend.title = element_text(size = 10),
                         legend.text = element_text(size = 8),
                         axis.title.x = element_text(size = 10),
                         axis.title.y = element_text(size = 10),
                         axis.text.x = element_text(size = 8),
                         axis.text.y = element_text(size = 8))+
  ylim(0,40)

G_Richness_2



### Plot the intercepts ---------
#### Extracting the intercept values
T<-summary(mod_richness)



intercept_names<-c("Cool and moist",
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")

intercept_mean<-c(T$coefficients[1,1],
                  T$coefficients[1,1]+T$coefficients[2:6,1])

u<-c(0,(as.vector(T$coefficients[2:6,2]))^2)
v<-rep((T$coefficients[1,2])^2,6)
intercept_std<-sqrt(u+v)

df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std)

#### Plot the intercept values

G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=intercept_mean/10,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(intercept_mean-intercept_std)/10, ymax=(intercept_mean+intercept_std)/10), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (Richness)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) #+
#  annotate("text", x = 1:6 ,y = c(70,70,70,100,80,100), 
#           label = c("a","a","a","b","c","b"), 
 #          color = "black", size = 3,fontface="italic")
G_intercept

##figure with insert
G_Richness_2_f<-G_Richness_2 +
  inset_element(
    G_intercept,
    left = 0.0, bottom = 0.4,
    right = 0.7, top = 1.1
  )
G_Richness_2_f



# Richness and FCL
mod_richness2<-lm(FCL ~ Climate_zone_e2+Climate_zone_e2:richness, data = df_4)
mod_richness2b<-lm(FCL ~ Climate_zone_e2*richness, data = df_4)

summary(mod_richness2)
summary(mod_richness2b)
anova(mod_richness2)
anova(mod_richness2b)
tab_model(mod_richness2)

##  Figure : Richness versus Ecosystem size across climate zones-----


### Richness vs size and cliamte
G_Richness_3<-ggplot(df_4,aes(richness, FCL,col=Climate_zonee3,fill=Climate_zonee3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  labs(x = "Richness", y = "FCL") +
  theme_classic() +theme(legend.position = "right",
                         legend.title = element_text(size = 10),
                         legend.text = element_text(size = 8),
                         axis.title.x = element_text(size = 10),
                         axis.title.y = element_text(size = 10),
                         axis.text.x = element_text(size = 8),
                         axis.text.y = element_text(size = 8))

G_Richness_3

### Plot the slopes ---------


## extract the slopes values
slopes_names<-c("Cool and moist", # check order of climate zones in T
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")

slope_mean<-as.vector(T$coefficients[7:12,1])
slope_std<-as.vector(T$coefficients[7:12,2])

df_slopes<-data.frame(slopes_names,slope_mean,slope_std)

#### Plot the slopes values

G_slopes<- df_slopes %>%     
  mutate(slopes_names = fct_relevel(slopes_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=slope_mean,x=slopes_names, 
             color=slopes_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(slope_mean-slope_std), ymax=(slope_mean+slope_std)), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Slopes (FCL)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = c(4,6) ,y = c(0.12,0.12), 
           label = c("*","*"), 
          color = "black", size = 5,fontface="italic")
G_slopes



##figure with insert
G_Richness_3_f<-G_Richness_3 +
  inset_element(
    G_slopes,
    left = 0.35, bottom = 0.5,
    right = 1, top = 1.1
  )
G_Richness_3_f


G_richness<-ggarrange(G_Richness_2_f,G_Richness_3_f,
                        ncol=1, nrow=2, labels=c("A","B"),
                        label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                        widths=c(1.5,1.5),align="h",common.legend = TRUE)

ggsave(here("figures", "SI2_Figure_richness.png"), G_richness,
       width = 200, height =200, units = "mm",
       dpi = 300, bg = "white")



################################################
## 5.2. UNIVARIATE MODELS FOR FCL ---------
################################################
### 5.2.1 Ecosystem Type  hypothesis ---------

### Model ---------
mod_Ecosystem<-lm(FCL ~ Ecosystem ,df)
anova(mod_Ecosystem)

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

mod_prodspace<-lm(FCL ~ TP ,df) # linear model
summary(mod_prodspace)
anova(mod_prodspace)


mod_TP_gam<-gam(FCL ~ s(TP,k=3), data = df) # gam model
summary(mod_TP_gam)

par(mfrow=c(2,2))
plot(mod_prodspace)

r2<- round(summary(mod_prodspace)$r.squared,digits = 3)
p<- round(anova(mod_prodspace)$`Pr(>F)`[1],digits=3)

### Plot ------

G_prodspace<-ggplot(df,aes(y=FCL,x=TP))+
  geom_point(alpha=0.35,col="coral2")+
  # ggtitle("Productivity hypothesis")+
  labs(x=expression(paste("P yield [kg ", ha^-1, yr^-1,"] (log))")),y="Food Chain Length")+
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


r2<- round(summary(mod_disturbance)$r.squared,digits = 3)
p<- round(anova(mod_disturbance)$`Pr(>F)`[1],digits=3)



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


## 2.5 Ecosystem Size hypothesis ---------
### Model ---------
mod_size_1<-lm(FCL ~ size_z_scored,df)
summary(mod_size_1)
anova(mod_size_1)
tab_model(mod_size_1)
r2<- round(summary(mod_size_1)$r.squared,digits = 3)
p<- round(anova(mod_size_1)$`Pr(>F)`[1],digits=6)



### Plot ---------
G_size_1<-ggplot(df,aes(y=FCL,x=size_z_scored))+
  geom_point(alpha=0.35,col="deepskyblue2")+
  geom_smooth(method = "lm",se = TRUE,col="deepskyblue2")+
  #           ggtitle("Ecosystem size hypothesis")+
  labs(x="Ecosystem size (log & z-scored)",y="Food Chain Length")+
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
           label = expression(paste("p=",10^-2)), 
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

jpeg("../figures/SI1_Univariate_FCL.jpeg",height=23,width=20,units="cm",res=300)
G1_3<-ggarrange(G_Ecosystem,
                G_prodspace,
                G_disturbance,
                G_size_1,G_climate_1,
                nrow=3,ncol=2,labels = c("A", "B", "C", "D", "E"),label.x=0.9,label.y=0.9, font.label = list(size = 12, face = "bold"))

G1_3
dev.off()







