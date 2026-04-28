# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects 
# Script Title : Figure 1 of the manuscript : theoretical effect of the drivers on food chain length
# Date:         2026-04-27
# Version:      3.0
# Notes:        This script produces the figure 1 of the manuscript, which illustrates the theoretical effects of the drivers on food chain length, as predicted by the model. The figure is composed of 4 panels, each showing the effect of one driver on food chain length, based on the model outputs. The script also exports the figure in the figures folder.
# Dependencies: 
# Author: Marie-Elodie Perga _ marie-elodie.perga@unil.ch
################################################################################


######################################################
# 1. SET UP -------
######################################################
#setwd("./Theoretical modelling/scripts") # set the working directory to the Theoretical modelling folder, where the data and figures folders are locate

## libraries---------

library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)


## data import and load
model_size_temp_path <- here("data", "result_BSmax_Temp.txt")
model_disturbance_path <- here("data", "result_Disturb.txt")
model_productivity_temp_path <- here("data", "result_Nut_Temp.txt")
model_S_temp_path <- here("data", "result_S_Temp.txt")

model_size_temp<-read.table(model_size_temp_path , header = TRUE)
model_disturbance<-read.table(model_disturbance_path  , header = TRUE)
model_productivity_temp<-read.table(model_productivity_temp_path , header = TRUE)
model_richness_temp<-read.table(model_S_temp_path , header = TRUE)


##########################
# 2. FIGURE FOR HYPOTHESES -------
##########################
## Generate individual figures

### Figure for hypothesis A : effect of Productivity and temperature on FCL -----
model_productivity_temp<- model_productivity_temp %>%
  filter(!is.na(K))%>%
  mutate(Temp=factor(Temp,levels=c(15,20,25)))%>%
  mutate(Temp_code=recode (Temp, "15" = "Cold", "20"="Temperate","25" = "Warm"))#%>%))


p_prod_temp<-ggplot(model_productivity_temp, aes(x = K, y = TLmax, colour = Temp_code,fill = Temp_code)) +
  geom_smooth(
    aes(group = Temp_code),
    method = "gam",
    formula = y ~ s(x, k = 3),
    se = TRUE,alpha=0.5
  ) +
  labs(x = "Relative resource availability",
       y = "Food chain length (FCL)",
       colour = NULL, fill = NULL) +
  theme_classic(base_family = "Avenir") +
  scale_fill_manual(values=c("deepskyblue","darkolivegreen2","darkgoldenrod2"), name = "") +
  scale_colour_manual(values=c("deepskyblue","darkolivegreen2","darkgoldenrod2"), name = "") +
  theme(legend.position = c(0.8,0.2),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        size=0.5))+
  guides(colour = guide_legend(title = NULL),
         fill   = guide_legend(title = NULL))

p_prod_temp

figure_hypotheses_path <- here("figures", "fig_hyp_a.png")

ggsave(figure_hypotheses_path, p_prod_temp, width = 7, height = 7, units = "cm")


### Figure for hypothesis B : effect of size and temperature on FCL -----
model_size_temp_2<- model_size_temp %>%
  mutate(Temp=factor(Temp,levels=c(15,20,25)))%>%
  mutate(Temp_code=recode (Temp, "15" = "Cold", "20"="Temperate","25" = "Warm"))#%>%))

#### Predator size versus FCL
p_apex<-ggplot(model_size_temp_2, aes(x=log(maxBM), y=TLmax)) +
  geom_smooth(method = "lm",alpha=0.5,col="grey")+
  labs(x="Apex predator max length (log-scaled)", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  ylim(2,6)+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        size=0.5))
p_apex


#### Insert: Predator size with temperature
p_apex_hist <- ggplot(
  model_size_temp_2,
  aes(TLmax, #fill = Temp_code,
      col = Temp_code)
) +
  geom_density(alpha = 0.5,size=0.8
  ) +
  labs(x = "Apex predator max length", y = "Density") +
  theme_classic(base_family = "Avenir") +
  scale_color_manual(
    values = c("deepskyblue", "darkolivegreen2", "darkgoldenrod2"),
    name = ""               # 
  ) +
  theme (
    legend.position="none",
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    panel.background = element_rect(fill = "transparent", colour = NA),  
    plot.background  = element_rect(fill = "transparent", colour = NA),  
  )

### Combining the two

p_apex_f<-p_apex +
  inset_element(
    p_apex_hist,
    left = 0.01, bottom = 0.40,
    right = 0.55, top = 0.99
  )

p_apex_f

### saving the figure in ggsave
figure_hypotheses_path <- here("figures", "fig_hyp_b.png")
ggsave(figure_hypotheses_path, p_apex_f, width = 7, height = 7, units = "cm")

### Figure for hypothesis C : effect of disturbance on FCL -----
summary(model_disturbance) 
model_disturbance<- model_disturbance %>%
  filter(!is.na(omega))%>%
  filter(!is.na(Z))%>%
  mutate(omega=factor(omega, levels=c(0.1,0.5,1)))



p_dist<-ggplot(model_disturbance, aes(x=Z, y=TLmax) )+
  geom_smooth(method = "lm",color="darkgrey",fill="darkgrey",alpha=0.5)+
  labs(x="Disturbance severity", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        plot.background = element_rect(fill = "white"))
p_dist

figure_hypotheses_path <- here("figures", "fig_hyp_c.png")

ggsave(figure_hypotheses_path, p_dist, width = 7, height = 7, units = "cm")

Fig1<-ggarrange(p_prod_temp, p_apex_f, p_dist, ncol=3,labels = c("A", "B", "C"), label.x = 0.95, label.y = 0.95, font.label = list(size = 10, family = "Avenir"))

ggsave(here("figures", "Fig1.png"), Fig1, width = 21, height = 7, units = "cm")
