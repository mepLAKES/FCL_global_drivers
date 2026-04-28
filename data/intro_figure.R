######################################################
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Figure for hypotheses : models outputs
# Date: 2025-10-21
# Version: 1.0
# Purpose: 
# Author: Marie-Elodie Perga, marie-elodie.perga@unil.ch
######################################################

######################################################
# 1. SET UP -------
######################################################
## libraries---------

library(dplyr)
library(ggplot2)
library(ggpubr)
library(rprojroot)

##  Set the working directory to the root of the project ------
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'/scripts')
figures.dir = paste0(root.dir,'/figures')
model.dir = paste0(root.dir,'/data/models')

setwd(script.dir)

### choice of color palette---------
col_pal<-c("darkgrey","deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")
col_type<-c("coral","deepskyblue3")
col_type<-c("brown3","goldenrod2")


### data import and load
model_size_temp_path <- paste0(model.dir,'/result_BSmax_Temp.txt')
model_disturbance_path <- paste0(model.dir,'/result_Disturb.txt')
model_productivity_temp_path <- paste0(model.dir,'/result_Nut_Temp.txt')
model_S_temp_path <- paste0(model.dir,'/result_S_Temp.txt')

model_size_temp<-read.table(model_size_temp_path , header = TRUE)
model_disturbance<-read.table(model_disturbance_path  , header = TRUE)
model_productivity_temp<-read.table(model_productivity_temp_path , header = TRUE)
model_richness_temp<-read.table(model_S_temp_path , header = TRUE)
##########################
# 2. FIGURE FOR HYPOTHESES -------
##########################
### Figure for hypothesis 1 : effect of size and temperature on FCL -----
model_size_temp_2<- model_size_temp %>%
  filter(Temp==15) #%>%
  #mutate(Temp=factor(Temp,levels=c(15,20,25)))

p_apex<-ggplot(model_size_temp_2, aes(x=maxBM, y=TLmax)) +
  geom_smooth(method = "lm",color="darkgrey",fill="darkgrey",alpha=0.5)+
  labs(x="Apex predator max length", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        colour="darkgrey", size=0.5),
        plot.background = element_rect(fill = "bisque3"))

model_productivity_temp<- model_productivity_temp %>%
  filter(!is.na(K))%>%
  mutate(Temp=factor(Temp, levels=c(20,25)))#%>%
#  mutate(K=factor(K, levels=c(0.1,0.25,0.5,1)))



p_prod_temp<-ggplot(model_productivity_temp, aes(x=K, y=TLmax, fill=Temp,color=Temp) )+
  geom_smooth(method = "lm",alpha=0.5)+
  labs(x="Nutrient concentration", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  scale_color_manual(name="Temp", values=c("deepskyblue","darkolivegreen3","darkgoldenrod2"))+
  scale_fill_manual(name="Temp", values=c("deepskyblue","darkolivegreen3","darkgoldenrod2"))+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        colour="darkgrey", size=0.5),
        plot.background = element_rect(fill = "bisque3"))


### Figure for hypothesis 2 : effect of disturbance on FCL -----
summary(model_disturbance) 
model_disturbance<- model_disturbance %>%
  filter(!is.na(omega))%>%
  filter(!is.na(Z))%>%
 # mutate(Z=factor(Z, levels=c(0,0.25,0.5)))%>%
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
        panel.background = element_rect(fill="white", 
                                        colour="darkgrey", size=0.5),
        plot.background = element_rect(fill = "bisque3"))

### Figure for hypothesis 2 : effect of disturbance on FCL -----
#summary(model_richness_temp) 
model_richness_temp<- model_richness_temp %>%
  filter(S<= 50)%>%
  mutate(Temp=factor(Temp, levels=c(15,20,25)))

lmR<-lm(TLmax ~ S+Temp, data=model_richness_temp)
summary(lmR)
  
  
p_Richness<-ggplot(model_richness_temp, aes(x=S, y=TLmax, color  =Temp, fill=Temp))+
 # geom_point(alpha=0.2)+
  geom_smooth(method = "lm",alpha=0.5)+
  labs(x="Richness", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  scale_color_manual(name="Temp", values=c("deepskyblue","darkolivegreen3","darkgoldenrod2"))+
  scale_fill_manual(name="Temp", values=c("deepskyblue","darkolivegreen3","darkgoldenrod2"))+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        colour="darkgrey", size=0.5),
        plot.background = element_rect(fill = "bisque3"))+
  ylim(3,4.5)



### Combine all panels -------
figure_hypotheses<-ggarrange(p_Richness,p_prod_temp, p_apex, p_dist, ncol=1, nrow=4,
                             labels=c("A","B","C","D"),
                             label.x = 0.1,
                             label.y = 0.95,
                             font.label = list(size = 12, color = "black", face = "bold", family = "Avenir"))


# Export figure
figure_hypotheses_path <- paste0(figures.dir,'/Figure_hypotheses.png')
ggsave(figure_hypotheses_path, figure_hypotheses, width = 6, height = 15, units = "cm", dpi = 300)
