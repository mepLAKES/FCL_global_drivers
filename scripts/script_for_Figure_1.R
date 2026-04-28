

######################################################
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Figure 1 : a summary of the drivers dataset
# Date: 2025-10-21
# Version: 1.0
# Purpose: Fig 1: summary of the drivers dataset
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

##  Set the working directory to the root of the project ------
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/scripts')
script.dir = paste0(root.dir,'/data')
figures.dir = paste0(root.dir,'/figures')

setwd(script.dir)

### choice of color palette---------
col_pal<-c("darkgrey","deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")
#col_type<-c("coral","deepskyblue3")
col_type<-c("brown3","goldenrod2")


### fixing colors for climate zones 
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")


## load datasets---------
load (file="../data/complete_metrics_env_May25.RData")

df <- df %>%
  mutate(Size_km2 = case_when(
    Ecosystem == "Lotic" ~ exp(Size)/100,
    Ecosystem == "Lentic" ~ exp(Size)),
    TP_2 = exp(TP))

######################################################
# 2. INDIVIDUAL FIGURES -------
######################################################

## MAP OF STUDY SITES -------

world <- map_data("world")
map_sites<-ggplot() +
  geom_map(data=world, map=world,
           aes(map_id=region),
           fill="lightgrey", color="white", size=0.2) +
  geom_point(data=df, aes(x=Longitude, y=Latitude, color=Type,alpha=0.3), size=1, alpha=0.7) +
  scale_color_manual(values=col_type,
                    breaks = c("Lentic","Lotic"),
                    labels =c("Lentic (n=423)", "Lotic (n=502)"))+
  theme_classic() +
#legend.position = "top",
    theme(legend.position = "top",
          legend.direction = "horizontal",
      plot.margin = margin(t = 50,  # Top margin
                             r = 10,  # Right margin
                             b = 50,  # Bottom margin
                             l = 10))+ # Left margin+
  labs(x="Longitude [°]", y="Latitude [°]", color="Ecosystem type")
#  theme(plot.title = element_text(hjust = 0.5)))
map_sites

## ECOSYSTEM-TYPE -------
#gtype<-ggplot(df, aes(x=Type, fill=Type)) +
 # geom_bar(alpha=0.7) +
 # scale_fill_manual(values=col_type,
 #                   breaks = c("Lentic","Lotic"),
 #                   labels =c("Lentic (n=423)", "Lotic (n=502)"))+
 # theme_classic() +
 # theme(legend.position = "top",
  #      legend.direction = "horizontal",
  #      plot.margin = margin(t = 10,  # Top margin
  #                           r = 50,  # Right margin
  #                           b = 10,  # Bottom margin
  #                           l = 50))+
  #labs(x="Ecosystem type", y="Number \nof ecosystems")

## PROD -------
gprod<-ggplot(df, aes(y=TP_2,x=Type, fill=Type,col=Type)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=col_type)+
  scale_color_manual(values=col_type)+
  theme_classic() +
  scale_y_log10(
    breaks = c(0.1, 10, 100, 1000),
    labels = scales::label_log())+
  theme(legend.position = "none",
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))+
  labs(x="Ecosystem type", y=expression(paste("P yield [kg ", ha^-1, yr^-1,"]")))

## SIZE -------

gSize<-ggplot(df, aes(y=Size_km2, x=Type, fill=Type,col=Type)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=col_type)+
  scale_color_manual(values=col_type)+
  scale_y_log10(
    breaks = c(0.1, 10, 1000, 100000),
    labels = scales::label_log())+
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))+
  labs(x="Ecosystem Type", y=expression(paste("Size [", km^2,"]")))

## hydro -------

gHydro<-ggplot(df, aes(y=exp(dis_r_sv)-0.00001, x=Type, fill=Type,col=Type)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=col_type)+
  scale_color_manual(values=col_type)+
  scale_y_log10(
    breaks = c(0.001,0.1,10,1000),
    labels = scales::label_log())+
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))+
  labs(x="Ecosystem Type", y=expression(paste("Flow variability [", 10^-6, s^-1,"]")))
## Footprint -------

gFootprint<-ggplot(df, aes(y=hft,x=Type, fill=Type,col=Type)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=col_type)+
  scale_color_manual(values=col_type)+
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(t = 70,  # Top margin
                             r = 10,  # Right margin
                             b = 70,  # Bottom margin
                             l = 10))+
  labs(y="Human Footprint [unitless]", x="Ecosystem Type")

## Climate -------
df2<-df
df2$Climate_zone_ord<-factor(df$Climate_zone_e2,
                             levels = c("Cold and wet/mesic","Cool and moist",
                                        "Cool temperate and dry/xeric", 
                                        "Warm temperate",
                                        "Hot and moist","Hot and dry"),
                             ordered = TRUE)
gclimate<-ggplot(df2, aes(x=Climate_zone_ord, fill=Climate_zone_ord)) +
  geom_bar() +
  scale_fill_manual(values=fixed_colors)+
  theme_classic() +
  labs(x="Climate zone", y="Number of\n ecosystems",fill="")+
 # scale_x_discrete(labels=
  #                   c("Cold and wet/mesic" = "Cold and \nwet/mesic",
  #                     "Cool and moist" = "Cool and \nmoist",
  #                     "Cool temperate and dry/xeric"= "Cool temperate \nand dry/xeric", 
  #                     "Warm temperate"="Warm \ntemperate",
  #                     "Hot and moist"="Hot and \nmoist",
   #                    "Hot and dry"="Hot and \ndry"))+  
  theme(
    #axis.text.x = element_text(hjust = 1,angle=90),
        legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        axis.title.x=element_text(vjust = -2),
        axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank(),
        plot.margin = margin(t = 10,  # Top margin
                                   r = 40,  # Right margin
                                   b = 10,  # Bottom margin
                                   l = 40)) # Left margin+) 

## FCL
means<-aggregate(FCL ~ Type, data=df, FUN=mean)

gFCL<-ggplot(df, aes(x=FCL,fill=Type,col=Type)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept=means$FCL, linetype="dashed", color=col_type,size=0.8)+
  scale_fill_manual(values=col_type)+
  scale_color_manual(values=col_type)+
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(t = 10,  # Top margin
                             r = 160,  # Right margin
                             b = 10,  # Bottom margin
                             l = 160))+
  labs(x="Food Chain Length ",y="Density")

pannel2b<-ggarrange(gSize, gprod,gHydro,
          ncol=3, nrow=1, labels=c("B","C","D"),
          label.x=0.9,label.y=0.9, font.label = list(size = 12, face = "bold"), 
          widths=c(1,1,1))

pannel2c<-ggarrange(pannel2b, gclimate,
                    ncol=1, nrow=2, labels=c("","E"),
                    label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                    heights=c(1,1))
pannel2a<-ggarrange(map_sites, pannel2c, 
                   ncol=2, nrow=1, labels=c("A"," "),
                   label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                   widths = c(0.8, 1.2))
pannel2d<-ggarrange(pannel2a, gFCL, 
                    ncol=1, nrow=2, labels=c(""," F"),
                    label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                    heights = c(2, 0.8))

pannel2d


jpeg("../figures/Figure_description_drivers.jpeg",height=150,width=300,units="mm",res=300)
pannel2d
dev.off()