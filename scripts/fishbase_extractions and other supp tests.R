
############################################################
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Fishbase extractions and miscellaneous supplementary tests
# Date: 2025-06-11
# Version: 1.0
# Description: This script performs various analyses related:
# - to fish species richness of studied sites and whether it is related to the ecosystem size (S1)
# - to fish species length and weight data extraction from FishBase
# - whether the productivty hypothesis is supported by the data when accounting for the climate zones
# Purpose: Further insights on FCL
# Author: Marie-Elodie Perga ,marie-elodie.perga@unil.ch
# Dependencies: dplyr, ggplot2, tidyr, rfishbase
################################################################

################################################
# 1. SET UP -------
################################################

## libraries---------
library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)
library(sjPlot)
library(rfishbase)
library(rprojroot)

##  Set the working directory to the root of the project
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/scripts')
script.dir = paste0(root.dir,'/data')
figures.dir = paste0(root.dir,'/figures')

setwd(script.dir)

##  define color palette
# fixing colors for climate zones
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")



## Load the data-------
df_fish <- read_excel("../data/ISOFRESH_March2025.xlsx", 
                                 sheet = "Isotope_Fish", col_types = c("text", 
"text", "text", "text", "text", "text", 
"numeric", "numeric", "numeric", 
"numeric", "text", "numeric", "text"))

save(df_fish, 
     file = "../data/df_fish.RData")

load("../data/Env.RData")

################################################
# 2. EXTRACTION OF FISH SPECIES LENGTH & WEIGHT DATA from FISHBASE -----------
################################################


## select fish species with highest d15N values per sites-------------
df_fish_max<-df_fish %>%
  group_by(`Food web_ID`) %>%
  filter(d15N == max(d15N)) 


## get the length and weight data for the present species ---------------
species_list<- unique(df_fish_max$Scientific_name)
X<-as.data.frame(popchar(species_list, 
                         fields = c("Species", "Lmax", "Wmax"), 
                         server = c("fishbase", "sealifebase"), 
                         version = "latest", 
                         db = NULL)) # all characteristics for all sites of FishBase

df_fishbase<- as.data.frame(X %>% group_by(Species) %>%
                              summarise(Lmax = mean(Lmax, na.rm = TRUE), 
                                        Wmax = mean(Wmax, na.rm = TRUE)) ) # mean Length and Weight for species across all sites of FishBase


## Join the data with the maximum d15N values per site for further analyses-----

df_fish_length_weights<-df_fish_max %>%
  left_join(df_fishbase, by = c("Scientific_name" = "Species"))

save(df_fish_length_weights, 
     file = "../data/df_fish_length_weights.RData")



################################################
# 3. TESTS RICHNESS X ECOSYSTEM SIZE -----------
################################################

##  Calculate Richness per sites and create the data_frame-----
load(file = "../data/df_fish.RData")

richness<-as.vector(table(df_fish$`Food web_ID`))
FWID<-as.vector(row.names(table(df_fish$`Food web_ID`)))
df_rich<-data.frame(FW_ID = FWID, 
                 richness = richness)

save(df_rich,file="../data/df_rich.RData")

df_rich2<-Env %>%
  dplyr::select(FW_ID='Food web_ID', Type,Size,size_z_scored,Climate_zone_e2) %>%
  left_join(df_rich)

##  Model of Richness versus Ecosystem size across climate zones-----

mod_richness<-lm(richness ~ Climate_zone_e2+Climate_zone_e2:size_z_scored, data = df_rich2)
summary(mod_richness)
anova(mod_richness)
tab_model(mod_richness)

mod_richness2<-lm(FCL~ richness , data = df_rich2)
summary(mod_richness2)
anova(mod_richness2)
tab_model(mod_richness2)

##  Figure : Richness versus Ecosystem size across climate zones-----

df_rich2b<- df_rich2 # a little tweak to reorder categories of climates in a logical order
df_rich2b$Climate_zonee3<-factor(df_rich2$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)


G_richness<-ggplot(df_rich2b,aes(size_z_scored, richness,col=Climate_zonee3,fill=Climate_zonee3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  labs(x = "Ecosystem Size (z-scored)", y = "Fish richness") +
  theme_classic() +theme(legend.position = "top",
                         legend.title = element_text(size = 10),
                         legend.text = element_text(size = 8),
                         axis.title.x = element_text(size = 10),
                         axis.title.y = element_text(size = 10),
                         axis.text.x = element_text(size = 8),
                         axis.text.y = element_text(size = 8))+
  ylim(0,40)

### Save the figure
jpeg("../figures/Fish_richness_vs_size.jpeg", 
     width = 2000, height = 1500, units="px", res=300)
G_richness
dev.off()



