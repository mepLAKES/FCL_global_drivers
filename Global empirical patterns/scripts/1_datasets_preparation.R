################################################################################
# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects 
# Script Title : Datasets preparation for the analyses of the drivers of food chain length across freshwater ecosystems 
# Description:  This scripts performs collection and handling of the datasets required for the ms analysis
# Date:         2026-04-27
# Version:      3.0
# Notes:        Refers to other GitHub repositories for the fish dataset
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
library(ggplot2)
library(ggpubr)
library(dplyr)
library(readxl)
library(forcats)
library(rfishbase)

################################################################################
# 2. FOOD CHAIN LENGTH DATASET ----------
################################################################################

#### FCL was computed for each site from max TP (species level) and a simulated baseline
#### Codes and procedures already published in Perga et al (2026)
#### Please refer to the Git repository https://github.com/mepLAKES/FoodWeb_baselines
#### https://doi.org/10.5281/zenodo.17718458 (Perga, 2025).

load(file = here("data", "FCL_dataset.RData"))


################################################################################
# 3. ENVIRONMENTAL PREDICTORS DATASET ----------
################################################################################


## Load files
load(file = here("data", "Predictors_Lotic.RData"))
load(file = here("data", "Predictors_Lentic.RData"))

## Compute the hydrological disturbances indexes
Predictors_Lotic$dis_r_sv=(Predictors_Lotic$dis_m3_pmx - Predictors_Lotic$dis_m3_pmn)/(0.001*Predictors_Lotic$riv_tc_csu)
Predictors_Lentic$dis_r_sv=(Predictors_Lentic$dis_m3_pmx - Predictors_Lentic$dis_m3_pmn)/(Predictors_Lentic$Vol_total)


## Standardize and natural log transform the predictors, and create classes for NPP, size and TP

Predictors_Lotic_b<-Predictors_Lotic %>% 
  mutate(Size=log(Size),  #reduce skewness
         dis_r_sv=log(dis_r_sv+0.00001),
         pop=log(pop+0.001),
         upstr=log(upstr+0.00001),
         pop_den=log(pop_den+0.00001),
        TP=log(TP))%>%
  mutate(size_z_scored=scale(Size, center = TRUE, scale = TRUE))%>% #normalize
  mutate(hydro_dis_z_scored=scale(dis_r_sv, center = TRUE, scale = TRUE))

Predictors_Lentic_b<-Predictors_Lentic %>% 
  mutate(Size=log(Size),  #reduce skewness
         dis_r_sv=log(dis_r_sv+0.00001),
         pop=log(pop+0.001),
         upstr=log(upstr+0.00001),
         pop_den=log(pop_den+0.00001),
         TP=log(TP))%>%
  mutate(size_z_scored=scale(Size, center = TRUE, scale = TRUE)) %>%#normalize
  mutate(hydro_dis_z_scored=scale(dis_r_sv, center = TRUE, scale = TRUE))


Env<-bind_rows(Predictors_Lotic_b,Predictors_Lentic_b)

Env<-Env %>% 
  mutate(Climate_zone_e=as.character(as.factor(Climate_zone))) %>%
  mutate(Climate_zone_e = fct_recode(Climate_zone_e,
"Cold and wet"="5",
"Extremely cold and mesic"="6",
"Cold and mesic" = "7",
"Cool temperate and dry"="8",
"Cool temperate and xeric"="9",
"Cool temperate and moist"="10",
"Warm temperate and mesic"="11",
"Warm temperate and xeric"="12",
"Hot and mesic"="13",
 "Hot and dry"="14",
"Hot and arid"="15",
"Extremely hot and arid"="16",
"Extremely hot and xeric"="17",
"Extremely hot and moist"="18"))%>%
  mutate(Climate_zone_e2 = fct_recode(Climate_zone_e,
               "Cold and wet/mesic"= "Cold and wet",
               "Cold and wet/mesic"="Extremely cold and mesic",
               "Cold and wet/mesic"="Cold and mesic" ,
               "Cool temperate and dry/xeric"="Cool temperate and dry",
               "Cool temperate and dry/xeric"="Cool temperate and xeric",
               "Cool and moist"="Cool temperate and moist",
               "Warm temperate"="Warm temperate and mesic",
               "Warm temperate"="Warm temperate and xeric",
               "Hot and moist"="Hot and mesic",
               "Hot and dry"="Hot and dry",
               "Hot and dry"="Hot and arid",
               "Hot and dry"="Extremely hot and arid",
               "Hot and dry"="Extremely hot and xeric",
               "Hot and moist"="Extremely hot and moist"))  
  
save(Env, file = here("data", "Env.RData"))


################################################################################
# 4. APEX PREDATOR BODY MASS ----------
################################################################################
## 4.1 LOAD THE FISH DATA FROM ISOFRESH-------

### for more information on ISOFRESH, see Bouletreau et al (2025)
### 		https://doi.org/10.1051/kmae/2025010

df_fish <- read_excel(here("data", "ISOFRESH.xlsx"), 
                      sheet = "Isotope_Fish", col_types = c("text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "numeric", "text"))
#save(df_fish, 
#     file = "./data/df_fish.RData")


## 4.2. EXTRACTION OF FISH SPECIES LENGTH & WEIGHT DATA from FISHBASE -----------

### select fish species with highest d15N values per sites-------------
df_fish_max<-df_fish %>%
  group_by(`Food web_ID`) %>%
  filter(d15N == max(d15N)) 


### get the length and weight data for the present species ---------------
species_list<- unique(df_fish_max$Scientific_name)
X<-as.data.frame(popchar(species_list, 
                         fields = c("Species", "Lmax", "Wmax"), 
                         server = c("fishbase", "sealifebase"), 
                         version = "latest", 
                         db = NULL)) # all characteristics for all sites of FishBase

df_fishbase<- as.data.frame(X %>% group_by(Species) %>%
                              summarise(Lmax = mean(Lmax, na.rm = TRUE), 
                                        Wmax = mean(Wmax, na.rm = TRUE)) ) # mean Length and Weight for species across all sites of FishBase

save(df_fishbase, 
     file = here("data", "df_fishbase.RData"))
### Join the data with the maximum d15N values per site for further analyses-----

df_fish_length_weights<-df_fish_max %>%
  left_join(df_fishbase, by = c("Scientific_name" = "Species"))

save(df_fish_length_weights, 
     file = here("data", "df_fish_length_weights.RData"))


################################################################################
# 5. COMPUTE RICHNESS PER SITE ----------
################################################################################
##  Calculate Richness per sites and create the data_frame-----
load(file = here("data", "df_fish.RData"))

richness<-as.vector(table(df_fish$`Food web_ID`))
FWID<-as.vector(row.names(table(df_fish$`Food web_ID`)))
df_rich<-data.frame(FW_ID = FWID, 
                    richness = richness)

save(df_rich, file = here("data", "df_rich.RData"))





