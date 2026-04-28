################################################################################
# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects 
# Script Title : Datasets preparation for the analyses of the drivers of food chain length across freshwater ecosystems 
# Description:  This scripts performs collection and handling of the datasets required for the ms analysis
# Date:         2026-04-27
# Version:      3.0
# Notes:        Refers to other GitHub repositories for the fish dataset
# Dependencies: 
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(readxl)
library(forcats)

################################################################################
# 2. FOOD CHAIN LENGTH DATASET ----------
################################################################################

#### FCL was computed for each site from max TP (species level) and a simulated baseline
#### Codes and procedures already published in Perga et al (2026)
#### Please refer to the Git repository https://github.com/mepLAKES/FoodWeb_baselines
load(file = "./data/FCL_dataset.RData")


################################################################################
# 3. ENVIRONMENTAL PREDICTORS DATASET ----------
################################################################################


#### Load files
load(file = "./data/Predictors_Lotic.RData")
load(file = "./data/Predictors_Lentic.RData")

#### Compute the hydrological disturbances indexes
Predictors_Lotic$dis_r_sv=(Predictors_Lotic$dis_m3_pmx - Predictors_Lotic$dis_m3_pmn)/(0.001*Predictors_Lotic$riv_tc_csu)
Predictors_Lentic$dis_r_sv=(Predictors_Lentic$dis_m3_pmx - Predictors_Lentic$dis_m3_pmn)/(Predictors_Lentic$Vol_total)


#### Standardize and natural log transform the predictors, and create classes for NPP, size and TP

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
  
save(Env,file = "Env.RData")


################################################################################
# 4. APEX PREDATOR BODY MASS ----------
################################################################################

