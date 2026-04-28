
################################################################################
# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects 
# Description : Checking the maximum size of fish species in FishBase against the maximum size measured in our dataset
# Date:         2026-04-27
# Version:      3.0
# Notes:        
# Dependencies: ggplot2, ggpubr, rfishbase
# Author: Marie-Elodie Perga _ marie-elodie.perga@unil.ch
################################################################################


######################################################
# 1. SET UP -------
######################################################
## libraries
library(here)
library(dplyr)
library(ggplot2)
library(rfishbase)

## datasets
load(file = here("data", "df_indiv.RData")) # individual database for fish isotopes and body length measurements (IsoFreshID)
########################################################

str(df_ind)
colnames(df_ind)

df_ind_length<- df_ind %>%
  filter(organism_type=="fish") %>%
  group_by(collection_site_id, scientific_name) %>%
  summarise(max_length_measured = max(collected_sample_total_length, na.rm=TRUE),
            n_indiv = n()) 

df_test_measured <- df_ind_length %>%
  group_by(scientific_name) %>%
  summarise(n_sites = n(),
            max_max_length_measured = max(max_length_measured, na.rm=TRUE),
            sd_max_length_measured = sd(max_length_measured, na.rm=TRUE)) %>%
  filter(n_sites>=5) %>%
  arrange(desc(max_max_length_measured))


## get the length and weight data for the present species ---------------
species_list<- unique(df_test_measured$scientific_name)
X<-as.data.frame(popchar(species_list, 
                         fields = c("Species", "Lmax"), 
                         server = c("fishbase", "sealifebase"), 
                         version = "latest", 
                         db = NULL)) # all characteristics for all sites of FishBase

df_fishbase<- as.data.frame(X %>% group_by(Species) %>%
                              summarise(Lmax_fishbase = 10*mean(Lmax, na.rm = TRUE)))

df_fishbase_test<- df_test_measured %>%
  left_join(df_fishbase, by = c("scientific_name" = "Species"))

str(df_fishbase_test)
dim(df_fishbase_test)
cor<-cor.test(df_fishbase_test$max_max_length_measured[1:280], df_fishbase_test$Lmax_fishbase[1:280], method="pearson",na.rm=TRUE)

## Visual check of the representation of max size in FishBase ---------------
p1<- ggplot(df_fishbase_test, aes(x= max_max_length_measured, y=Lmax_fishbase)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Mean of max length measured per species (mm, log scale)") +
  ylab("Max length reported in FishBase (mm, log scale)") +
  theme_classic()+
  annotate("text", x = 100, y = 2500, label = paste0("r = ", round(cor$estimate,2), 
                                                     "\np-value =", 10^-21,"\n280 fish species"), size=3)
  
p1

ggsave(here("figures", "SI4_Check_max_size_FishBase_vs_measured.png"), p1, width=6, height=5)


