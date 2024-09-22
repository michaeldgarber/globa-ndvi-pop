#table-main-text

#Note this code is very similar to the RMarkdown
#global-ndvi-pop/docs-for-word/tables-main-text.Rmd
#but this one explicitly relies on the publicly posted
#dataset

library(tidyverse)
library(here)

#Run this script
source(here("scripts", "analysis-functions.R"))

setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib_public.RData")# pixel-level results
load("lookup_pop_cat_max_fac.RData")#look-up table for pop density results
load("lookup_income_grp.RData") #look-up table for country-level WHO income class

lookup_income_grp
lookup_pop_cat_max_fac

names(pop_ndvi_gub_biome_tib_public)
nrow(pop_ndvi_gub_biome_tib_public)
ncol(pop_ndvi_gub_biome_tib_public)
# Table 2-----
#number of pixels below top tertile
pop_ndvi_gub_biome_tib_public %>% 
  filter(ndvi_tertile<3) %>%
  nrow()


## Overall results----
t_results_overall = pop_ndvi_gub_biome_tib_public %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  hia_summarise_who_only() %>% 
  mutate(prop_na_n_deaths_of_total=
           n_d_na_prev_crude_who_mean_pt/
           n_d_na_0_crude_who_mean
  ) %>% 
  select_vars_for_table() %>% 
  mutate_conc_for_table() %>% 
  dplyr::select(
    #    overall, 
    #    area_km2, #don't need
    contains("conc")) 

t_overall

## By population density-------
setwd(here("data-processed"))
t_results_pop_cat_max_fac=pop_ndvi_gub_biome_tib_public %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(pop_cat_max_fac) %>% 
  hia_summarise_who_only() %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(pop_cat_max_fac!="0") %>% #remove the lowest category - max 0
  mutate_conc_for_table() %>% 
  arrange(desc(pop_cat_max_fac)) %>% 
  dplyr::select(pop_cat_max_fac_w_comma, contains("conc")) %>% 
  dplyr::select(-contains("ndvi_conc")) 

t_results_pop_cat_max_fac

## By biome----
names(pop_ndvi_gub_biome_tib_public)
t_results_biome = pop_ndvi_gub_biome_tib_public %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  filter(is.na(biome_name_imp)==FALSE) %>% 
  group_by(biome_name_imp) %>% 
  hia_summarise_who_only() %>% 
  #Let's sort the biomes by population first
  arrange(desc(pop_cat_mean_val_scaled_who)) %>% 
  select_vars_for_table() %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  mutate_conc_for_table() %>% 
  dplyr::select(
    biome_name_imp, 
    contains("conc")) 

t_results_biome

## By World Bank Income Classification----
t_results_income_grp = pop_ndvi_gub_biome_tib_public %>% 
  #in the original country dataset, name is "name_en", but in the merged
  #file with UN data, I changed it to "country_name_en".
  #This works
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(income_grp) %>% 
  hia_summarise_who_only() %>% 
  left_join(lookup_income_grp_5_rename, by = "income_grp") %>% 
  select_vars_for_table() %>% 
  filter(is.na(income_grp)==FALSE) %>% 
  mutate_conc_for_table() %>% 
  dplyr::select(
    income_grp, 
    contains("conc")) %>% 
  dplyr::select(-contains("ndvi_conc")) 

t_results_income_grp
