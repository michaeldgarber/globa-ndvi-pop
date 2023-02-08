#Analysis-initial steps
# January 23, 2023
#Okay, here I'm making some decisions on what to do with the population categories.
#It might be simplest to just keep the categories as they are, 
#rather than group them further into tertiles,
#given they're already presented in categories

library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
#Load data------
#Global urban boundaries for Colorado
#Update January 26, 2023 - let's just do USA
#Old versions are in the old folder. Make this simpler.

setwd(here("data-processed"))
load("gub_colorado.RData") #Global urban boundaries
pop_ndvi_gub_biome_usa_48 = terra::rast("pop_ndvi_gub_biome_usa_48.tif") #from merge-rasters.R
load("gub_usa_48.RData") #for masking
source(here("scripts", "read-united-nations-deaths.R")) #mortality data (United Nations)

setwd(here("data-processed"))
source(here("scripts", "rojas_green_space_drf.R")) #load dose-response info

## Analysis-------
#The reason for converting to tibble earlier is so that you can use grouping operations.
#I think it will work. You can use the ID
drf_deaths
un_pop_deaths_2019_usa

# Write a function for the mutate steps since I use it in three tests
#Note this comes after the grouping (no summarise)
mutate_steps_hia_ndvi_pop = function(df){
  df %>% 
    mutate( #use group_by() %>% mutate() rather than group_by() %>% summarise()
      #I'm getting strange things with negative values. Maybe create categories manually
      #rather than using cut_interval, as I was getting strange results
      ndvi_33=quantile(ndvi_2019, probs=0.333333, na.rm=TRUE),
      ndvi_66=quantile(ndvi_2019, probs=0.666666, na.rm=TRUE),
      #The median of the top tertile is by definition 83.3333, right? 5/6. Yes.
      ndvi_83=quantile(ndvi_2019, probs=0.833333, na.rm=TRUE),
      ndvi_tertile = case_when(
        ndvi_2019 <= ndvi_33 ~1,
        ndvi_2019 > ndvi_66 ~3,
        ndvi_2019 > ndvi_33 & ndvi_2019 <=ndvi_66 ~2),
      #Now calculate the difference in NDVI, but only for the bottom two tertiles:
      #lifting up to the median NDVI value of the top tertile
      ndvi_diff = case_when(
        ndvi_tertile <3 ~       ndvi_83-ndvi_2019,
        ndvi_tertile==3 ~ NA_real_),
      
      #Now I can simply do the HIA. For now just use the mean of
      #the population category as a population estimate.
      #Modified from the HIA code in green-space-denver
      #Note we probably need to scale the population first, as I think the
      #land-scan data includes everyone, not just 20+, so we could get a proportion 20+
      pop_cat_mean_val_scaled = pop_cat_mean_val*pop_ratio_20_plus,
      rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      paf =(rr_alt -1)/rr_alt , #pop_est attrib fraction
      #estimate number of deaths first for summary purposes
      deaths_baseline = death_rate_20_plus*pop_cat_mean_val_scaled,
      attrib_d = paf*deaths_baseline, 
      deaths_prevented = attrib_d*-1,
      deaths_prevented_per_pop = deaths_prevented/pop_cat_mean_val_scaled   
    )
}

# Entire USA-------
## Analysis ------
names(pop_ndvi_gub_biome_usa_48)
pop_ndvi_gub_biome_usa_48_tib = pop_ndvi_gub_biome_usa_48 %>% 
  terra::mask(gub_usa_48) %>% 
  as_tibble() %>% 
  bind_cols(drf_deaths) %>%   #add DRF to every row
  bind_cols(un_pop_deaths_2019_usa) %>% #add number of deaths to every row
  mutate( pixel_id = row_number()) %>% #row number for a given geography
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0
  group_by(BIOME_NAME, ORIG_FID, pop_cat_1_8) %>% #group by biome, then city, then pop cat
  mutate_steps_hia_ndvi_pop() %>% 
  ungroup()

setwd(here("data-processed"))
save(pop_ndvi_gub_biome_usa_48_tib, file = "pop_ndvi_gub_biome_usa_48_tib.RData")

## Summary----
hia_summarise = function(df){
  df %>% 
    summarise(
      pop_cat_mean_val_scaled = sum(pop_cat_mean_val_scaled,na.rm=TRUE),
      deaths_baseline = sum(deaths_baseline, na.rm=TRUE),
      deaths_prevented = sum(deaths_prevented, na.rm=TRUE),
      ndvi_2019_mean = mean(ndvi_2019, na.rm=TRUE),
      ndvi_2019_sd = sd(ndvi_2019, na.rm=TRUE),
      ndvi_diff_mean = mean(ndvi_diff, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(deaths_prevented_per_1k_pop = (deaths_prevented/pop_cat_mean_val_scaled)*1000) 
}

### Summarize by biome and city-----
pop_ndvi_gub_biome_usa_48_tib_summary = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(BIOME_NAME, ORIG_FID) %>% 
  hia_summarise() 
pop_ndvi_gub_biome_usa_48_tib_summary

#do any cities have within-city variation of biome? yes, 300 do.
nrow(pop_ndvi_gub_biome_usa_48_tib_summary)
n_distinct(pop_ndvi_gub_biome_usa_48_tib_summary$ORIG_FID)
pop_ndvi_gub_biome_usa_48_tib_summary %>% 
  arrange(desc(deaths_prevented))

### Summarize by biome-----
pop_ndvi_biome_usa_48_tib_summary = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(BIOME_NAME) %>% 
  hia_summarise() 

pop_ndvi_biome_usa_48_tib_summary
pop_ndvi_biome_usa_48_tib_summary %>% View()


#Link with vector data and visualize?
gub_usa_48_hia = gub_usa_48 %>% 
  left_join(pop_ndvi_gub_biome_usa_48_tib_summary, by = "ORIG_FID")

#make a simplified (lower memory) geometry file for vis
gub_usa_48_hia_simplified = gub_usa_48_hia %>% 
  st_transform(2232) %>% 
  st_simplify(dTolerance = 100) %>% #simplify before mapview
  st_transform(4326)
object.size(gub_usa_48_hia_simplified)
object.size(gub_usa_48_hia)

st_crs(gub_usa_48_hia)
mv_deaths_prevented_per_1k_pop= gub_usa_48_hia_simplified %>% 
  mapview(
    layer.name = "deaths_prevented_per_1k_pop",
    zcol = "deaths_prevented_per_1k_pop")

mv_deaths_prevented_per_1k_pop
mv_ndvi_diff_mean = gub_usa_48_hia_simplified %>% 
  dplyr::select(ORIG_FID, ndvi_diff_mean) %>% 
  mapview(
    layer.name = "ndvi_diff_mean",
    zcol = "ndvi_diff_mean")

mv_ndvi_diff_mean


object.size(mv_gub_usa_48_hia)