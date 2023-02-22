#Analysis-initial steps
# January 23, 2023
#Okay, here I'm making some decisions on what to do with the population categories.
#It might be simplest to just keep the categories as they are, 
#rather than group them further into tertiles,
#given they're already presented in categories

#Feb 21, 2023: this is now just the USA analysis

library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
library(sf)
#Load data------
#Global urban boundaries for Colorado
#Update January 26, 2023 - let's just do USA
#Old versions are in the old folder. Make this simpler.

setwd(here("data-processed"))
load("gub_colorado.RData") #Global urban boundaries
pop_ndvi_gub_biome_usa_48 = terra::rast("pop_ndvi_gub_biome_usa_48.tif") #from merge-rasters.R
load("gub_usa_48.RData") #for masking
2983*6938*9
setwd(here("data-processed"))
source(here("scripts", "rojas_green_space_drf.R")) #load dose-response info

#get the merged UN data
source(here("scripts", "merge-un-countries-geo.R")) 
countries_joined_with_un_pop_deaths_pared_nogeo

# Define analysis function-------
#The reason for converting to tibble earlier is so that you can use grouping operations,
#and so the code will run faster.
#I think it will work. 
drf_deaths
un_pop_deaths_2019_usa

# Write a function for the mutate steps since I use it in three tests
#Note this comes after the grouping (no summarise)
#Feb 21, 2023:
#I moved to its own script so that I can run the same functions
#for the global analysis
source(here("scripts", "analysis-functions.R")) 

#Note we're now USA only
# Analysis ------
#Feb 9 2023 update to use left_join to link UN data instead of bind cols so that
#the code is the most consistent between global and USA
names(pop_ndvi_gub_biome_usa_48)
names(countries_joined_with_un_pop_deaths_pared_nogeo)

setwd(here("data-processed"))
load("lookup_city_biome_majority.RData")#created in ~analysis-global.R
pop_ndvi_gub_biome_usa_48_tib = pop_ndvi_gub_biome_usa_48 %>% 
  terra::mask(gub_usa_48) %>% #step not necessary for global
  tidyterra::as_tibble() %>% 
  bind_cols(drf_deaths) %>%   #add DRF to every row (regardless of country)
  #add number of deaths to every row (would be a left_join for more countries)
  #just the USA here so could use bind_cols() but using left_join() for scalability
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  mutate(pixel_id = row_number()) %>% #row number for a given geography
  #link in the city-biome lookup so that I can impute biomes
  #for pixels with missing biomes
  left_join(lookup_city_biome_majority, by = "ORIG_FID") %>% 
  #Feb 21 2023: now, if a given pixel is missing a biome, set it to the value
  #of the most common biome for that city
  mutate(
    #imp for imputed (if necessary)
    biome_name_imp=case_when(
      is.na(BIOME_NAME==TRUE) ~biome_name_majority,
      TRUE ~BIOME_NAME)
  ) %>% 
  filter(is.na(ORIG_FID)==FALSE) %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(country_name_en, biome_name_imp, ORIG_FID, pop_cat_1_8) %>%
  mutate_steps_hia_ndvi_pop() %>% 
  ungroup()

pop_ndvi_gub_biome_usa_48_tib

setwd(here("data-processed"))
save(pop_ndvi_gub_biome_usa_48_tib, file = "pop_ndvi_gub_biome_usa_48_tib.RData")

# Summary----
#I moved the hia_summarise() function to this script:
source(here("scripts", "analysis-functions.R")) 

## Summarize by biome and city-----
hia_summary_usa_48_biome_gub= pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(biome_name_imp, ORIG_FID) %>% 
  hia_summarise() 
hia_summary_usa_48_biome_gub

#do any cities have within-city variation of biome? yes, 278 do
hia_summary_usa_48_biome_gub %>% 
  group_by(ORIG_FID) %>% 
  summarise(n_biomes_within_city=n()) %>% 
  ungroup() %>% 
  mutate(biome_varies_within_city = case_when(n_biomes_within_city>1~1,TRUE~0)) %>% 
  filter(biome_varies_within_city==1) %>% 
  arrange(desc(n_biomes_within_city))
nrow(hia_summary_usa_48_biome_gub)
n_distinct(hia_summary_usa_48_biome_gub$ORIG_FID)
hia_summary_usa_48_biome_gub %>% 
  arrange(desc(deaths_prevented_per_1k_pop_mean))

## Summarize by biome-----
hia_summary_usa_48_gub = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(biome_name_imp) %>% 
  hia_summarise() 


#pop_ndvi_biome_usa_48_tib_summary %>% View()

## Summarize by city-----
hia_summary_usa_48_gub = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() 

hia_summary_usa_48_gub

### Link with vector data and visualize by city-----
gub_usa_48_hia = gub_usa_48 %>% 
  left_join(hia_summary_usa_48_gub, by = "ORIG_FID")
setwd(here("data-processed"))
save(gub_usa_48_hia, file = "gub_usa_48_hia.RData")


#make a simplified (lower memory) geometry file for vis
gub_usa_48_hia_simplified = gub_usa_48_hia %>% 
  st_transform(2232) %>% 
  st_simplify(dTolerance = 1000) %>% #simplify before mapview
  st_transform(4326)
object.size(gub_usa_48_hia_simplified)
object.size(gub_usa_48_hia)

setwd(here("data-processed"))
save(gub_usa_48_hia_simplified, file = "gub_usa_48_hia_simplified.RData")

library(viridis)
mv_deaths_prevented_per_1k_pop_mean= gub_usa_48_hia_simplified %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "deaths_prevented_per_1k_pop_mean",
    zcol = "deaths_prevented_per_1k_pop_mean")

mv_deaths_prevented_per_1k_pop_mean
mv_ndvi_diff_mean = gub_usa_48_hia_simplified %>% 
  dplyr::select(ORIG_FID, ndvi_diff_mean) %>% 
  mapview(
    lwd=.1,
    layer.name = "ndvi_diff_mean",
    zcol = "ndvi_diff_mean")

mv_ndvi_diff_mean


