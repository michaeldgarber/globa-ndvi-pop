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
mutate_steps_hia_ndvi_pop = function(df){
  df %>% 
    mutate( #use group_by() %>% mutate() rather than group_by() %>% summarise()
      #I'm getting strange things with negative values. Maybe create categories manually
      #rather than using cut_interval.
      ndvi_33=quantile(ndvi_2019, probs=0.333333, na.rm=TRUE),
      ndvi_66=quantile(ndvi_2019, probs=0.666666, na.rm=TRUE),
      #The median of the top tertile is by definition 83.3333, right? 5/6. Yes.
      ndvi_83=quantile(ndvi_2019, probs=0.833333, na.rm=TRUE),
      ndvi_tertile = case_when(
        ndvi_2019 <= ndvi_33 ~1,
        ndvi_2019 > ndvi_66 ~3,
        ndvi_2019 > ndvi_33 & ndvi_2019 <=ndvi_66 ~2),
      #Now calculate the difference in NDVI, but only for the bottom two tertiles:
      #scenario: raise all pixels to the median NDVI value of the top tertile
      ndvi_diff = case_when(
        ndvi_tertile <3 ~       ndvi_83-ndvi_2019,
        ndvi_tertile==3 ~ NA_real_),
      
      #Now I can simply do the HIA. For now just use the mean of
      #the population category as a population estimate.
      #Modified from the HIA code in green-space-denver
      #Note we should scale the population first, as I think the
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

# USA-------
## Analysis ------
#Feb 9 2023 update to use left_join to link UN data instead of bind cols so that
#the code is the most consistent between global and USA
names(pop_ndvi_gub_biome_usa_48)
names(countries_joined_with_un_pop_deaths_pared_nogeo)
pop_ndvi_gub_biome_usa_48_tib = pop_ndvi_gub_biome_usa_48 %>% 
  terra::mask(gub_usa_48) %>% #step not necessary for global
  tidyterra::as_tibble() %>% 
  bind_cols(drf_deaths) %>%   #add DRF to every row (regardless of country)
  #add number of deaths to every row (would be a left_join for more countries)
  #just the USA here so could use bind_cols() but using left_join() for scalability
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  mutate(pixel_id = row_number()) %>% #row number for a given geography
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(BIOME_NAME, ORIG_FID, pop_cat_1_8) %>% #group by biome, then city, then pop cat
  mutate_steps_hia_ndvi_pop() %>% 
  ungroup()

pop_ndvi_gub_biome_usa_48_tib

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
hia_summary_usa_48_biome_gub= pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(BIOME_NAME, ORIG_FID) %>% 
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
  arrange(desc(deaths_prevented))

### Summarize by biome-----
hia_summary_usa_48_gub = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(BIOME_NAME) %>% 
  hia_summarise() 

hia_summary_usa_48_biome
#pop_ndvi_biome_usa_48_tib_summary %>% View()

### Summarize by biome-----
hia_summary_usa_48_gub = pop_ndvi_gub_biome_usa_48_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() 

hia_summary_usa_48_gub

#Link with vector data and visualize by city?
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
mv_deaths_prevented_per_1k_pop= gub_usa_48_hia_simplified %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "deaths_prevented_per_1k_pop",
    zcol = "deaths_prevented_per_1k_pop")

mv_deaths_prevented_per_1k_pop
mv_ndvi_diff_mean = gub_usa_48_hia_simplified %>% 
  dplyr::select(ORIG_FID, ndvi_diff_mean) %>% 
  mapview(
    lwd=.1,
    layer.name = "ndvi_diff_mean",
    zcol = "ndvi_diff_mean")

mv_ndvi_diff_mean


# Global----------
## Load data----
library(tidyverse)
library(terra)
library(tidyterra)
library(here)
setwd(here("data-processed"))
pop_ndvi_gub_biome = terra::rast("pop_ndvi_gub_biome.tif") #from merge-rasters.R
object.size(pop_ndvi_gub_biome)
#Remember, you first have to do the mutate steps for the Landscan.
#You already wrote a function for those steps here: scripts/read-ls-pop.R
names(pop_ndvi_gub_biome)
res(pop_ndvi_gub_biome)
dim(pop_ndvi_gub_biome)
pop_ndvi_gub_biome
count(is.na(pop_ndvi_gub_biome$ORIG_FID))


## Convert to tibble--------
#Feb 9 23 4:00 pm
#I'm having issues converting the entire raster stack into a tibble, as I did above
#for the USA analyis. R keeps aborting with a fatal error.
#That is, in theory, this should work, but it doesn't
# pop_ndvi_gub_biome_df = pop_ndvi_gub_biome %>% 
#   tidyterra::as_tibble()


#so maybe I can do it this way and then cbind them all together since they have the 
#same dimensions, as confirmed in ~merge.rasters.R
#Okay, this seems to be working. Let's go.
names(pop_ndvi_gub_biome)
ls_1 = pop_ndvi_gub_biome$`landscan-global-2019-colorized_1` %>% tidyterra::as_tibble()
ls_2 = pop_ndvi_gub_biome$`landscan-global-2019-colorized_2` %>% tidyterra::as_tibble()
ls_3 = pop_ndvi_gub_biome$`landscan-global-2019-colorized_3` %>% tidyterra::as_tibble()
ls_4 = pop_ndvi_gub_biome$`landscan-global-2019-colorized_4` %>% tidyterra::as_tibble()

#okay, those all worked. now can I create a smaller tib without the landscan columns?
#405 pm hasn't failed yet.
#Didn't get a fatal error that it aborted, but did get a vector memory exhausted.
#Okay, let's go one by one then
ndvi_2019 = pop_ndvi_gub_biome$ndvi_2019 %>% tidyterra::as_tibble()
ORIG_FID = pop_ndvi_gub_biome$ORIG_FID %>% tidyterra::as_tibble()
BIOME_NAME = pop_ndvi_gub_biome$BIOME_NAME %>% tidyterra::as_tibble()
country_name_en = pop_ndvi_gub_biome$country_name_en %>% tidyterra::as_tibble()
nrow(country_name_en) #933 million
nrow(BIOME_NAME)
#Another idea: 542 pm: restrict to values where GUB (ORIG_FID) is not missing
names(pop_ndvi_gub_biome)
# pop_ndvi_gub_biome_ORIG_FID_not_miss = pop_ndvi_gub_biome %>% 
#   tidyterra::filter(is.na(ORIG_FID)==FALSE) 
# doesn't work on the raster
#here but could do once it's a tibble.

# hi = ORIG_FID %>% 
#   mutate(ORIG_FID_miss = case_when(is.na(ORIG_FID) ~1, TRUE ~0))
# hi %>% group_by(ORIG_FID_miss) %>% summarise(n=n())
#931918881 missing
#1201119 non missing
1201119/(1201119+931918881)*100 #about 0.1% have ORIG_FID, so limit entire dataset to where it's not missing
#that should make it much more manageable. try it.
#table(country_name_en$country_name_en)
#table(BIOME_NAME$BIOME_NAME)

  
#Now combine them all together again. This could theoretically
#all be done in one piped sequence, but I'm going to separate the steps out 
#so that I can save as I go.
pop_ndvi_gub_biome_tib_int1 = ls_1 %>% 
  bind_cols(
    ls_2, ls_3, ls_4, ndvi_2019, ORIG_FID, BIOME_NAME, country_name_en
  ) %>% 
  mutate(
    ORIG_FID_miss = case_when(
      is.na(ORIG_FID) ~1, 
      TRUE ~0)
    )
    

object.size(pop_ndvi_gub_biome_tib_int1) #this is 50 GB of data.
dim(pop_ndvi_gub_biome_tib_int1)
names(pop_ndvi_gub_biome_tib_int1)
#okay, now filter to non-missing values of ORIG_FID (fingers crossed)
pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_int1 %>% 
  filter(ORIG_FID_miss==0)  
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
#yay! this worked. let's save Feb 9 2023 - 6 pm
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib_gub_not_miss, file = "pop_ndvi_gub_biome_tib_gub_not_miss.RData")

## Data wrangling steps / HIA------
#Do the landscan data wrangling steps
#Function from ~read-ls-pop.R
library(usethis)
usethis::edit_r_environ()
pop_ndvi_gub_biome_tib_int2 = pop_ndvi_gub_biome_tib_int1 %>% 
  
pop_ndvi_gub_biome_tib = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  landscan_pop_wrangle() %>% #should work on the tibble as well
  bind_cols(drf_deaths) %>%   #add DRF to every row (regardless of country)
  #add number of deaths to every row (would be a left_join for more countries)
  #just the USA here so could use bind_cols() but using left_join() for scalability
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  mutate(pixel_id = row_number()) %>% #row number for a given geography
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  #add a country group here first so I can summarize by country. this order is good.
  group_by(country_name_en, BIOME_NAME, ORIG_FID, pop_cat_1_8) %>%
  mutate_steps_hia_ndvi_pop() %>% 
  ungroup()

#Done! 
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib, file = "pop_ndvi_gub_biome_tib.RData")

pop_ndvi_gub_biome_tib

## Summary----
### Summarize by biome and city-----
hia_summary_biome_gub= pop_ndvi_gub_biome_tib %>% 
  group_by(BIOME_NAME, ORIG_FID) %>% 
  hia_summarise() 
hia_summary_biome_gub

### Summarize by biome-----
hia_summary_biome = pop_ndvi_gub_biome_tib %>% 
  group_by(BIOME_NAME) %>% 
  hia_summarise() 


### Summarize by country
hia_summary_country = pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en) %>% 
  hia_summarise() 

hia_summary_country %>% 
  arrange(desc(deaths_prevented_per_1k_pop))



