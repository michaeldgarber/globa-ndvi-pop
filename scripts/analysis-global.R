#Analysis-initial steps
# January 23, 2023
#Okay, here I'm making some decisions on what to do with the population categories.
#It might be simplest to just keep the categories as they are, 
#rather than group them further into tertiles,
#given they're already presented in categories
# Feb 21, 2023: now this script is just the global analysis

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

# Run functions----
#Feb 21, 2023:
#I moved to its own script so that I can run the same functions
#for the global analysis as for the USA analysis
source(here("scripts", "analysis-functions.R")) 



# Global analysis
#Load data----
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


# Convert to tibble--------
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

# Data wrangling steps / HIA------
#Do the landscan data wrangling steps
#Function from ~read-ls-pop.R

  
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

## Filter to non-missing cities and landscan pop wrangling-------
#okay, now filter to non-missing values of ORIG_FID (fingers crossed)

#Feb 21, 2023: I'm going to my Landscan wrangling steps here
#because I'd like to get some summary stats of cities by population and area
#to be used as a filter below.
pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_int1 %>% 
  filter(ORIG_FID_miss==0)  %>% 
  landscan_pop_wrangle()#should work on the tibble as well. created ~read-ls-pop.R
  

#Code to be used interactively differs from above to save time.
# pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_gub_not_miss_int %>% 
#   landscan_pop_wrangle()#should work on the tibble as well. created ~read-ls-pop.R

pop_ndvi_gub_biome_tib_gub_not_miss
names(pop_ndvi_gub_biome_tib_gub_not_miss)
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
#yay! this worked. let's save Feb 9 2023 - 6 pm
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib_gub_not_miss, file = "pop_ndvi_gub_biome_tib_gub_not_miss.RData")
load("pop_ndvi_gub_biome_tib_gub_not_miss.RData")
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 


## Create a look-up for city area and population----
#Can't do this in the steps below because that's at the pixel level,
#whereas this needs to be at the city level
names(pop_ndvi_gub_biome_tib_gub_not_miss)
#note this isn't the scaled value.
load("lookup_gub_city_id.RData")
lookup_city_pop_area = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(ORIG_FID) %>% 
  summarise(
    #have to name them something different so they can be joined
    #without duplicating column names
    pop_cat_mean_val_gub = sum(pop_cat_mean_val,na.rm=TRUE),
    pop_cat_min_val_gub = sum(pop_cat_min_val,na.rm=TRUE),
    pop_cat_max_val_gub = sum(pop_cat_max_val,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(lookup_gub_area_km2, by ="ORIG_FID") %>% #area of GUB
  left_join(lookup_gub_city_id, by = "ORIG_FID") %>% #link city ID
  left_join(lookup_city_id_city_population, by = "geoname_id") #link pop of city id
  
save(lookup_city_pop_area, file = "lookup_city_pop_area.RData")
lookup_city_pop_area
lookup_city_pop_area %>% 
  left_join(lookup_gub_city_name, by = "ORIG_FID") %>% 
  arrange(desc(pop_cat_mean_val))
summary(lookup_city_pop_area$pop_cat_min_val)
summary(lookup_city_pop_area$pop_cat_mean_val)
summary(lookup_city_pop_area$pop_cat_max_val)

lookup_city_pop_area %>% 
  filter(pop_cat_mean_val>10000000) %>% 
  ggplot(aes(pop_cat_mean_val))+
  geom_histogram()

## Explore cities with missing biomes------
#Feb 21, 2023:
#Generate a city-biome look-up table. Note there may be one to many in some
#cases, so count the number of pixels. Like this:
#This is in response to the concern of pixels with missing biome (below)
lookup_city_biome = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  landscan_pop_wrangle() %>% #should work on the tibble as well. created ~read-ls-pop.R
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, BIOME_NAME) %>% 
  summarise(n_pixel_city_biome = n()) %>% 
  ungroup()

lookup_city_biome %>% 
  print(n=100)

#cool. now get a 1-1 lookup corresponding to the most common biome within city
lookup_city_biome_majority = lookup_city_biome %>% 
  arrange(ORIG_FID, desc(n_pixel_city_biome)) %>% #sort descending by this
  group_by(ORIG_FID) %>% 
  slice(1) %>%   #and take top row per group
  ungroup() %>% 
  rename(biome_name_majority = BIOME_NAME)

lookup_city_biome_majority  %>% 
  print(n=100)

#save in case I use in other code
setwd(here("data-processed"))
save(lookup_city_biome_majority, file = "lookup_city_biome_majority.RData")
#There are apparenlty some cities that don't have a biome under this method.
lookup_city_biome_majority %>% 
  filter(is.na(BIOME_NAME)==TRUE) %>% 
  nrow()
#325 of 64694
325/64694
nrow(lookup_city_biome_majority)
lookup_city_biome_majority %>% 
  filter(is.na(BIOME_NAME)==TRUE) %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  mapview() #looks like mostly islands and coastal areas.
  
names(pop_ndvi_gub_biome_tib)
table(pop_ndvi_gub_biome_tib$pop_cat_min_val)

## Main analysis steps----
pop_ndvi_gub_biome_tib = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  bind_cols(drf_deaths) %>%   #add DRF to every row (regardless of country)
  #add number of deaths to every row (would be a left_join for more countries)
  #just the USA here so could use bind_cols() but using left_join() for scalability
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  mutate(pixel_id = row_number()) %>% #row number for a given geography
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
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
  #link area and population of the global urban boundary.
  #only include gubs above 5 square km and above 1,000 residents (min per landscan)
  left_join(lookup_city_pop_area, by ="ORIG_FID") %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  filter(area_km2 >5) %>% 
  #add a country group here first so I can summarize by country. this order is good.
  group_by(country_name_en, biome_name_imp, ORIG_FID, pop_cat_1_8) %>%
  mutate_steps_hia_ndvi_pop() %>% #see ~analysis-functions.R
  ungroup()

#Done! 
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib, file = "pop_ndvi_gub_biome_tib.RData")
#load("pop_ndvi_gub_biome_tib.RData")
pop_ndvi_gub_biome_tib

#how do the filters affect the number of rows?
nrow(pop_ndvi_gub_biome_tib)#1193569 if no pop or area filter
#1026388 with the 5 km2 filters and 1,000 pop
names(pop_ndvi_gub_biome_tib)
object.size(pop_ndvi_gub_biome_tib) 
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)
# Summary----
#Add some area and population filters per mtg with David and Tarik Feb 10 2023
#Note the filters will be applied at the level of the city, not the pixel,
#so after these summary steps.

# Look at a table of biome vs corrected biome
pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp, BIOME_NAME) %>% 
  summarise(n=n()) %>%
  print(n=100) #cool. as expected.

## Summarize by biome and city-----
hia_summary_biome_gub= pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp, ORIG_FID) %>% 
  hia_summarise()

hia_summary_biome_gub

## Summarize by city-----
hia_summary_gub= pop_ndvi_gub_biome_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() 
hia_summary_gub %>% 
  arrange(desc(deaths_prevented_per_1k_pop_mean))

### Cities: 1 million plus-----
#Among cities above 1,000,000 people, what are the top 10?
hia_summary_gub_1mil_plus = pop_ndvi_gub_biome_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() %>% 
  filter(pop_cat_mean_val_scaled>=1000000) 
  
hia_summary_gub_1mil_plus
setwd(here("data-processed"))
save(hia_summary_gub_1mil_plus, file="hia_summary_gub_1mil_plus.RData")
### map cities 1 mil plus-----
setwd(here("data-processed"))
load("gub.RData")
gub_hia_1mil_plus = gub %>% 
  left_join(hia_summary_gub_1mil_plus, by = "ORIG_FID") %>% 
  filter(pop_cat_mean_val_scaled >0)

mv_gub_hia_1mil_plus=gub_hia_1mil_plus %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "deaths_prevented_per_1k_pop_mean",
    zcol = "deaths_prevented_per_1k_pop_mean")

mv_gub_hia_1mil_plus
object.size(mv_gub_hia_1mil_plus)
#and one to top 100 by deaths per 1k
library(viridis)
names(gub_hia_1mil_plus)
mv_gub_hia_1mil_plus_top_100=gub_hia_1mil_plus %>% 
  arrange(desc(deaths_prevented_per_1k_pop_mean)) %>% 
  slice(1:100) %>% 
  #add name
  left_join(lookup_gub_city_name, by = "ORIG_FID") %>% 
  dplyr::select(
    starts_with("ORIG_FID"),
    starts_with("city_n"), everything()) %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "deaths_prevented_per_1k_pop_mean",
    zcol = "deaths_prevented_per_1k_pop_mean")

mv_gub_hia_1mil_plus_top_100
#make them centroids for easier vis
mv_gub_hia_1mil_plus_top_100_centroid=gub_hia_1mil_plus %>% 
  arrange(desc(deaths_prevented_per_1k_pop_mean)) %>% 
  slice(1:100) %>% 
  st_centroid() %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "deaths_prevented_per_1k_pop_mean",
    zcol = "deaths_prevented_per_1k_pop_mean")


mv_gub_hia_1mil_plus_top_100_centroid

### Number of population categories in each city / biome---
#note we don't need variation by this to do the analysis,
#but it's worth investigating. how much does pop. vary within city?
n_pop_cat_by_city_biome = pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en, biome_name_imp, ORIG_FID, pop_cat_1_8) %>%
  summarise(n_pixels_in_cat=n()) %>% 
  ungroup() %>% 
  group_by(country_name_en, biome_name_imp, ORIG_FID) %>% 
  summarise(n_pop_cat=n()) %>% #how many pop. categories within group?
  ungroup()

n_pop_cat_by_city_biome
summary(n_pop_cat_by_city_biome$n_pop_cat)
## Summarize by biome-----
hia_summary_biome = pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp) %>% 
  hia_summarise() 

#Feb 21 2023: 330 pm. Biome was modified
# There are a few missing biome. Why?
#Okay, so all of the missings correspond to coastal cities.
#So the missings must have ocurred during the vector to raster conversion.
#That is, there are some pixels that correspond to a city that do not correspond
#to a biome because the pixel corresponding to the city might include
#some of the coastal area
#Note the entire cities aren't missing, just some pixels.
#also see notes on this above.
biome_miss = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(biome_name_imp)==TRUE)

#let's look which ORIG-fid they're a part of
setwd(here("data-processed"))
#load("gub.RData")
load("lookup_gub_orig_fid_geo.RData")#from ~read-gub.R

orig_fid_biome_miss = biome_miss %>% 
  left_join(lookup_gub_orig_fid, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  #take a random sample so it's easier to map
  slice_sample(prop=.1) 

orig_fid_biome_miss %>% mapview()
orig_fid_biome_miss %>% plot()



## Summarize by country----
hia_summary_country = pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en) %>% 
  hia_summarise() 

hia_summary_country %>% 
  arrange(desc(deaths_prevented_per_1k_pop_mean)) %>% 
  print(n=100)



