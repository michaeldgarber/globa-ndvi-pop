#Final combination of datasets before analyses
# January 23, 2023
#Okay, here I'm making some decisions on what to do with the population categories.
#It might be simplest to just keep the categories as they are, 
#rather than group them further into tertiles,
#given they're already presented in categories
# Feb 21, 2023: now this script is just the global analysis
# Dec 1, 2023: I'm separating this from the analysis script, as it
#is very memory intensive. This script produces
#pop_ndvi_gub_biome_tib_gub_not_miss and then ends

#Load data & run preliminary scripts------
library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
library(sf)

## Load data----
setwd(here("data-processed"))
pop_ndvi_gub_biome = terra::rast("pop_ndvi_gub_biome.tif") #from merge-rasters.R
object.size(pop_ndvi_gub_biome)
names(pop_ndvi_gub_biome)
res(pop_ndvi_gub_biome)
dim(pop_ndvi_gub_biome)
pop_ndvi_gub_biome

setwd(here("data-processed"))
source(here("scripts", "rojas_green_space_drf.R")) #load dose-response info
drf_deaths
#get the merged UN - GBD data
source(here("scripts", "merge-un-countries-geo.R")) 
countries_joined_with_un_pop_deaths_pared_nogeo
names(countries_joined_with_un_pop_deaths_pared_nogeo)

## Run functions----
#Feb 21, 2023:
#I moved to its own script so that I can run the same functions
#for the global analysis as for the USA analysis. 
#Feb 22 - this now includes landscan wrangling function as well
source(here("scripts", "analysis-functions.R"))
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups



# Convert to tibble--------
#Feb 9 23 4:00 pm
#I'm having issues converting the entire raster stack into a tibble, as I did above
#for the USA analyis. R keeps aborting with a fatal error.
#That is, in theory, this should work, but it doesn't due to memory issues
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
ndvi_2019 = pop_ndvi_gub_biome$ndvi_2019 %>% 
  tidyterra::as_tibble()

ORIG_FID = pop_ndvi_gub_biome$ORIG_FID %>% 
  tidyterra::as_tibble()

BIOME_NAME = pop_ndvi_gub_biome$BIOME_NAME %>% 
  tidyterra::as_tibble()

#saving as I go, as this sometimes doesn't run.
#Feb 23 2023 - I had to restart R and clear objects for this to work.
#Oct 6, 2023: want to change the name of _en to _ne for natural earth
#to keep track of where the country name comes from.
country_name_en = pop_ndvi_gub_biome$country_name_en %>% 
  tidyterra::as_tibble()
setwd(here("data-processed"))
save(country_name_en, file = "country_name_en.RData")


nrow(country_name_en) #933 million
nrow(BIOME_NAME)
nrow(ORIG_FID)
nrow(ndvi_2019)
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
1201119/(1201119+931918881)*100 
##about 0.1% have ORIG_FID, so limit entire dataset to where it's not missing
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
  ) 
#Feb 23 2023 - this is throwing a memory error. it's not necessary
# pop_ndvi_gub_biome_tib_int2=pop_ndvi_gub_biome_tib_int1 %>% 
#   mutate(
#     ORIG_FID_miss = case_when(
#       is.na(ORIG_FID) ~1, 
#       TRUE ~0)
#     )
    

object.size(pop_ndvi_gub_biome_tib_int1) #this is 50 GB of data.
dim(pop_ndvi_gub_biome_tib_int1)
names(pop_ndvi_gub_biome_tib_int1)

## Filter to non-missing cities and landscan pop wrangling-------
#okay, now filter to non-missing values of ORIG_FID (fingers crossed)

#Feb 21, 2023: I'm going to my Landscan wrangling steps here
#because I'd like to get some summary stats of cities by population and area
#to be used as a filter below.
pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_int1 %>% 
  filter(is.na(ORIG_FID)==FALSE)  %>% 
  #should work on the tibble as well. originally created ~read-ls-pop.R
  #now (feb 22, 2023) can be found in analysis-functions.R
  landscan_pop_wrangle() %>% 
  #Nov 19: simplify some country names
  #so they link cleanly with UN data
  mutate(
    #to keep this around
    country_name_en_original=country_name_en,
    
    country_name_en=
      case_when(
        country_name_en_original=="United Nations Buffer Zone in Cyprus"~"Cyprus",
        country_name_en_original=="Guantanamo Bay Naval Base"~"United States of America",
        country_name_en_original=="Turkish Republic of Northern Cyprus"~"Cyprus",
        country_name_en_original=="United Nations Buffer Zone in Cyprus"~"Cyprus",
        country_name_en_original=="Dhekelia Cantonment"~"Cyprus",
        country_name_en_original=="Akrotiri"~"Cyprus",
        country_name_en_original=="Åland Islands"~"Finland",
        #if else, set it to:
        TRUE ~ country_name_en_original
  )
  )



#Code to be used interactively differs from above to save time.
# pop_ndvi_gub_biome_tib_gub_not_miss_int =pop_ndvi_gub_biome_tib_gub_not_miss
# pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_gub_not_miss_int %>%
# #  landscan_pop_wrangle() %>%
#   mutate(
#     #to keep this around
#     country_name_en_original=country_name_en,
# 
#     country_name_en=
#       case_when(
#         country_name_en_original=="United Nations Buffer Zone in Cyprus"~"Cyprus",
#         country_name_en_original=="Guantanamo Bay Naval Base"~"United States of America",
#         country_name_en_original=="Turkish Republic of Northern Cyprus"~"Cyprus",
#         country_name_en_original=="United Nations Buffer Zone in Cyprus"~"Cyprus",
#         country_name_en_original=="Dhekelia Cantonment"~"Cyprus",
#         country_name_en_original=="Akrotiri"~"Cyprus",
#         country_name_en_original=="Åland Islands"~"Finland",
#         #if else, set it to:
#         TRUE ~ country_name_en_original
#     )
#   )

names(pop_ndvi_gub_biome_tib_gub_not_miss)
# pop_ndvi_gub_biome_tib_gub_not_miss %>%
#   distinct(country_name_en) %>%
#   View()

names(pop_ndvi_gub_biome_tib_gub_not_miss)
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
#nice, this worked. let's save Feb 9 2023 - 6 pm
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib_gub_not_miss, 
     file = "pop_ndvi_gub_biome_tib_gub_not_miss.RData")
setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib_gub_not_miss.RData")
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
names(pop_ndvi_gub_biome_tib_gub_not_miss)


### Checks of intermediate dataset-----
#Of those with non-missing GUB, how many with NDVI below 0?
pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(ndvi_2019<0) %>% 
  nrow()
#0 

pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(is.na(ndvi_2019)) %>% 
  nrow()
#7550
7550/nrow(pop_ndvi_gub_biome_tib_gub_not_miss)

pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  distinct(country_name_en) %>% 
  View()

#Dec 1, 2023: Cut it here. For a continuation of this, see ~analysis-global.

names(pop_ndvi_gub_biome_tib_gub_not_miss)
## Filter to US only for use in the Chicago demo----

