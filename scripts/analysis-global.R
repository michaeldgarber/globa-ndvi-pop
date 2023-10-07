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
#Load data & run preliminary scripts------

## Load data----
library(tidyverse)
library(terra)
library(tidyterra)
library(here)
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
#get the merged UN data
source(here("scripts", "merge-un-countries-geo.R")) 
countries_joined_with_un_pop_deaths_pared_nogeo

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
  landscan_pop_wrangle()
  

#Code to be used interactively differs from above to save time.
# pop_ndvi_gub_biome_tib_gub_not_miss_int =pop_ndvi_gub_biome_tib_gub_not_miss
# pop_ndvi_gub_biome_tib_gub_not_miss = pop_ndvi_gub_biome_tib_gub_not_miss_int %>%
#   landscan_pop_wrangle()

names(pop_ndvi_gub_biome_tib_gub_not_miss)
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
#yay! this worked. let's save Feb 9 2023 - 6 pm
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib_gub_not_miss, 
     file = "pop_ndvi_gub_biome_tib_gub_not_miss.RData")
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



# Lookups-------
## lookup for city area and population----
#Can't do this in the steps below because that's at the pixel level,
#whereas this needs to be at the city level
names(pop_ndvi_gub_biome_tib_gub_not_miss)
#note this isn't the scaled value.
load("lookup_gub_city_id.RData")
load("lookup_gub_area_km2.RData")
lookup_gub_pop_area = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
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
  #Sep 29, 2023 note:
  #note this city_population comes from 
  #https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/
  #which does not necessarily equal the same land area / definition as the urban areas
  #otherwise used.
  #This lookup is defined here
  #global-ndvi-pop/scripts/read-boundaries-states-countries.R
  left_join(lookup_city_id_city_population_geonames, by = "geoname_id") %>%  #link pop of city id
  #Per Ana's book (Chapter 2), it can be important to classify cities by size
  #Some popular categories (figure 2.4, urban public health)
  #: fewer than 300k, 300k to 500k, 500k to 1 million, 1 to 5 million
  #The atlas of urban expansion also classifies cities (table 2.1) as
  #100k to 427k
  #427k to 1.57 m
  #5.715 m
  #5.714 m plus
  #Decisions for how to classify population - either based on the city_pop
  #from the other city-specific dataset or via a summed total of landscan
  #within the GUB. I think the more interpretable result will be via
  #the city-specific numbers, even if it doesn't correspond to the total GUB
  mutate(
    #The first classification system is from Figure 2.4, Urban Public Health
    #and corresponds to specific city boundaries
    pop_cat_breaks_city_geonames=cut(city_population_geonames, 
                     breaks=c(0,300000,500000,1000000,5000000,10000000, 25000000)),
    #The second corresponds to the Atlas of Urban Expansion and we therefore use
    #the landscan population values. let's go with mean.
    #aue for atlas of urban expansion; ls for landscan
    pop_cat_breaks_gub_aue_ls=cut(pop_cat_mean_val_gub,
                           breaks=c(0,100000,427000,1570000,5715000,
                                    300000000)#last category is 300,000,000 - max mean value
                           )
    )
  
lookup_gub_pop_area
setwd(here("data-processed"))
save(lookup_gub_pop_area, file = "lookup_gub_pop_area.RData")
load("lookup_gub_city_name.RData")

## lookup for country in case country is missing from city data-------
names(pop_ndvi_gub_biome_tib_gub_not_miss)
#note some of these overlap multiple countries, so do the below
lookup_orig_fid_country_name_en=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(ORIG_FID,country_name_en) %>% 
  summarise(pop_cat_min_val=sum(pop_cat_min_val,na.rm=T)) %>% 
  group_by(ORIG_FID) %>% 
  arrange(desc(pop_cat_min_val)) %>% 
  slice(1) %>% #grab the top
  ungroup() %>% 
  arrange(ORIG_FID)

lookup_orig_fid_country_name_en


setwd(here("data-processed"))
save(lookup_orig_fid_country_name_en,file="lookup_orig_fid_country_name_en.RData")

#Examine city population
lookup_gub_pop_area %>% 
  left_join(lookup_gub_city_name, by = "ORIG_FID") %>% 
  arrange(desc(pop_cat_mean_val_gub))

lookup_gub_pop_area %>% 
  left_join(lookup_gub_city_name, by = "ORIG_FID") %>% 
  arrange(desc(city_population))

summary(lookup_gub_pop_area$pop_cat_min_val_gub)
summary(lookup_gub_pop_area$pop_cat_mean_val_gub)
summary(lookup_gub_pop_area$pop_cat_max_val_gub)

#How does city_pop generally plot against landsat values
lookup_gub_pop_area %>% 
  filter(pop_cat_mean_val_gub>100000) %>% 
  ggplot(aes(x=city_population,y= pop_cat_min_val_gub))+
  geom_point()

lookup_gub_pop_area %>% 
  filter(pop_cat_mean_val_gub>100000) %>% 
  ggplot(aes(pop_cat_mean_val_gub))+
  geom_histogram()

summary(lookup_gub_pop_area$pop_cat_mean_val_gub)
summary(lookup_gub_pop_area$pop_cat_min_val_gub)
summary(lookup_gub_pop_area$pop_cat_max_val_gub)
summary(lookup_gub_pop_area$city_population)

#the issue with strictly using city_population is that there are some GUBs that
#don't have a corresponding city
lookup_gub_pop_area %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  filter(area_km2 >5) %>% 
  group_by(pop_cat_breaks_city) %>% 
  summarise(n=n())

#This looks pretty good. We can go with this.

lookup_gub_pop_area %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  filter(area_km2 >5) %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n())

#a lookup just for the population categories I just created
lookup_city_gub_pop_cat_breaks=lookup_gub_pop_area %>% 
  dplyr::select(ORIG_FID,starts_with("pop_cat_breaks"))

setwd(here("data-processed"))
save(lookup_city_gub_pop_cat_breaks,file="lookup_city_gub_pop_cat_breaks.RData")
  
  

## lookup for city-biome-----------

### Explore cities with missing biomes------
#Feb 21, 2023:
#Generate a city-biome look-up table. Note there may be one to many in some
#cases, so count the number of pixels. Like this:
#This is in response to the concern of pixels with missing biome (below)
#changing this from lookup_city_biome to lookup_gub_biome to differentiate the two
lookup_gub_biome = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, BIOME_NAME) %>% 
  summarise(n_pixel_city_biome = n()) %>% 
  ungroup()

lookup_gub_biome %>% 
  print(n=100)

#cool. now get a 1-1 lookup corresponding to the most common biome within city
lookup_gub_biome_top  = lookup_gub_biome %>% 
  arrange(ORIG_FID, desc(n_pixel_city_biome)) %>% #sort descending by this
  group_by(ORIG_FID) %>% 
  slice(1) %>%   #and take top row per group
  ungroup() %>% 
  rename(biome_name_top = BIOME_NAME)

lookup_gub_biome_top   %>% 
  print(n=100)

#save in case I use in other code
setwd(here("data-processed"))
save(lookup_gub_biome_top , file = "lookup_gub_biome_top .RData")
#There are apparently some cities that don't have a biome under this method.
lookup_gub_biome_top  %>% 
  filter(is.na(biome_name_top)==TRUE) %>% 
  nrow()
#325 of 64694
325/64694
nrow(lookup_gub_biome_top )
load("lookup_gub_orig_fid_geo.RData")
lookup_gub_biome_top  %>% 
  filter(is.na(biome_name_top)==TRUE) %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  mapview() #looks like mostly islands and coastal areas.

##lookup for Population category------
#I want to add a comma to the upper bound, especially, so
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_max_val)
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_max_fac)
lookup_pop_cat_max_fac=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  distinct(pop_cat_max_val,pop_cat_max_fac) %>% 
  mutate(
    pop_cat_max_fac_w_comma_int=as.factor(prettyNum(pop_cat_max_val, big.mark=",", trim=T)),
    #this step orders it by values of the double version of the pop_cat_max_val. nice
    #https://forcats.tidyverse.org/reference/fct_reorder.html
    pop_cat_max_fac_w_comma=fct_reorder(pop_cat_max_fac_w_comma_int,pop_cat_max_val)
  ) %>% 
  #Now I can drop pop_cat_max_val and the intermediate factor 
  dplyr::select(-pop_cat_max_val,-pop_cat_max_fac_w_comma_int) %>% 
  arrange(pop_cat_max_fac_w_comma)
    
lookup_pop_cat_max_fac
#Going to save this for use elsewhere
setwd(here("data-processed"))
save(lookup_pop_cat_max_fac, 
     file = "lookup_pop_cat_max_fac.RData")

lookup_pop_cat_max_fac

# Main analysis steps----
names(countries_joined_with_un_pop_deaths_pared_nogeo)
pop_ndvi_gub_biome_tib = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  bind_cols(drf_deaths) %>%   #add DRF to every row (regardless of country)
  #add number of deaths to every row (would be a left_join for more countries)
  #just the USA here so could use bind_cols() but using left_join() for scalability
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  mutate(pixel_id = row_number()) %>% #row number for a given pixel
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  #link in the city-biome lookup so that I can impute biomes
  #for pixels with missing biomes
  left_join(lookup_gub_biome_top , by = "ORIG_FID") %>% 
  #Feb 21 2023: now, if a given pixel is missing a biome, set it to the value
  #of the most common biome for that city
  mutate(
    #imp for imputed (if necessary)
    biome_name_imp=case_when(
      is.na(BIOME_NAME==TRUE) ~biome_name_top,
      TRUE ~BIOME_NAME)
  ) %>% 
  #link area and population of the global urban boundary.
  #only include gubs above 5 square km and above 1,000 residents (min per landscan)
  left_join(lookup_gub_pop_area, by ="ORIG_FID") %>% 
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

### Checks-----
#how do the filters affect the number of rows?
nrow(pop_ndvi_gub_biome_tib)#1193569 if no pop or area filter
#1026388 with the 5 km2 filters and 1,000 pop
names(pop_ndvi_gub_biome_tib)
object.size(pop_ndvi_gub_biome_tib) 
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)


#How many cities have more than one biome?
n_biome_within_gub = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, BIOME_NAME) %>% 
  summarise(n_pixels_biome=n()) %>% 
  group_by(ORIG_FID) %>% 
  summarise(n_biome = n_distinct(BIOME_NAME)) %>% 
  ungroup()

n_biome_within_gub
summary(n_biome_within_gub$n_biome)

n_gub_w_2_or_more_biomes = n_biome_within_gub %>% 
  filter(n_biome>1) %>% 
  nrow()

n_gub = n_biome_within_gub %>% nrow()

n_gub_w_2_or_more_biomes/n_gub
1-(n_gub_w_2_or_more_biomes/n_gub)

#Describe variation of pop. density within GUB?
names(pop_ndvi_gub_biome_tib_gub_not_miss)
n_pop_cat_gub = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, pop_cat_1_8) %>% 
  summarise(n_pixels_pop_cat=n()) %>% 
  group_by(ORIG_FID) %>% 
  summarise(n_pop_cat = n_distinct(pop_cat_1_8))

n_pop_cat_gub
summary(n_pop_cat_gub$n_pop_cat)

# check on pt vs ll vs ul variables
names(pop_ndvi_gub_biome_tib)
# pop_ndvi_gub_biome_tib %>% 
#   dplyr::select(ORIG_FID, starts_with("paf"), starts_with("n_d_prev_m")) %>% 
#   slice(1:500) %>% 
#   rowwise() %>% 
#   #what is the min and max of the 9 attrib deaths var?
#   mutate(
#     n_d_prev_min_across_vars = min(
#       n_d_prev_mean_pt, n_d_prev_min_pt, n_d_prev_max_pt,
#       n_d_prev_mean_ll, n_d_prev_min_ll, n_d_prev_max_ll,
#       n_d_prev_mean_ul, n_d_prev_min_ul, n_d_prev_max_ul, na.rm=TRUE)
#     ,
#   n_d_prev_max_across_vars = max(
#     n_d_prev_mean_pt, n_d_prev_min_pt, n_d_prev_max_pt,
#     n_d_prev_mean_ll, n_d_prev_min_ll, n_d_prev_max_ll,
#     n_d_prev_mean_ul, n_d_prev_min_ul, n_d_prev_max_ul, na.rm=TRUE)
#   ) %>% 
#   View()

      #okay, n_d_prev_min_ul is the overall min.
      #That makes sense because _ul corresponds to the bound closer to 1 in the risk ratio
      #That means n_d_prev_max_ll should correspond to the overall max. Yes.

#check the factor version of the scaled population


#End of code here
#For the summaries, now look here: ~summary-global.R
