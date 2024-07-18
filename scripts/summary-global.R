# Summary----#
#March 10th, 2023 
#Breaking this off from the main script so that I can source it
#Rebvised Sep 29, 2023 - working on figures for main text
#Revising Jan 19, 2024 to focus presentation on non-accidental mortality

library(terra)
library(tidyterra)
library(sf)
library(mapview)
library(tidyverse)
library(here)


#Load data from 
#~scripts/analysis-global.R

setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib.RData")
source(here("scripts", "analysis-functions.R"))

#Run this. It's pretty fast and loads necessary look-ups
#Jul 2, 2024: don't run this every time anymore because the bootstrapping takes a while
source(here("scripts", "read-united-nations-gbd-data.R"))


# HIA results-----

## By global urban area (city)-----
#The others should load as long as this has been run:
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups
setwd(here("data-processed"))
load("lookup_gub_several_vars_wrangle.RData")
load("lookup_gub_city_info.RData")
load("lookup_gub_pop_area.RData")

names(lookup_gub_several_vars_wrangle)
table(pop_ndvi_gub_biome_tib$ndvi_tertile)
names(pop_ndvi_gub_biome_tib)
hia_summary_gub= pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  group_by(ORIG_FID) %>% 
  hia_summarise() %>% 
  ungroup() %>% 
  #Oct 7, 2023: this adds lots of relevant variables, created in analysis-global
  #Oct 9, 2023: be very careful with these lookups. See analysis-global
  #for definitions
  left_join(lookup_gub_city_info,by="ORIG_FID") %>% 
  left_join(lookup_gub_pop_area,by="ORIG_FID") %>% 
  mutate(
  #make city name a factor ranked by deaths prevented.
  #Do so here rather than in the ggplot code
  #Base this on the city, country text as it will be graphed
    #Dec 5, 2023: revising ranked factors
    #Jan 17, 2024: updated with the GBDn_d_na_prev_std_who_per_100k_pop_pt
    #could consider ranking by the WHO version as well
    city_name_country_name_ranked_by_n_d_na_prev_per_pop_int=as.factor(
      city_name_country_name),
    city_name_country_name_ranked_by_n_d_na_prev_per_pop=fct_reorder(
      city_name_country_name_ranked_by_n_d_na_prev_per_pop_int,
      #update with the _gbd suffix
      n_d_na_prev_std_who_per_100k_pop_pt,
      .na_rm=TRUE),#note unusual spelling
  
  #and one including the admin code, but only if dupe city-name-country-name
  city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop_int=as.factor(
    city_name_country_name_admin_if_dupe),
  city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop=fct_reorder(
    city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop_int,
    n_d_na_prev_std_who_per_100k_pop_pt,
    .na_rm=TRUE)
  ) %>% 
  #try also a row number sorting
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  mutate(
    order=row_number()
  )

setwd(here("data-processed"))
save(hia_summary_gub,file="hia_summary_gub.RData")

hia_summary_gub %>% View()
nrow(hia_summary_gub)
n_distinct(hia_summary_gub$ORIG_FID)
names(hia_summary_gub)
hia_summary_gub %>%
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>%
  dplyr::select(
    ORIG_FID, starts_with("city_name"),
    ends_with("simplemaps"),
    ends_with("geonames"),
    starts_with("n_d_ac_prev_std_gdb_per_100k"),
    starts_with("n_d_ac_prev_std_who_per_100k"),
    starts_with("n_d_ac_prev_crude_gbd_per_100k"),
    starts_with("n_d_ac_prev_crude_who_per_100k")
                ) %>% 
  View()

names(hia_summary_gub)
hia_summary_gub %>% 
  mutate(
    #Round these before passing through...will be easier than trying to round using data table
    #0 decimals is good.
    n_d_na_prev_std_who_per_100k_pop_pt_round=round(n_d_na_prev_std_who_per_100k_pop_pt),
    n_d_na_prev_std_who_per_100k_min_over_9_round=round(n_d_na_prev_std_who_per_100k_pop_min_over_9),
    n_d_na_prev_std_who_per_100k_max_over_9_round=round(n_d_na_prev_std_who_per_100k_pop_max_over_9)
  ) %>% 
  dplyr::select(
    city_name_admin_code_country_name,
    ndvi_diff_med, #are zeros because of zero NDVI diff?
    n_d_na_0_crude_who_mean, #did WHO data not merge?
    pop_cat_mean_val_gub,ends_with("round")
  ) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt_round)) %>% 
  View()
  

#which country name do I prefer?
# hia_summary_gub %>% 
#   dplyr::select(ORIG_FID, starts_with("city_name"),starts_with("country_name")) %>% 
#   View()

hia_summary_gub %>% 
  dplyr::select(starts_with("ORIG"), starts_with("pop")) %>% 
  arrange(pop_cat_mean_val_scaled_who) %>% 
  print(n=100)

#hia_summary_gub %>%  View()

## checks----
#check duplicate city names. if so, add admin name
city_name_country_name_dupe=hia_summary_gub %>% 
  group_by(city_name_country_name) %>% 
  summarise(n=n()) %>% 
  filter(n>2) %>% 
  print(n=200)

hia_summary_gub %>% 
  filter(country_name_simplemaps=="United States") %>% 
  #  filter(country_name_simplemaps=="China") %>% 
  dplyr::select(contains("admin"))

#Feb 22, 2023: 
#Why are there still some with zero values for the scaled population?
# I assume these are the cities without UN data and thus without a scale factor.
#Let's check.
setwd(here("data-processed"))
load("lookup_gub_geoname_id.RData")
names(hia_summary_gub)
hia_summary_gub %>% 
  dplyr::select(
    starts_with("ORIG"), 
    starts_with("geoname"),
    starts_with("city"),
    starts_with("country"),
    starts_with("pop")
  ) %>% 
  arrange(pop_cat_mean_val_scaled_who)  
#many seem to be in China. Let's map these cities with zero population.

setwd(here("data-processed"))
load("lookup_gub_orig_fid_geo.RData")
#Oct 9, 2023: having issues with st_centroid.
#Suggestion to use st_point_on_surface() instead
#https://www.researchgate.net/post/Error_Messages_with_st_centroid_function_in_Rstudio_Any_Idea
library(sf)
# hia_summary_gub %>%
#   filter(pop_cat_mean_val_scaled_who<5) %>%
#   left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>%
#   st_as_sf() %>%
#   st_point_on_surface() %>% 
# #  st_centroid() %>%
#   mapview()

### GUBs with missing city name (either source)------
#how many?
#after the update to simplemaps, fewer missing - down to 1844
table(hia_summary_gub$city_name_simplemaps_miss)

#Most are pretty small, but not all of them.
hia_summary_gub_city_name_simplemaps_miss=hia_summary_gub %>% 
  filter(is.na(city_name_simplemaps)==T)

hia_summary_gub_city_name_simplemaps_miss %>% 
  ggplot(aes(x=pop_cat_mean_val))+
  geom_histogram()

hia_summary_gub_city_name_simplemaps_miss %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n())

#One with lots of deaths prevented is missing
setwd(here("data-processed"))
load("gub.RData")
gub %>% 
  filter(ORIG_FID=="35930") %>% #it's in japan near Kure
  mapview()
#very close to a city called kure, japan
gub %>% 
  filter(ORIG_FID=="35926") %>% 
  mapview()



### Map-----
hia_summary_gub_geo=gub %>% 
  dplyr::select(-starts_with("area_km2")) %>%   #to avoid conflicts when merging
  left_join(hia_summary_gub,by="ORIG_FID") %>% 
  #this should always appear
  filter(is.na(pop_cat_mean_val)==F)

names(hia_summary_gub)
class(hia_summary_gub_geo$geometry)
hia_summary_gub_geo %>% 
  filter(is.na(city_name_either_source)==T) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  #  st_centroid() %>%  
  dplyr::select(contains("name"), contains("breaks")) %>% 
  mapview(zcol="pop_cat_breaks_gub_aue_ls")



## Cities: 1 million plus-----
#Among cities above 1,000,000 people, what are the top 10?
hia_summary_gub_1mil_plus = pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(ORIG_FID) %>% 
  hia_summarise() %>% 
  filter(pop_cat_mean_val_scaled_who>=1000000) 

hia_summary_gub_1mil_plus
setwd(here("data-processed"))
save(hia_summary_gub_1mil_plus, file="hia_summary_gub_1mil_plus.RData")

## Figures: cities--------
#Revised Jan 19, 2024 using non-accidental in place of all-cause
###Figure of top n cities by deaths prevented----
#make a function as I repeat this ggplot code several times
names(hia_summary_gub)
hia_summary_gub %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
  max_n_d_na_prev_std_who_per_100k_pop_max_over_9=max(n_d_na_prev_std_who_per_100k_pop_max_over_9,na.rm=T)
  )
max(hia_summary_gub$n_d_na_prev_std_who_per_100k_pop_max_over_9)
ggplot_top_n_gub_by_n_death_prev_per_100k = function(df){
  df %>% 
    ggplot(aes(x=n_d_na_prev_std_who_per_100k_pop_pt,
               y=city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop,
               #             y=city_name_country_name_ranked_by_n_d_na_prev_per_pop
    ))+
    geom_point(stroke=0,size=.5,color="transparent")+
    geom_pointrange(aes(xmin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                        xmax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
    scale_x_continuous(limits=c(0,250))+
    labs(
      x="Age-standardized\nnon-accidental death rate\n prevented\nper 100,000 population\n(adults 30+)",
      y="Largest city in GUB")+
    theme_bw(base_size = 12)
}
names(hia_summary_gub)
hia_summary_gub %>% 
  filter(is.na(city_name_either_source)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:120) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()



### Facet top n by city size---------
table(hia_summary_gub$pop_cat_breaks_gub_aue_ls)
hia_summary_gub %>% 
  filter(is.na(city_name_either_source)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:100) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()+
  facet_grid(
    cols = vars(pop_cat_breaks_gub_aue_ls))

### Top n, sep. figure for each city size--------
#This works okay, but is a bit unclear.
#Can try instead just a filter
pop_cat_breaks_gub_aue_ls_levels=hia_summary_gub %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n()) %>% 
  ungroup()
pop_cat_breaks_gub_aue_ls_levels
pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[1]
pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[2]
pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[3]
pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[4]
pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[5]

#### Pop dens cat 5-------
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  #filter to the first level, second level, etc.
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[5]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()

# comment Dec 7, 2023: seem to have lost CIs for some cities. Why?
#save for a panel plot. make it fairly narrow
#ah - because the max x-axis value was too low.
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_city_pop_cat_5.png", height=10, width=5)

names(hia_summary_gub)
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  #filter to the first level, second level, etc.
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[5]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  dplyr::select(
    city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20)





#### Pop dens cat 4-------
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[4]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_city_pop_cat_4.png", height=10, width=5)

hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  #filter to the first level, second level, etc.
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[4]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  dplyr::select(
#    city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20)


#### Pop dens cat 3-------
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[3]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_city_pop_cat_3.png", height=10, width=5)

#### Pop dens cat 2-------
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[2]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_city_pop_cat_2.png", height=10, width=5)

#### Pop dens cat 1-------
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[1]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot_top_n_gub_by_n_death_prev_per_100k()

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_city_pop_cat_1.png", height=10, width=5)


#### top values for in-text writing----
names(hia_summary_gub)
hia_summary_gub %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  #filter to the first level, second level, etc.
  filter(pop_cat_breaks_gub_aue_ls==pop_cat_breaks_gub_aue_ls_levels$pop_cat_breaks_gub_aue_ls[5]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    ORIG_FID,
    city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop, 
    starts_with("n_d_na_prev_std_who_per_100k_pop")) %>% 
  View()

### Top n in US--------
names(hia_summary_gub)
hia_summary_gub %>% 
  filter(country_name_en=="United States of America") %>% 
  filter(is.na(city_name_either_source)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:75) %>% #take top n
  ggplot(aes(x=n_d_na_prev_std_who_per_100k_pop_pt,
             y=city_name_country_name_admin_if_dupe_ranked_by_n_d_na_prev_per_pop,
             #             y=city_name_country_name_ranked_by_n_d_na_prev_per_pop
  ))+
  geom_point(stroke=0,size=.5,color="transparent")+
  geom_pointrange(aes(xmin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                      xmax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
  #  scale_x_continuous(limits=c(0,110))+
  labs(
    x="Estimated number of\nnon-accidental deaths prevented\nper 100,000 population",
    y="City name")+
  theme_bw(base_size = 8)


## maps-------
### map cities 1 mil plus-----
setwd(here("data-processed"))
load("gub.RData")
gub_hia_1mil_plus = gub %>% 
  left_join(hia_summary_gub_1mil_plus, by = "ORIG_FID") %>% 
  filter(pop_cat_mean_val_scaled_who >0)

library(viridis)
names(gub_hia_1mil_plus)
mv_gub_hia_1mil_plus=gub_hia_1mil_plus %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_na_prev_std_who_per_100k_pop_pt",
    zcol = "n_d_na_prev_std_who_per_100k_pop_pt")

mv_gub_hia_1mil_plus
object.size(mv_gub_hia_1mil_plus)
#and one to top 100 by deaths per 1k
names(gub_hia_1mil_plus)
setwd(here("data-processed"))
load("lookup_gub_city_name.RData")

### Map of top 100 by deaths prevented per capita, regardless of size----
hia_summary_gub %>% 
  filter(pop_cat_mean_val_scaled_who >0)

#which ones have missing city names and have pretty large pop?

#Now it's back to just a warning...
#July 2, 2024: this throws an error
# hia_summary_gub_geo %>% 
#   arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
#   slice(1:100) %>% 
#   filter(is.na(city_name_either_source)==T) %>% 
# #  st_centroid() %>% #so easier to see
#   st_point_on_surface() %>% 
#   mapview(zcol="n_d_na_prev_std_who_per_100k_pop_pt")

mv_gub_hia_1mil_plus_top_100=gub_hia_1mil_plus %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:100) %>% 
  #add name
  left_join(lookup_gub_city_info,by="ORIG_FID") %>% 
  dplyr::select(
    starts_with("ORIG_FID"),
    starts_with("city_n"), everything()) %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_na_prev_std_who_per_100k_pop_pt",
    zcol = "n_d_na_prev_std_who_per_100k_pop_pt")

mv_gub_hia_1mil_plus_top_100
#make them centroids for easier vis
mv_gub_hia_1mil_plus_top_100_centroid=gub_hia_1mil_plus %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:100) %>% 
  st_centroid() %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_na_prev_std_who_per_100k_pop_pt",
    zcol = "n_d_na_prev_std_who_per_100k_pop_pt")


mv_gub_hia_1mil_plus_top_100_centroid
## By biome and city-----
hia_summary_biome_gub= pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  group_by(biome_name_imp, ORIG_FID) %>% 
  hia_summarise()

hia_summary_biome_gub




### Number of population categories in each city / biome----
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


### Pop. distribution of cities
hia_summary_gub
names(hia_summary_gub)


## By population density category-----
#Main summary
setwd(here("data-processed"))
load("lookup_pop_cat_max_fac.RData")#load this look-up table for the figure
lookup_pop_cat_max_fac
names(pop_ndvi_gub_biome_tib)
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)
hia_summary_pop_cat_max_fac = pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  group_by(pop_cat_max_fac) %>% 
  hia_summarise() %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(pop_cat_max_fac!="0")#remove the lowest category - max 0

setwd(here("data-processed"))
save(hia_summary_pop_cat_max_fac,file="hia_summary_pop_cat_max_fac.RData")
names(hia_summary_pop_cat_max_fac)
hia_summary_pop_cat_max_fac %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  print(n=100)

hia_summary_pop_cat_max_fac %>%    
  dplyr::select(
    pop_cat_max_fac,
   n_d_na_prev_std_who_per_100k_pop_pt,
    n_d_na_prev_std_who_per_100k_pop_min_over_9,
    n_d_na_prev_std_who_per_100k_pop_max_over_9,
    
    n_d_ac_prev_crude_gbd_per_100k_pop_pt,
    n_d_ac_prev_crude_gbd_per_100k_pop_min_over_9,
    n_d_ac_prev_crude_gbd_per_100k_pop_max_over_9
  ) %>%  View()

#Dec 1, 2023: Here, Pier asks for the baseline number of deaths.
#What do they look like?
hia_summary_pop_cat_max_fac %>% 
  dplyr::select(starts_with("n_d_ac"))

### Figure: pop dens x age-standardized death rate---------
names(hia_summary_pop_cat_max_fac)

#define the upper limit of the y-axis
hia_summary_pop_cat_max_fac_y_upper_lim=max(
  hia_summary_pop_cat_max_fac$n_d_na_prev_std_who_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_pop_cat_max_fac_y_upper_lim

plot_n_d_na_prev_std_per_100k_x_pop_cat=hia_summary_pop_cat_max_fac %>% 
  ggplot(aes(x=pop_cat_max_fac_w_comma,# Note update Sep 28, 2023-commas
             y=n_d_na_prev_std_who_per_100k_pop_pt))+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                      ymax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_pop_cat_max_fac_y_upper_lim))+
  labs(
    x=
      "Population density\n (unscaled upper bound of LandScan category)\n of pixel per sq. km",
    y="Age-standardized\nnon-accidental\ndeath rate prevented\nper 100,000 population\n(adults 30+)"
    )+ 
  theme_bw(base_size=12)

plot_n_d_na_prev_std_per_100k_x_pop_cat

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_pop_cat.png", height=5, width=10)

## By pop. density & biome----
names(pop_ndvi_gub_biome_tib)
hia_summary_pop_cat_biome= pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  group_by(biome_name_imp, pop_cat_max_fac) %>% 
  hia_summarise() %>% 
  #I need this Oct 9 2023
  left_join(lookup_pop_cat_max_fac,by="pop_cat_max_fac")

hia_summary_pop_cat_biome %>% 
  ggplot(aes(x=pop_cat_max_fac, y=ndvi_mean))+
  geom_point(aes(colour = biome_name_imp))

names(hia_summary_pop_cat_biome)
hia_summary_pop_cat_biome %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  ggplot(aes(x=pop_cat_max_fac, y=ndvi_mean))+
  geom_point()+
  facet_grid(
    #use labeler to wrap text. brilliant.
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 8)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle=90))

## By biome-----
hia_summary_biome = pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(biome_name_imp) %>% 
  hia_summarise() 

setwd(here("data-processed"))
save(hia_summary_biome,file="hia_summary_biome.RData")
names(hia_summary_biome)

hia_summary_biome %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(biome_name_imp, 
                starts_with("n_d_na_prev_std_who_per_100k_pop")
  ) %>% View()

hia_summary_biome %>% 
  arrange(desc(pop_cat_mean_val_scaled_who_millions)) %>% 
  dplyr::select(biome_name_imp, pop_cat_mean_val_scaled_who_millions) %>% 
  mutate(
    pop_cat_mean_val_scaled_who_millions_total=sum(pop_cat_mean_val_scaled_who_millions),
    pop_cat_mean_val_scaled_who_millions_cumsum=cumsum(pop_cat_mean_val_scaled_who_millions),
    pop_cat_mean_cumsum_share_of_total=pop_cat_mean_val_scaled_who_millions_cumsum/
      pop_cat_mean_val_scaled_who_millions_total
  )

hia_summary_biome %>% 
  dplyr::select(biome_name_imp, starts_with("ndvi_diff")) %>% View()
### Figure: biome x age-standardized death rate---------

#define the upper limit of the y-axis
hia_summary_biome_y_upper_lim=max(
  hia_summary_biome$n_d_na_prev_std_who_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_biome_y_upper_lim
plot_n_d_na_prev_std_per_100k_x_biome=hia_summary_biome %>% 
  filter(is.na(biome_name_imp)==F) %>% 
  ggplot(aes(x=biome_name_imp,# Note update Sep 28, 2023-commas
             y=n_d_na_prev_std_who_per_100k_pop_pt))+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                      ymax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_biome_y_upper_lim))+
  labs(
    x="Biome",
    y="Age-standardized\nnon-accidental\ndeath rate prevented\nper 100,000 population\n(adults 30+)"
  )+ 
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=50, hjust=1))


plot_n_d_na_prev_std_per_100k_x_biome

setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_biome.png", height=5, width=10)

#hia_summary_biome %>% View()
#Feb 21 2023: 330 pm. Biome was modified
# There are a few missing biome. Why?
#Okay, so all of the missings correspond to coastal cities.
#So the missings must have ocurred during the vector to raster conversion.
#That is, there are some pixels that correspond to a city that do not correspond
#to a biome because the pixel corresponding to the city might include
#some of the coastal area
#Note the entire cities aren't missing, just some pixels.
#also see notes on this above.
#intersting. they're all coastal, so something strange is happening with coastal cities.
#hmm, I wonder if my country file isn't picking these up?
#Update Feb 23 9 am - the higher-resolution country file fixed it! 
biome_miss = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(biome_name_imp)==TRUE)

#let's look which ORIG-fid they're a part of
setwd(here("data-processed"))
#load("gub.RData")
load("lookup_gub_orig_fid_geo.RData")#from ~read-gub.R

orig_fid_biome_miss = biome_miss %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  #take a random sample so it's easier to map
  slice_sample(prop=.1) 

# orig_fid_biome_miss %>% mapview()
# orig_fid_biome_miss %>% plot()


#What biome is egypt? Desert I assume
#Actually lots of flooded grasslands and savannas..
pop_ndvi_gub_biome_tib
table(pop_ndvi_gub_biome_tib$country_name_en)
pop_ndvi_gub_biome_tib %>% 
  filter(country_name_en=="Egypt") %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n())
names(pop_ndvi_gub_biome_tib)

#what about Central African Republic?
pop_ndvi_gub_biome_tib %>% 
  filter(country_name_en=="Central African Republic") %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n())
pop_ndvi_gub_biome_tib %>% 
  filter(country_name_en=="South Sudan") %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n())
pop_ndvi_gub_biome_tib %>% 
  filter(country_name_en=="Indonesia") %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n())
n_distinct(pop_ndvi_gub_biome_tib$biome_name_imp)


## By income category------
#use this look-up table created here:
#scripts/read-boundaries-states-countries.R
setwd(here("data-processed"))
load("lookup_income_grp_5_rename.RData")#loads the new name for income group
lookup_income_grp

hia_summary_income_grp = pop_ndvi_gub_biome_tib %>% 
  #in the original country dataset, name is "name_en", but in the merged
  #file with UN data, I changed it to "country_name_en".
  #This works
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(income_grp) %>% 
  hia_summarise() %>% 
  left_join(lookup_income_grp_5_rename, by = "income_grp")

setwd(here("data-processed"))
save(hia_summary_income_grp,file="hia_summary_income_grp.RData")

names(hia_summary_income_grp)
hia_summary_income_grp %>% 
  dplyr::select(
    contains("income"),
   n_d_na_prev_std_who_per_100k_pop_pt,
    n_d_na_prev_std_who_per_100k_pop_min_over_9,
    n_d_na_prev_std_who_per_100k_pop_max_over_9,
    
    n_d_ac_prev_crude_gbd_per_100k_pop_pt,
    n_d_ac_prev_crude_gbd_per_100k_pop_min_over_9,
    n_d_ac_prev_crude_gbd_per_100k_pop_max_over_9
  ) %>% 
  View()

### Figure: WB income group x age-standardized death rate---------
load("lookup_income_grp_5_rename.RData")
names(hia_summary_income_grp)
#set the upper limit of the y-axis
hia_summary_income_grp_y_upper_lim=max(
  hia_summary_income_grp$n_d_na_prev_std_who_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_income_grp_y_upper_lim
plot_n_d_na_prev_std_per_100k_x_income_grp=hia_summary_income_grp %>% 
  filter(is.na(income_grp)==F) %>% 
  ggplot(aes(x=income_grp_5_rename, #add dash in the name
             y=n_d_na_prev_std_who_per_100k_pop_pt #Sep 28, 2023 to include commas 
          )
  )+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                      ymax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_income_grp_y_upper_lim))+
  labs(
    y="Age-standardized\nnon-accidental\ndeath rate prevented\nper 100,000 population\n(adults 30+)",
    x="World Bank Income Group")+
  theme_bw(base_size=12)+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=10))


plot_n_d_na_prev_std_per_100k_x_income_grp
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_income_grp.png", height=5, width=10)

## By gini coefficient of country-------
lookup_country_name_en_iso3_alpha_code
gini_by_country

names(pop_ndvi_gub_biome_tib)
hia_summary_gini = pop_ndvi_gub_biome_tib %>% 
  #the gini coefficient data only has the 3-letter code, not the 
  #country name, so do first
  left_join(lookup_country_name_en_iso3_alpha_code,by="country_name_en") %>% 
  left_join(gini_by_country, by = c("iso3_alpha_code")) %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(gini_quartile) %>% 
  hia_summarise() 

names(hia_summary_gini)
hia_summary_gini %>% 
  dplyr::select(contains("gini"),
               n_d_na_prev_std_who_per_100k_pop_pt,
                n_d_na_prev_std_who_per_100k_pop_min_over_9,
                n_d_na_prev_std_who_per_100k_pop_max_over_9,
                
                n_d_ac_prev_crude_gbd_per_100k_pop_pt,
                n_d_ac_prev_crude_gbd_per_100k_pop_min_over_9,
                n_d_ac_prev_crude_gbd_per_100k_pop_max_over_9
  ) %>%  View()
                
setwd(here("data-processed"))
save(hia_summary_gini,file="hia_summary_gini.RData")

## By country----
setwd(here("data-processed"))
load("lookup_income_grp.RData")
#how many pixels in bottom two tertiles?
pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>% 
  nrow()
pop_ndvi_gub_biome_tib %>% 
  nrow()

0.067/0.122

hia_summary_country = pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(country_name_en) %>% 
  hia_summarise() %>% 
  ungroup() %>% 
  #make country a factor ranked byn_d_na_prev_std_who_per_100k_pop_pt
  mutate(
    #Jan 17, 2024
    #consider updating name here. this is in terms of gbd
    country_name_ranked_by_n_d_na_prev_per_pop_int=as.factor(country_name_en),
    country_name_ranked_by_n_d_na_prev_per_pop=fct_reorder(
      country_name_ranked_by_n_d_na_prev_per_pop_int,
     n_d_na_prev_std_who_per_100k_pop_pt,
      .na_rm=TRUE)
  ) %>% 
  #I might want these to come along
  left_join(lookup_income_grp, by = c("country_name_en"="name_en"))  

#hia_summary_country %>% View()

#save for Rmarkdown
setwd(here("data-processed"))
save(hia_summary_country,file="hia_summary_country.RData")
# deaths prevented per population
hia_summary_country %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
#  View()
  print(n=100)

#deaths prevented total
names(hia_summary_country)
hia_summary_country %>% 
  arrange(desc(n_d_na_prev_crude_who_mean_pt)) %>% 
  print(n=100)
#yea, not so informative, as it's simply the most populous countries

###Figure of top n countries by deaths prevented----
#this might work if I put countries on the y-axis?
nrow(hia_summary_country)#176 countries
nrow(hia_summary_country)/3

names(hia_summary_country)
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot(aes(color=income_grp,
             x=n_d_na_prev_std_who_per_100k_pop_pt,
             y=country_name_ranked_by_n_d_na_prev_per_pop))+
  geom_point(size=1)+
  geom_pointrange(aes(xmin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                      xmax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
  scale_x_continuous(limits=c(0,200))+
  scale_color_brewer(type="qual",
                     #Set2 also works but doesn't portray the sequential nature
                     #The default sequential palettes have too much white
                     #towards the end
                     palette = "Set2",
                     name="Income Group")+
  labs(
    x="Estimated annual\nnon-accidental deaths prevented\nper 100,000 population\n(adults 30+)",
    y="Country")+
  theme_bw(base_size = 12)

plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_n.png", height=15, width=10)
nrow(hia_summary_country)
table(hia_summary_country$income_grp_5_rename)

### top n countries by income group--------
#Let's do by income group, actually
income_grp_5_rename_levels =hia_summary_country %>% 
  group_by(income_grp_5_rename) %>% 
  summarise(n=n()) %>% 
  ungroup()

income_grp_5_rename_levels
income_grp_5_rename_levels$income_grp_5_rename[1]



table(hia_summary_country$income_grp_5_rename)
#make a function to reduce copy/paste
ggplot_top_n_country_by_n_death_prev_per_100k=function(df){
  df %>% 
    ggplot(aes(x=n_d_na_prev_std_who_per_100k_pop_pt,
               y=country_name_ranked_by_n_d_na_prev_per_pop))+
    geom_point(size=1)+
    geom_pointrange(aes(xmin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                        xmax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
    scale_x_continuous(limits=c(0,200))+
#    scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=30))+
    labs(
      x="Age-standardized\nnon-accidental\ndeath rate\n prevented\nper 100,000 population\n(adults 30+)",
      y="Country")+
    theme_bw(base_size = 12)
}

#### values for in-text results---------
#Adding these to text Jan 22, 2024
hia_summary_country%>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  group_by(income_grp_5_rename) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  slice(1:2) %>% 
  dplyr::select(
    country_name_en, 
    income_grp_5_rename,
    starts_with("n_d_na_prev_std_who_per_100k_pop")) %>% 
  print(n=30)

#### top countries: high income OECD--------
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[1]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k


plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_wb_1.png", height=6, width=4)

hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[1]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20)

#### top countries: high income non-OECD--------
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[2]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k


plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_wb_2.png", height=6, width=4)

hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[2]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20)

#### top countries: middle income--------
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[3]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k


plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_wb_3.png", height=6, width=4)

hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[3]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20) 

#### top countries: lower middle income--------
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[4]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k


plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_wb_4.png", height=6, width=4)

hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[4]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20)# %>% View()

#Is Egypt highest overall?
hia_summary_country %>% 
#  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[4]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20) 
#no DRC is


#### top countries: lower income--------
plot_n_d_na_prev_std_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[5]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k


plot_n_d_na_prev_std_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_top_wb_5.png", height=6, width=4)

hia_summary_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[5]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  dplyr::select(
    country_name_en,
    contains("n_d_na_prev_std_who_per_100k_pop")
  ) %>% 
  print(n=20) %>% View()

## Overall HIA results for a table-----
table(pop_ndvi_gub_biome_tib$ndvi_tertile)
names(pop_ndvi_gub_biome_tib)
hia_summary_overall= pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  hia_summarise() %>% 
  mutate(prop_na_n_deaths_of_total=
           n_d_na_prev_crude_who_mean_pt/
           n_d_na_0_crude_who_mean
           )


setwd(here("data-processed"))
save(hia_summary_overall,file="hia_summary_overall.RData")
names(hia_summary_overall)
hia_summary_overall %>% 
  dplyr::select(contains("prop_na"))
hia_summary_overall %>% 
  dplyr::select(
   n_d_na_prev_std_who_per_100k_pop_pt,
    n_d_na_prev_std_who_per_100k_pop_min_over_9,
    n_d_na_prev_std_who_per_100k_pop_max_over_9,
   
   n_d_na_prev_crude_who_per_100k_pop_pt,
   n_d_na_prev_crude_who_per_100k_pop_min_over_9,
   n_d_na_prev_crude_who_per_100k_pop_max_over_9
                ) %>% 
  View()

#comment: crude deaths seem messed up. what about the WHO versions?
hia_summary_overall %>% 
  dplyr::select(
    n_d_ac_prev_std_who_per_100k_pop_pt,
    n_d_ac_prev_std_who_per_100k_pop_min_over_9,
    n_d_ac_prev_std_who_per_100k_pop_max_over_9,
    
    n_d_ac_prev_crude_who_per_100k_pop_pt,
    n_d_ac_prev_crude_who_per_100k_pop_min_over_9,
    n_d_ac_prev_crude_who_per_100k_pop_max_over_9,
  ) %>% 
  View()
#interesting. who looks fine, but GBD is like an order of magnitude too small. strange.
#update: I fixed it.

#what about the crude number of deaths prevented?
names(hia_summary_overall)
hia_summary_overall %>% 
  dplyr::select(
    
    n_d_ac_prev_crude_gbd_mean_pt,
    n_d_ac_prev_crude_gbd_min_over_9,
    n_d_ac_prev_crude_gbd_max_over_9,
    
    n_d_ac_prev_crude_who_mean_pt,
    n_d_ac_prev_crude_who_min_over_9,
    n_d_ac_prev_crude_who_max_over_9,
  ) %>% 
  View()

#how much higher is GBD than WHO?
hia_summary_overall %>% 
  dplyr::select(
    n_d_ac_prev_crude_gbd_mean_pt,
    n_d_ac_prev_crude_who_mean_pt) %>% 
  mutate(gbd_vs_who_n_d_ac_prev_crude=
           n_d_ac_prev_crude_gbd_mean_pt/n_d_ac_prev_crude_who_mean_pt
           )

#non-accidental cause
names(hia_summary_overall)
hia_summary_overall %>% 
  dplyr::select(
    n_d_na_prev_std_who_per_100k_pop_pt,
    n_d_na_prev_std_who_per_100k_pop_min_over_9,
    n_d_na_prev_std_who_per_100k_pop_max_over_9,
    
    n_d_na_prev_crude_who_per_100k_pop_pt,
    n_d_na_prev_crude_who_per_100k_pop_min_over_9,
    n_d_na_prev_crude_who_per_100k_pop_max_over_9,
    
    n_d_na_prev_std_who_mean_pt,
    n_d_na_prev_std_who_min_over_9,
    n_d_na_prev_std_who_max_over_9,
    
    n_d_na_prev_crude_who_mean_pt,
    n_d_na_prev_crude_who_min_over_9,
    n_d_na_prev_crude_who_max_over_9
  ) %>% 
  View()

#what share of global deaths is this preventing?
hia_summary_overall$n_d_0_crude_mean
hia_summary_overall$n_d_ac_prev_crude_mean_pt
hia_summary_overall$prop_n_d_ac_prev_crude
names(hia_summary_overall)
hia_summary_overall %>% 
  dplyr::select(starts_with("pop_cat_")) %>% 
  dplyr::select(contains("millions"))

#and now our target pop is 1.8 billion, which is about right.

names(hia_summary_overall)
hia_summary_overall %>% 
  dplyr::select(starts_with("n_d_ac_prev_std_who_per_100k_pop"))

hia_summary_overall %>% 
  dplyr::select(starts_with("n_d_na_prev_std_who_per_100k_pop"))

hia_summary_overall %>% 
  dplyr::select(starts_with("n_d_na_prev_crude_who_per_100k_pop"))

hia_summary_overall %>% 
  dplyr::select(starts_with("n_d_na_prev_crude_who_mean_pt"))

#Pier's question
hia_summary_overall %>% 
  dplyr::select(starts_with("n_d_na_0"))



#lower bound is basically same as october revision, but upper
#bound is much lower and more sensible. good job.
hia_summary_overall %>% 
  dplyr::select(
    starts_with("n_d_ac_prev_mean_pt"),
    starts_with("n_d_ac_prev_min_over_9"),
    starts_with("n_d_ac_prev_max_over_9"))


# Descriptive (non-HIA) results---------
#Note some of these results rely on the objects created above, 
#which is why they are located here in the data
## Univariate summary of GUB: NDVI and pop. density----

names(hia_summary_gub)
hia_summary_gub %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    ndvi_mean_mean = mean(ndvi_mean, na.rm=TRUE),
    ndvi_diff_mean_mean = mean(ndvi_diff_mean, na.rm=TRUE),
    ndvi_diff_mean_min = min(ndvi_diff_mean, na.rm=TRUE),
    ndvi_diff_mean_max = max(ndvi_diff_mean, na.rm=TRUE)
  )

summary(hia_summary_gub$ndvi_mean)
sd(hia_summary_gub$ndvi_mean)
library(scales)
hia_summary_gub %>% 
  ggplot(aes(ndvi_mean))+
  geom_histogram()+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks=seq(0,1,.1))+
  ylab("Number of\n urban areas")+
  xlab("Mean NDVI, all pixels")+
  theme_bw(base_size=12)


summary(hia_summary_gub$ndvi_diff_mean)
sd(hia_summary_gub$ndvi_diff_mean)
hia_summary_gub %>% 
  ggplot(aes(ndvi_diff_mean))+
  geom_histogram()+
  scale_y_continuous(labels = comma)+
  ylab("Number of\n urban areas")+
  xlab("Mean difference in NDVI,\n target (83rd percentile)-baseline value")+
  theme_bw(base_size=12)

summary(hia_summary_gub$n_distinct_pop_cat_not_scaled)

## Number of unique pop categories------
#the number of unique pop. categories in the cities
summary(hia_summary_gub$n_distinct_pop_cat_not_scaled)
hia_summary_gub %>% 
  ggplot(aes(n_distinct_pop_cat_not_scaled))+
  geom_histogram(bins=9)+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks=seq(1,9,1))+
  ylab("Number of\n urban areas")+
  xlab("Number of distinct population categories\n among constituent pixels")+
  theme_bw()

summary(hia_summary_gub$area_km2)
hia_summary_gub %>% 
  filter(area_km2>100000000)#2238

hia_summary_gub %>% 
  ggplot(aes(area_km2))+
  geom_histogram()+
  scale_y_continuous(labels = comma)+
  #  scale_x_continuous(breaks=seq(1,9,1))+
  ylab("Number of urban areas")+
  xlab("Number of distinct population categories\n among constituent pixels")+
  theme_bw()


## NDVI x pop. density overall-----
library(scales)
### NDVI x pop density stratied by biome (appendix)
#Use default boxplot settings to get distribution
#Make this an object so I can use below
pop_ndvi_boxplot_fun = function(df){
  df %>% 
    filter(is.na(biome_name_imp)==FALSE) %>% 
    ggplot(aes(x=pop_cat_max_fac_w_comma, y=ndvi_2019))+
    geom_boxplot(
      outlier.size =  .01,
      outlier.colour = "gray",
      varwidth=TRUE,
    )+
    #add mean - see Evernote for notes
    stat_summary(fun=mean, geom="point", shape=23, size=2)+
    xlab(
      "Population density\n (unscaled upper bound of LandScan category)\n of pixel per sq. km"
    )+ 
    ylab("NDVI")
}

pop_ndvi_boxplot_fun_no_varwidth = function(df){
  df %>% 
    filter(is.na(biome_name_imp)==FALSE) %>% 
    ggplot(aes(x=pop_cat_max_fac_w_comma, y=ndvi_2019))+
    geom_boxplot(
      outlier.size =  .01,
      outlier.colour = "gray"
    )+
    #add mean - see Evernote for notes
    stat_summary(fun=mean, geom="point", shape=23, size=2)+
    xlab(
      "Population density\n (unscaled upper bound of LandScan category)\n of pixel per sq. km"
    )+ 
    ylab("NDVI")
}

lookup_pop_cat_max_fac
plot_ndvi_x_pop_pop_cat_max_fac=pop_ndvi_gub_biome_tib %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  pop_ndvi_boxplot_fun()+
  theme_bw(base_size=12)

#save with uniform dimensions
#Note this is going into a panel, so keep height rather short
setwd(here("plots"))
ggsave("plot_ndvi_x_pop_pop_cat_max_fac.png", height=5, width=10)


#facet by biome
#Separate 7 and 7
biome_number = pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(biome_number=row_number()) %>% 
  dplyr::select(-n)

biome_first_7 = biome_number %>% 
  filter(biome_number<=7)
biome_last_7=biome_number %>% 
  filter(biome_number>7)

names(pop_ndvi_gub_biome_tib)
### NDVI x pop. density x biome---------
#First 7. Note not varying width of plot per n obs,
#as it's too difficult to see.
pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(biome_number<=7) %>% 
  pop_ndvi_boxplot_fun_no_varwidth()+
  facet_grid(
    #use labeler to wrap text. brilliant.
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 10)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle=90))
#second 7 biomes
pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(biome_number>7) %>% 
  pop_ndvi_boxplot_fun_no_varwidth()+
  facet_grid(
    #use labeler to wrap text. brilliant.
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 10)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle=90))


table(pop_ndvi_gub_biome_tib$biome_name_imp)
pop_ndvi_gub_biome_tib %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  ggplot(aes(x=pop_cat_max_fac, y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  #add mean - see Evernote for notes
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab("Population density (unscaled upper bound of category)\n of pixel per sq. km")+ 
  ylab("NDVI")+
  theme_bw()+
  facet_grid(
    #use labeler to wrap text. brilliant.
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 8)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle=90))

# Distribution of total population of cities

## NDVI x biome--------
#imp for imputed. see analysis-global.
table(pop_ndvi_gub_biome_tib$biome_name_imp)

plot_ndvi_x_biome_name_imp= pop_ndvi_gub_biome_tib %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  #Oct 5, 2023: Note I'm reordering biome by its NDVI level. Not sure how ggplot
  #figured out how to use the mean, but it did...
  #https://stackoverflow.com/questions/40199274/
  ggplot(aes(x=fct_reorder(biome_name_imp, ndvi_2019, .desc=TRUE), y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab(
    "Biome"
  )+ 
  ylab("NDVI")+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=70, hjust=1))

#save with uniform dimensions
#Note this is going into a panel, so keep height rather short
setwd(here("plots"))
ggsave("plot_ndvi_x_biome_name_imp.png", height=5, width=10)

## NDVI x income group-------
setwd(here("data-processed"))
load(file="lookup_income_grp.RData")
lookup_income_grp
plot_ndvi_x_income_grp=pop_ndvi_gub_biome_tib %>% 
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  filter(is.na(income_grp)==F) %>% 
  #Oct 5, 2023: Note I'm reordering biome by its NDVI level. Not sure how ggplot
  #figured out how to use the mean, but it did...
  #https://stackoverflow.com/questions/40199274/
  ggplot(aes(x=income_grp_5_rename, y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab(
    "Income group"
  )+ 
  ylab("NDVI")+
  #This wraps the x-axis labels. Very useful. See
  #https://stackoverflow.com/questions/61782882/
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=10))+
  theme_bw(base_size=12)

plot_ndvi_x_income_grp
setwd(here("plots"))#note it takes the last plot.
ggsave("plot_ndvi_x_income_grp.png", height=5, width=10)



## Static maps------
#see alternative script so I can source this summary-global script
#~global-ndvi-pop/scripts/summary-global-maps.R

## Figure illustrating process for one city------
#See separate script:
#~global-ndvi-pop/scripts/figure-illustrate-method.R

## Tables
# Tables for paper

#See rmarkdown. easier to customize output.
#global-ndvi-pop/docs-for-word/tables-main-text.Rmd
hia_summary_pop_cat_max_fac
names(hia_summary_pop_cat_max_fac)




# Look at a table of biome vs corrected biome
names(pop_ndvi_gub_biome_tib)
pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp, BIOME_NAME) %>% 
  summarise(n=n()) %>%
  print(n=100) #cool. as expected.

# Broad results for text------
## Number of GUBs----
hia_summary_gub %>% 
  filter(is.na(ORIG_FID)==F) %>% 
  nrow()
nrow(hia_summary_gub)
n_distinct(hia_summary_gub$ORIG_FID)
##Number of countries
hia_summary_country %>% 
  filter(is.na(country_name_en)==F) %>% 
  nrow()
nrow(hia_summary_country)
n_distinct(hia_summary_country$country_name_en)

## number of biomes-------
hia_summary_biome %>% 
  filter(is.na(biome_name_imp)==F) %>% 
  nrow()

## number of pixels in bottom two tertiles----
pop_ndvi_gub_biome_tib %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  nrow()

# Discussion section - exploring claim that pop. doesn't matter for per-pop values------
#The question is - do these vary? I think they will all be the same
#July 2, 2024: I'm getting an issue
names(hia_summary_pop_cat_max_fac)
# hia_summary_pop_cat_max_fac %>% 
#   dplyr::select(
#     pop_cat_max_fac,
#     n_d_na_prev_crude_who_per_100k_pop_min_pt,
#     n_d_na_prev_crude_who_per_100k_pop_mean_pt,
#     n_d_na_prev_crude_who_per_100k_pop_max_pt)
#confirmed - no variation.

#is it the same for GUBs?
# hia_summary_gub %>% 
#   dplyr::select(
#     ORIG_FID,
#     n_d_ac_prev_per_pop_max_pt,
#     n_d_ac_prev_per_pop_mean_pt,
#     n_d_ac_prev_per_pop_min_pt)
#interesting...not exactly the same if we use GUBs
#okay...so that means that the proof might be telling us something
#they're the same as long as the top expression has independence
#between the population r.v. and the r.v. of baseline rate*PAF
# hia_summary_country %>% 
#   dplyr::select(
#     country_name_en,
#     n_d_ac_prev_per_pop_max_pt,
#     n_d_ac_prev_per_pop_mean_pt,
#     n_d_ac_prev_per_pop_min_pt)
#again - they're close but not identical.
#okay, that's good...means there's some use in running the different
#values through

#what about by country?
#are these the same? 
# hia_summary_pop_cat_max_fac %>% 
#   dplyr::select(
#     pop_cat_max_fac,
#     n_d_ac_prev_per_pop_mean_pt,
#     n_d_ac_prev_per_pop_mean_pt_alt_calc)
#no they're not, but what about this

#ah ha! That was the reason it wasn't adding up
#They're the same apart from rounding error...almost
#except also for some issues in the high-pop zone
# hia_summary_pop_cat_max_fac %>% 
#   dplyr::select(
#     pop_cat_max_fac,
#     n_d_ac_prev_per_pop_mean_pt, starts_with("paf_rate"))

