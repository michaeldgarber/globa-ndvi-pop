# Summary----
#March 10th, 2023 
#Breaking this off from the main script so that I can source it

library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
library(sf)
#Load data from 
#~scripts/analysis-global.R

setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib.RData")
source(here("scripts", "analysis-functions.R"))

# Look at a table of biome vs corrected biome
names(pop_ndvi_gub_biome_tib)
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

names(hia_summary_gub)
hia_summary_gub %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt))

hia_summary_gub %>% 
  dplyr::select(starts_with("ORIG"), starts_with("pop")) %>% 
  arrange(pop_cat_mean_val_scaled) %>% 
  print(n=100)

#hia_summary_gub %>%  View()

#Feb 22, 2023: 
#Why are there still some with zero values for the scaled population?
# I assume these are the cities without UN data and thus without a scale factor.
#Let's check.
setwd(here("data-processed"))
load("lookup_gub_geoname_id.RData")
#The others should load as long as this has been run:
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups
hia_summary_gub %>% 
  left_join(lookup_gub_geoname_id, by = "ORIG_FID") %>% 
  left_join(lookup_geoname_id_city_name, by = "geoname_id") %>% 
  left_join(lookup_geoname_id_country_name, by = "geoname_id") %>% 
  dplyr::select(
    starts_with("ORIG"), 
    starts_with("geoname"),
    starts_with("city"),
    starts_with("country"),
    starts_with("pop")
  ) %>% 
  arrange(pop_cat_mean_val_scaled)  
  #okay, they're all in china. can I map them?

setwd(here("data-processed"))
load("lookup_gub_orig_fid_geo.RData")
hia_summary_gub %>% 
  filter(pop_cat_mean_val_scaled<5) %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  mapview()


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

library(viridis)
names(gub_hia_1mil_plus)
mv_gub_hia_1mil_plus=gub_hia_1mil_plus %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_prev_per_1k_pop_pt",
    zcol = "n_d_prev_per_1k_pop_pt")

mv_gub_hia_1mil_plus
object.size(mv_gub_hia_1mil_plus)
#and one to top 100 by deaths per 1k
names(gub_hia_1mil_plus)
setwd(here("data-processed"))
load("lookup_gub_city_name.RData")
mv_gub_hia_1mil_plus_top_100=gub_hia_1mil_plus %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  slice(1:100) %>% 
  #add name
  left_join(lookup_gub_city_name, by = "ORIG_FID") %>% 
  dplyr::select(
    starts_with("ORIG_FID"),
    starts_with("city_n"), everything()) %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_prev_per_1k_pop_pt",
    zcol = "n_d_prev_per_1k_pop_pt")

mv_gub_hia_1mil_plus_top_100
#make them centroids for easier vis
mv_gub_hia_1mil_plus_top_100_centroid=gub_hia_1mil_plus %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  slice(1:100) %>% 
  st_centroid() %>% 
  mapview(
    lwd=.1,
    col.regions = viridis_pal(option = "plasma"),
    layer.name = "n_d_prev_per_1k_pop_pt",
    zcol = "n_d_prev_per_1k_pop_pt")


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



## Summarize by country----
hia_summary_country = pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en) %>% 
  hia_summarise() 

hia_summary_country %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  print(n=100)

## Summarize by income category------
#use this look-up table created here:
#scripts/read-boundaries-states-countries.R
hia_summary_income_grp = pop_ndvi_gub_biome_tib %>% 
  #in the original country dataset, name is "name_en", but in the merged
  #file with UN data, I changed it to "country_name_en".
  #This works
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  group_by(income_grp) %>% 
  hia_summarise() 

#hia_summary_income_grp %>%  View()




## Summarize by population density category-----
#Main summary
names(pop_ndvi_gub_biome_tib)
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)
hia_summary_pop_cat_max_fac = pop_ndvi_gub_biome_tib %>% 
  group_by(pop_cat_max_fac) %>% 
  hia_summarise()
names(hia_summary_pop_cat_max_fac)
hia_summary_pop_cat_max_fac %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  print(n=100)

#hia_summary_pop_cat_max_fac %>%    View()


#plot average NDVI vs pop density
hia_summary_pop_cat_max_fac %>% 
  ggplot(aes(x=pop_cat_max_fac, y=ndvi_mean))+
  geom_point()

# hia_summary_pop_cat_max_fac %>% 
#   arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
#   View()

## Summarize by pop. density & biome----
names(pop_ndvi_gub_biome_tib)
hia_summary_pop_cat_biome= pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp, pop_cat_max_fac) %>% 
  hia_summarise()

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

# Tables and figures for paper------
## Initial description-----
### Univariate summary of GUB: NDVI and pop. density----

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

#### Number of unique pop categories------
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


### NDVI x pop. density overall-----
library(scales)
### NDVI x pop density stratied by biome (appendix)
#Use default boxplot settings to get distribution
#Make this an object so I can use below
pop_ndvi_boxplot_fun = function(df){
  df %>% 
    filter(is.na(biome_name_imp)==FALSE) %>% 
    ggplot(aes(x=pop_cat_max_fac, y=ndvi_2019))+
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
    ggplot(aes(x=pop_cat_max_fac, y=ndvi_2019))+
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

pop_ndvi_gub_biome_tib %>% 
  pop_ndvi_boxplot_fun()+
  theme_bw()

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
#First 7. Note not varying width of plot per n obs,
#as it's too difficult to see.
pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
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

