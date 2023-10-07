# Summary----#
#March 10th, 2023 
#Breaking this off from the main script so that I can source it
#Rebvised Sep 29, 2023 - working on figures for main text

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

# Descriptive (non-HIA) results---------
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

# HIA results-----

## By city-----
#The others should load as long as this has been run:
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups
setwd(here("data-processed"))

load("lookup_city_gub_pop_cat_breaks.RData")
load("lookup_gub_geoname_id.RData")
nrow(lookup_gub_geoname_id)
n_distinct(lookup_gub_geoname_id$ORIG_FID)
load("lookup_orig_fid_country_name_en.RData")#analysis-global
nrow(lookup_orig_fid_country_name_en)
n_distinct(lookup_orig_fid_country_name_en$ORIG_FID)
hia_summary_gub= pop_ndvi_gub_biome_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() %>% 
  left_join(lookup_gub_geoname_id, by = "ORIG_FID") %>% #add city name
  left_join(lookup_geoname_id_city_name, by = "geoname_id") %>% 
  #add country name based on geoname_id; note geoname_id is sometimes missing
  left_join(lookup_geoname_id_country_name, by = "geoname_id") %>%  #add country name
  left_join(lookup_orig_fid_country_name_en,by="ORIG_FID") %>% #adds based on vector data
  #Sep 29, 2023
  #add the population breaks at the city level as well for possible classification
  left_join(lookup_city_gub_pop_cat_breaks,by="ORIG_FID") %>% 
  
  #Oct 6, 2023:
  #add simplemaps citynames to see if it helps with missing city names
  left_join(lookup_simplemaps_among_missing_geonames,by="ORIG_FID") %>% 
  ungroup() %>% 
  mutate(
    #a city name for either source
    city_name_either_source=case_when(
      is.na(city_name_geonames)==T~city_name_simplemaps,
      TRUE ~city_name_geonames
    ),
    #both sources are missing
    city_name_miss_both_sources=case_when(
      is.na(city_name_either_source)==T~1,
      TRUE~0),
  #make city name a factor ranked by deaths prevented.
  #Do so here rather than in the ggplot code
    city_name_geonames_ranked_by_n_d_prev_per_pop_int=as.factor(city_name_geonames),
    city_name_geonames_ranked_by_n_d_prev_per_pop=fct_reorder(
      city_name_geonames_ranked_by_n_d_prev_per_pop_int,
      n_d_prev_per_100k_pop_pt,
      na.rm=TRUE)
  ) %>% 
  #try also a row number sorting
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  mutate(
    order=row_number()
  )



nrow(hia_summary_gub)
n_distinct(hia_summary_gub$ORIG_FID)
names(hia_summary_gub)
hia_summary_gub %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  dplyr::select(ORIG_FID, starts_with("city_name"), starts_with("n_d_prev_per_1k_pop_pt")) %>% 
  View()

hia_summary_gub %>% 
  dplyr::select(starts_with("ORIG"), starts_with("pop")) %>% 
  arrange(pop_cat_mean_val_scaled) %>% 
  print(n=100)

#hia_summary_gub %>%  View()

## checks----
#Feb 22, 2023: 
#Why are there still some with zero values for the scaled population?
# I assume these are the cities without UN data and thus without a scale factor.
#Let's check.
setwd(here("data-processed"))
load("lookup_gub_geoname_id.RData")
hia_summary_gub %>% 
  dplyr::select(
    starts_with("ORIG"), 
    starts_with("geoname"),
    starts_with("city"),
    starts_with("country"),
    starts_with("pop")
  ) %>% 
  arrange(pop_cat_mean_val_scaled)  
#many seem to be in China. Let's map these cities with zero population.

setwd(here("data-processed"))
load("lookup_gub_orig_fid_geo.RData")
hia_summary_gub %>% 
  filter(pop_cat_mean_val_scaled<5) %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  mapview()

### GUBs with missing city name (either source)------
#how many?
table(hia_summary_gub$city_name_miss_both_sources)

#Most are pretty small, but not all of them.
gubs_missing_city_name_geonames=hia_summary_gub %>% 
  filter(is.na(city_name_geonames)==T)

gubs_missing_city_name_geonames %>% 
  ggplot(aes(x=pop_cat_mean_val))+
  geom_histogram()

gubs_missing_city_name_geonames %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n())

#One with lots of deaths prevented is missing
gub %>% 
  filter(ORIG_FID=="35930") %>% #it's in japan near Kure
  mapview()
#very close to a city called kure, japan
gub %>% 
  filter(ORIG_FID=="35926") %>% 
  mapview()



### Map-----
hia_summary_gub_geo=gub %>% 
  dplyr::select(-area_km2) %>%   #to avoid conflicts when merging
  left_join(hia_summary_gub,by="ORIG_FID") %>% 
  #this should always appear
  filter(is.na(pop_cat_mean_val)==F)

names(hia_summary_gub)
hia_summary_gub_geo %>% 
  filter(is.na(city_name)==T) %>% 
  st_centroid() %>% 
  dplyr::select(contains("name"), contains("breaks")) %>% 
  mapview(zcol="pop_cat_breaks_gub_aue_ls")



## Cities: 1 million plus-----
#Among cities above 1,000,000 people, what are the top 10?
hia_summary_gub_1mil_plus = pop_ndvi_gub_biome_tib %>% 
  group_by(ORIG_FID) %>% 
  hia_summarise() %>% 
  filter(pop_cat_mean_val_scaled>=1000000) 

hia_summary_gub_1mil_plus
setwd(here("data-processed"))
save(hia_summary_gub_1mil_plus, file="hia_summary_gub_1mil_plus.RData")

## Figure of top n cities by deaths prevented----
#Some strange things with the sorting...why is it not always correct?
#also note that there are some cities with missing names
#that are excluded from this. Otherwise, it looks okay.
names(hia_summary_gub)
hia_summary_gub %>% 
  filter(is.na(city_name)==F) %>% 
  filter(is.na(n_d_prev_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot(aes(x=n_d_prev_per_100k_pop_pt,
             y=city_name_ranked_by_n_d_prev_per_pop))+
  geom_point(stroke=0,size=.5,color="transparent")+
  geom_pointrange(aes(xmin=n_d_prev_per_100k_pop_min_over_9,
                      xmax=n_d_prev_per_100k_pop_max_over_9))+
  #  scale_x_continuous(limits=c(0,110))+
  labs(
    x="Estimated number of\npremature deaths prevented\nper 100,000 population",
    y="City name")+
  theme_bw(base_size = 8)

#Can I facet this by city size?
table(hia_summary_gub$pop_cat_breaks_gub_aue_ls)
hia_summary_gub %>% 
  filter(is.na(city_name)==F) %>% 
  filter(is.na(n_d_prev_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  slice(1:100) %>% #take top n
  ggplot(aes(x=n_d_prev_per_100k_pop_pt,
             y=city_name_ranked_by_n_d_prev_per_pop))+
  geom_point(stroke=0,size=.5,color="transparent")+
  geom_pointrange(aes(xmin=n_d_prev_per_100k_pop_min_over_9,
                      xmax=n_d_prev_per_100k_pop_max_over_9))+
  #  scale_x_continuous(limits=c(0,110))+
  labs(
    x="Estimated number of\npremature deaths prevented\nper 100,000 population",
    y="City name")+
  theme_bw(base_size = 8)+
  facet_grid(
    cols = vars(pop_cat_breaks_gub_aue_ls))

#This works okay, but is a bit unclear.
#Can try instead just a filter
table(hia_summary_gub$pop_cat_breaks_gub_aue_ls)
hia_summary_gub %>% 
  filter(is.na(city_name)==F) %>% 
  filter(is.na(n_d_prev_per_100k_pop_pt)==F) %>% 
  filter(pop_cat_breaks_gub_aue_ls=="(5.72e+06,3e+08]") %>% 
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  slice(1:50) %>% #take top n
  ggplot(aes(x=n_d_prev_per_100k_pop_pt,
             y=city_name_ranked_by_n_d_prev_per_pop))+
  geom_point(stroke=0,size=.5,color="transparent")+
  geom_pointrange(aes(xmin=n_d_prev_per_100k_pop_min_over_9,
                      xmax=n_d_prev_per_100k_pop_max_over_9))+
  scale_x_continuous(breaks = )
#  scale_x_continuous(limits=c(0,110))+
labs(
  x="Estimated number of\npremature deaths prevented\nper 100,000 population",
  y="City name")+
  theme_bw(base_size = 8)

## maps-------
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

### Map of top 100 by deaths prevented per capita, regardless of size----
hia_summary_gub %>% 
  filter(pop_cat_mean_val_scaled >0)

#which ones have missing city names and have pretty large pop?

hia_summary_gub_geo %>% 
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  slice(1:100) %>% 
  filter(is.na(city_name)==T) %>% 
  st_centroid() %>% #so easier to see
  mapview(zcol="n_d_prev_per_100k_pop_pt")

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
## By biome and city-----
hia_summary_biome_gub= pop_ndvi_gub_biome_tib %>% 
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

names(pop_ndvi_gub_biome_tib)
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)
hia_summary_pop_cat_max_fac = pop_ndvi_gub_biome_tib %>% 
  group_by(pop_cat_max_fac) %>% 
  hia_summarise() %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(pop_cat_max_fac!="0")#remove the lowest category - max 0
names(hia_summary_pop_cat_max_fac)
hia_summary_pop_cat_max_fac %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  print(n=100)
#hia_summary_pop_cat_max_fac %>%    View()

### Figure: deaths per 100k x pop dens---------
names(hia_summary_pop_cat_max_fac)

#define the upper limit of the y-axis
hia_summary_pop_cat_max_fac_y_upper_lim=max(
  hia_summary_pop_cat_max_fac$n_d_prev_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_pop_cat_max_fac_y_upper_lim

plot_n_d_prev_per_100k_x_pop_cat=hia_summary_pop_cat_max_fac %>% 
  ggplot(aes(x=pop_cat_max_fac_w_comma,# Note update Sep 28, 2023-commas
             y=n_d_prev_per_100k_pop_pt))+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_prev_per_100k_pop_min_over_9,
                      ymax=n_d_prev_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_pop_cat_max_fac_y_upper_lim))+
  labs(
    y="Estimated number of\npremature deaths prevented\nper 100,000 population",
    x=
      "Population density\n (unscaled upper bound of LandScan category)\n of pixel per sq. km"
    )+ 
  theme_bw(base_size=12)

plot_n_d_prev_per_100k_x_pop_cat

setwd(here("plots"))
ggsave("plot_n_d_prev_per_100k_x_pop_cat.png", height=5, width=10)

## By pop. density & biome----
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

## By biome-----
hia_summary_biome = pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp) %>% 
  hia_summarise() 

names(hia_summary_biome)

### Figure: deaths per 100k x biome---------

#define the upper limit of the y-axis
hia_summary_biome_y_upper_lim=max(
  hia_summary_biome$n_d_prev_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_biome_y_upper_lim
plot_n_d_prev_per_100k_x_biome=hia_summary_biome %>% 
  filter(is.na(biome_name_imp)==F) %>% 
  ggplot(aes(x=biome_name_imp,# Note update Sep 28, 2023-commas
             y=n_d_prev_per_100k_pop_pt))+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_prev_per_100k_pop_min_over_9,
                      ymax=n_d_prev_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_biome_y_upper_lim))+
  labs(
    y="Estimated number of\npremature deaths prevented\nper 100,000 population",
    x="Biome")+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=50, hjust=1))


plot_n_d_prev_per_100k_x_biome

setwd(here("plots"))
ggsave("plot_n_d_prev_per_100k_x_biome.png", height=5, width=10)

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


## By income category------
#use this look-up table created here:
#scripts/read-boundaries-states-countries.R
setwd(here("data-processed"))
load("lookup_income_grp_5_rename.RData")#loads the new name for income group
hia_summary_income_grp = pop_ndvi_gub_biome_tib %>% 
  #in the original country dataset, name is "name_en", but in the merged
  #file with UN data, I changed it to "country_name_en".
  #This works
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  group_by(income_grp) %>% 
  hia_summarise() %>% 
  left_join(lookup_income_grp_5_rename, by = "income_grp")

hia_summary_income_grp %>% View()

hia_summary_income_grp %>%  View()

### Figure: deaths per 100k x income group------------------
load("lookup_income_grp_5_rename.RData")
names(hia_summary_income_grp)
#set the upper limit of the y-axis
hia_summary_income_grp_y_upper_lim=max(
  hia_summary_income_grp$n_d_prev_per_100k_pop_max_over_9,na.rm=TRUE)*1.1
hia_summary_income_grp_y_upper_lim
plot_n_d_prev_per_100k_x_income_grp
plot_n_d_prev_per_100k_x_income_grp=hia_summary_income_grp %>% 
  filter(is.na(income_grp)==F) %>% 
  ggplot(aes(x=income_grp_5_rename, #add dash in the name
             y=n_d_prev_per_100k_pop_pt #Sep 28, 2023 to include commas 
          )
  )+
  geom_point()+
  geom_pointrange(aes(ymin=n_d_prev_per_100k_pop_min_over_9,
                      ymax=n_d_prev_per_100k_pop_max_over_9))+
  scale_y_continuous(limits=c(0,hia_summary_income_grp_y_upper_lim))+
  labs(
    y="Estimated number of\npremature deaths prevented\nper 100,000 population",
    x="World Bank Income Group")+
  theme_bw(base_size=12)+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=10))


plot_n_d_prev_per_100k_x_income_grp
setwd(here("plots"))
ggsave("plot_n_d_prev_per_100k_x_income_grp.png", height=5, width=10)


## By country----
hia_summary_country = pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en) %>% 
  hia_summarise() %>% 
  ungroup() %>% 
  #make country a factor ranked by n_d_prev_per_100k_pop_pt
  mutate(
    country_name_ranked_by_n_d_prev_per_pop_int=as.factor(country_name_en),
    country_name_ranked_by_n_d_prev_per_pop=fct_reorder(
      country_name_ranked_by_n_d_prev_per_pop_int,
      n_d_prev_per_100k_pop_pt,
      na.rm=TRUE)
  ) %>% 
  #I might want these to come along
  left_join(lookup_income_grp, by = c("country_name_en"="name_en"))  
  

# deaths prevented per population
hia_summary_country %>% 
  arrange(desc(n_d_prev_per_1k_pop_pt)) %>% 
  print(n=100)

#deaths prevented total
hia_summary_country %>% 
  arrange(desc(n_d_prev_mean_pt)) %>% 
  print(n=100)
#yea, not so informative, as it's simply the most populous countries

###Figure of top n countries by deaths prevented----
#this might work if I put countries on the y-axis?
nrow(hia_summary_country)#176 countries
nrow(hia_summary_country)/3

names(hia_summary_country)
plot_n_d_prev_per_100k_x_country_top_n=hia_summary_country %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_prev_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_prev_per_100k_pop_pt)) %>% 
  slice(1:60) %>% #take top n
  ggplot(aes(color=income_grp,
             x=n_d_prev_per_100k_pop_pt,
             y=country_name_ranked_by_n_d_prev_per_pop))+
  geom_point(size=1)+
  geom_pointrange(aes(xmin=n_d_prev_per_100k_pop_min_over_9,
                      xmax=n_d_prev_per_100k_pop_max_over_9))+
  scale_x_continuous(limits=c(0,110))+
  scale_color_brewer(type="qual",
                     #Set2 also works but doesn't portray the sequential nature
                     #The default sequential palettes have too much white
                     #towards the end
                     palette = "Set2",
                     name="Income Group")+
  labs(
    x="Estimated number of\npremature deaths prevented\nper 100,000 population",
    y="Country")+
  theme_bw(base_size = 12)

plot_n_d_prev_per_100k_x_country_top_n
setwd(here("plots"))
ggsave("plot_n_d_prev_per_100k_x_country_top_n.png", height=15, width=10)


