#This loads the city boundaries aka global urban boundaries (GUB)
#and calculates their area
#Began January 10, 2022
#Edits Oct 6, 2023 re. city names

library(raster)
library(terra)
library(here)
library(tidyverse)
library(mapview)
library(tidyterra)
library(sf)

# Load global urban boundary data-------
setwd(here("data-input", "global-urban-boundaries"))
getwd()
gub = st_read(dsn ="GUB_Global_2018") %>%
  st_transform(4326) %>% 
  mutate(  #measure area this way
    area_m2 = as.numeric(st_area(geometry)),
    area_km2 = area_m2*1e-6 
  )
setwd(here("data-processed"))
save(gub, file = "gub.RData")
#load("gub.RData")
nrow(gub)
names(gub)

gub %>% 
  st_set_geometry(NULL) %>% 
  ggplot()+
  geom_histogram(
    aes(area_km2),bins=100)

#gub %>% mapview() #great - works!
#gub %>% plot()

## Lookups and modified versions-------
#a version without geometry for presentations
gub_nogeo = gub %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
setwd(here("data-processed"))
save(gub_nogeo, file = "gub_nogeo.RData")
#load("gub_nogeo.RData")
nrow(gub_nogeo)
summary(gub_nogeo$area_km2)
gub_nogeo
### geometry lookup------
# lookup for the geometry
lookup_gub_orig_fid_geo = gub %>% 
  dplyr::select(ORIG_FID, geometry)

setwd(here("data-processed"))
save(lookup_gub_orig_fid_geo, file = "lookup_gub_orig_fid_geo.RData")

### lookup for city's area----
lookup_gub_area_km2 = gub %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(ORIG_FID, area_km2)

save(lookup_gub_area_km2, file = "lookup_gub_area_km2.RData")

# Link with city-name data--------  
#Changes here October 6, 2023
#8 pm
#I need a better way to keep track of the data source for city names.
#I have to add a suffix.

#845 pm
#I'm making simplemaps pro the main source ($200 USD) to avoid missings in geonames

source(here("scripts","read-boundaries-states-countries.R"))

#This takes longer than it did previously, but hopefully that means it's getting more matches.
gub_city_name_simplemaps = gub %>% 
  st_join(cities_simplemaps_pro) %>% 
  #if there are many points within a given polygon,
  #take the one with the largest population. 
  #that will be the corresponding name
  group_by(ORIG_FID) %>% 
  arrange(desc(city_population_simplemaps)) %>% 
  slice(1) %>% 
  ungroup()

gub_city_name_simplemaps
names(gub_city_name_simplemaps)
lookup_gub_geoname_id = gub_city_name_simplemaps %>% 
  arrange(desc(city_population_geonames)) %>% 
  st_set_geometry(NULL) %>% 
  distinct(ORIG_FID, geoname_id)
setwd(here("data-processed"))
save(lookup_gub_geoname_id, file = "lookup_gub_geoname_id.RData")

lookup_gub_city_name_geonames_geonames = gub_city_name_simplemaps %>% 
  arrange(desc(city_population_geonames)) %>% 
  st_set_geometry(NULL) %>% 
  distinct(ORIG_FID, city_name_geonames)
setwd(here("data-processed"))
save(lookup_gub_city_name_geonames_geonames, file = "lookup_gub_city_name_geonames_geonames.RData")

lookup_gub_city_name_geonames_geonames %>% 
  print(n=100)
gub_city_name_simplemaps %>% 
  filter(city_population_geonames>8000000) %>% 
  mapview(zcol = "city_name_geonames")

gub_orig_fid_many_names=gub_city_name_simplemaps %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(ORIG_FID) %>% 
  summarise(n=n()) %>% 
  mutate(orig_fid_many_names = case_when(n>1~1,TRUE~0))

gub_orig_fid_many_names %>% 
  filter(orig_fid_many_names==1)

### missing city names in geonames data-------
# Oct 6, 2023:
#I have about 3k cities missing their name.
#Let's find them and add the name from source 2 if it's missing a name from source 1
orig_fids_without_geoname_id=lookup_gub_geoname_id %>% 
  filter(is.na(geoname_id)==T) %>% 
  mutate(city_name_geoname_miss=1)

names(gub_city_name_simplemaps)
gub_names_add_missing_names=gub_city_name_simplemaps %>% 
  left_join(orig_fids_without_geoname_id, by = "ORIG_FID") %>% 
  filter(city_name_geoname_miss==1)

#now join these with the simple maps data to see if we can pick up more
names(cities_simplemaps)
nrow(gub_names_add_missing_names)
names(gub_names_add_missing_names)
## lookup simplemaps cities among missing geonames------
gub_names_missing_geo_join_simple_maps=gub_names_add_missing_names %>% 
  dplyr::select(ORIG_FID) %>% #to avoid conflicts. note geo is sticky
  st_join(cities_simplemaps) %>%   
  group_by(ORIG_FID) %>% 
  arrange(desc(city_population_simplemaps)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(city_name_simplemaps_miss=case_when(
    is.na(city_name_simplemaps)==T~1,
    TRUE~0
  )
  )

names(gub_names_missing_geo_join_simple_maps)
#okay, most are still missing
nrow(gub_names_missing_geo_join_simple_maps)
n_distinct(gub_names_missing_geo_join_simple_maps$ORIG_FID)
table(gub_names_missing_geo_join_simple_maps$city_name_simplemaps_miss)
gub_names_missing_geo_join_simple_maps %>% 
  filter(city_name_simplemaps_miss==0) %>% 
  mapview()

lookup_simplemaps_among_missing_geonames=gub_names_missing_geo_join_simple_maps %>% 
  st_set_geometry(NULL) %>% 
  distinct(ORIG_FID,city_name_simplemaps)

nrow(lookup_simplemaps_among_missing_geonames)
n_distinct(lookup_simplemaps_among_missing_geonames$ORIG_FID)
lookup_simplemaps_among_missing_geonames
# Check out specific cities--------

## Tokyo----
# What ORIG_FID is Tokyo?
lookup_gub_city_name_geonames %>% 
  filter(city_name_geonames == "Tokyo")#2238

## Schenzhen-----
#What city does 2238 correspond to? It has a very large area
lookup_gub_city_name_geonames %>% 
  filter(ORIG_FID == "2238") #Shenzhen

## New York City v. Manhattan-------
#For the example, I should do New York.
#Which city is New York?
#begin with pixels of new york, then summarize to new york, etc
#or maybe Chicago since I know it better, and it might be easier to see
lookup_gub_city_name_geonames %>% 
  filter(city_name_geonames == "New York City")
gub %>% 
  filter(ORIG_FID=="60310") %>% 
  mapview()

#Manhattan. Oh - different Manhattan (Kansas)
gub %>% 
  filter(ORIG_FID=="56569") %>% 
  mapview()

# Manhattan, Illinois (phew)
gub %>% 
  filter(ORIG_FID=="55694") %>% 
  mapview()


lookup_gub_city_name_geonames %>% 
  filter(city_name_geonames == "Chicago")




gub %>% 
  filter(ORIG_FID=="55685") %>% 
  mapview()

# Restrict to Colorado-----
source(here("scripts", "read-boundaries-states-countries.R")) 

gub_colorado = gub %>% 
  st_intersection(colorado_boundary) 


#okay, it looks like the measure they give us is in square km
setwd(here("data-processed"))
save(gub_colorado, file = "gub_colorado.RData")

#okay, so to avoid these small fringe-like islands, we could just filter
#to areas above a certain value. How about the 5th percentile and above?
summary(gub_colorado$area_km2_quantile_5)
gub_colorado %>%
  mapview(zcol = "area_km2")

#Make a Colorado one that's simplified (smaller size) for visualization
gub_colorado_simplified = gub_colorado %>% 
  st_transform(2232) %>% 
  st_simplify(dTolerance = 500) %>% #simplify before mapview
  st_transform(4326)

gub_colorado_simplified %>% mapview()
save(gub_colorado_simplified, file = "gub_colorado_simplified.RData")
object.size(gub_colorado_simplified)
object.size(gub_colorado)
# Restrict to Georgia----
#I'm curious how Georgia looks
#loaded above

georgia_boundary %>% mapview()
st_crs(gub)
gub_georgia = gub %>% 
  st_intersection(georgia_boundary)

gub_georgia %>% mapview()
nrow(gub)
#Restrict to USA-------
sf::sf_use_s2(FALSE)
usa_boundaries_cont_48_union %>% mapview()
usa_boundaries_cont_48 %>% mapview()
gub_usa_48 = gub %>% #whatever I did sped this up considerably.
  st_make_valid() %>% 
  st_intersection(usa_boundaries_cont_48_union)%>% 
  st_make_valid() 

gub_usa_48
nrow(gub_usa_48)
setwd(here("data-processed"))
save(gub_usa_48, file = "gub_usa_48.RData")

#where is 54733
gub_usa_48 %>% filter(ORIG_FID=="54733") %>% mapview()




