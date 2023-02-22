#This loads the city boundaries aka global urban boundaries (GUB)
#and calculates their area
#Began January 10, 2022

library(raster)
library(terra)
library(here)
library(tidyverse)
library(mapview)
library(tidyterra)
library(sf)

# Load global data-------
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
  st_set_geometry(NULL)
setwd(here("data-processed"))
save(gub_nogeo, file = "gub_nogeo.RData")

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

### Link with city-name data--------  
#Use the geonames data instead
source(here("scripts","generate-boundaries-states-countries.R"))
names(cities_pt)
names(cities_geonames)#using this now Feb 21 2023
cities_pt
gub_names = gub %>% 
  st_join(cities_geonames) %>% 
  #if there are many points within a given polygon,
  #take the one with the largest population. that will be the corresponding name
  group_by(ORIG_FID) %>% 
  arrange(desc(city_population)) %>% 
  slice(1) %>% 
  ungroup()

gub_names
names(gub_names)
lookup_gub_city_id = gub_names %>% 
  arrange(desc(city_population)) %>% 
  st_set_geometry(NULL) %>% 
  distinct(ORIG_FID, geoname_id)
setwd(here("data-processed"))
save(lookup_gub_city_id, file = "lookup_gub_city_id.RData")

lookup_gub_city_name = gub_names %>% 
  arrange(desc(city_population)) %>% 
  st_set_geometry(NULL) %>% 
  distinct(ORIG_FID, city_name)
setwd(here("data-processed"))
save(lookup_gub_city_name, file = "lookup_gub_city_name.RData")

lookup_gub_city_name %>% 
  print(n=100)
gub_names %>% 
  filter(city_population>8000000) %>% 
  mapview(zcol = "city_name")

gub_orig_fid_many_names=gub_names %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(ORIG_FID) %>% 
  summarise(n=n()) %>% 
  mutate(orig_fid_many_names = case_when(n>1~1,TRUE~0))

gub_orig_fid_many_names %>% 
  filter(orig_fid_many_names==1)


# What ORIG_FID is Tokyo?


# Restrict to Colorado-----
source(here("scripts", "generate-boundaries-states-countries.R")) 

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


