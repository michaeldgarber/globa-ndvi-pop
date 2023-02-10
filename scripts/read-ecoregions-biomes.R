#Filename: read-ecoregions-biomes

#Read Ecoregion data
#Found via GEE: 
#https://developers.google.com/earth-engine/datasets/catalog/RESOLVE_ECOREGIONS_2017

#https://ecoregions.appspot.com
# based on this article:
# https://academic.oup.com/bioscience/article/67/6/534/3102935
library(raster)
library(terra)
library(here)
library(tidyverse)
library(mapview)
library(tidyterra)
library(sf)
setwd(here("data-input", "ecoregions-biomes"))
getwd()
# Read ecoregions / biome data------
ecoregions = st_read(dsn ="Ecoregions2017") %>%
  #don't use st_simplify yet because it has to be unioned,
  #and if it has been st_simplified, it creates little slivers.
  st_transform(4326) %>% 
  st_make_valid() 


object.size(ecoregions)
#file size without simplification   267601304 bytes
#dTolerance 100                     90119752 bytes                 
ecoregions %>% 
  mapview(zcol = "BIOME_NAME")


setwd(here("data-processed"))
save(ecoregions, file = "ecoregions.RData")

ecoregions
names(ecoregions)
nrow(ecoregions)
table(ecoregions$BIOME_NAME)
table(ecoregions$REALM)
table(ecoregions$ECO_NAME)
table(ecoregions$ECO_BIOME_)
#how many biomes?
ecoregions %>% 
  st_set_geometry(NULL) %>% 
  group_by(BIOME_NAME) %>% 
  summarise(n_ecoregions=n())

#how many ecoregions?
names(ecoregions)
ecoregions %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(ECO_NAME) %>% 
  summarise(n=n()) 


# Dissolve into just the fourteen biomes - smaller data------
#Note this takes some time, so probably good to save.
sf::sf_use_s2(FALSE) 
biomes_14 = ecoregions %>% 
  group_by(BIOME_NAME) %>% 
  summarise(n_ecoregions=n()) %>% 
  ungroup() 

object.size(biomes_14)
setwd(here("data-processed"))
save(biomes_14, file = "biomes_14.RData")

biomes_14 %>% 
  mapview(zcol = "BIOME_NAME")

# Restrict to Colorado----
sf::sf_use_s2(FALSE)
source(here("scripts", "generate-boundaries-states-countries.R")) 
state_boundaries %>% mapview()
colorado_boundary = state_boundaries %>% 
  filter(name == "Colorado")
biomes_14_colorado = biomes_14 %>% 
  st_intersection(colorado_boundary)

names(biomes_14)
biomes_14_colorado %>% mapview(zcol = "BIOME_NAME")

#and what about ecoregions in Colorado?
sf::sf_use_s2(FALSE) #needed for it to work
ecoregions_colorado = ecoregions %>% 
  st_intersection(colorado_boundary)

names(ecoregions_colorado)
ecoregions_colorado %>% mapview(zcol = "ECO_NAME")

# Restrict to continental USA-------
source(here("scripts", "generate-boundaries-states-countries.R")) #if needed to run again
setwd(here("data-processed"))
load("biomes_14.RData") #if needed
sf::sf_use_s2(FALSE) #needed for it to work
biomes_14_usa_48 = biomes_14 %>% 
  st_intersection(st_buffer(usa_boundaries_cont_48_union,0)) 

#little lines are gone b/c didn't st_simplify. great.
biomes_14_usa_48 %>% mapview(zcol = "BIOME_NAME")

setwd(here("data-processed"))
save(biomes_14_usa_48, file = "biomes_14_usa_48.RData")

#Make a smaller version as well for vis
biomes_14_usa_48_simplified = biomes_14_usa_48 %>%   
  st_transform(2232) %>% 
  st_simplify(dTolerance = 10000) %>% #simplify before mapview
  st_transform(4326)
object.size(biomes_14_usa_48_simplified)
object.size(biomes_14_usa_48)
biomes_14_usa_48_simplified %>% 
  mapview(
    col.regions = viridis::turbo(n=n_distinct(biomes_14_usa_48_simplified$BIOME_NAME)),
    zcol = "BIOME_NAME"
    )
setwd(here("data-processed"))
save(biomes_14_usa_48_simplified, file = "biomes_14_usa_48_simplified.RData")

ecoregions_usa_48 = ecoregions %>% 
  st_intersection(usa_boundaries_cont_48_union)
save(ecoregions_usa_48, file = "ecoregions_usa_48.RData")
ecoregions_usa_48 %>% 
  mapview(
    zcol = "ECO_NAME", 
    col.regions = viridis::turbo(n=n_distinct(ecoregions_usa_48$ECO_NAME))
    )

