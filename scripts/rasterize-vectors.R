#Rasterize the necessary vector data
#so that it can be linked with the raster data
#The three sets of vector data we have are
#1. Global urban boundaries
#2. Ecoregions & biomes
#3. Country boundaries

library(terra)
library(raster)
library(mapview)
library(here)
library(tidyverse)

setwd(here("data-processed"))
load("gub_colorado.RData") #Global urban boundaries


#Rasterize with respect to the landscan pop. raster.

# Rasterize GUB vector files---------
## USA-------
#To merge the values from gub_colorado, I think I need to rasterize it first.
#See rasterize description in Lovelace 
#https://geocompr.robinlovelace.net/raster-vector.html#rasterization

#use landscan as target raster throughout, as it's somewhat higher resolution than NDVI
setwd(here("data-processed"))
ls_2019_usa_48_wrangle = terra::rast("ls_2019_usa_48_wrangle.tif") 


setwd(here("data-processed"))
load("gub_usa_48.RData")
gub_usa_48_raster_orig_fid = rasterize(
  gub_usa_48, #vector to be rasterized
  ls_2019_usa_48_wrangle, #target raster
  #It'd be cool to add multiple values in one go, but I guess not.
  #https://stackoverflow.com/questions/68227745/how-to-rasterise-with-multiple-field-values
  field = "ORIG_FID") 

terra::writeRaster(
  gub_usa_48_raster_orig_fid,
  overwrite=TRUE,
  filename = "gub_usa_48_raster_orig_fid.tif" 
)



## Global------
setwd(here("data-input", "ls-global-2019-alt-dl"))
ls_2019_global = terra::rast("landscan-global-2019-colorized.tif")
setwd(here("data-processed"))
load("gub.RData")
gub_raster_orig_fid = rasterize(
  gub, #vector to be rasterized
  ls_2019_global, #target raster
  field = "ORIG_FID"
  )

setwd(here("data-processed"))
terra::writeRaster(
  gub_raster_orig_fid,
  overwrite=TRUE,
  filename = "gub_raster_orig_fid.tif" 
)


# Rasterize ecoregions & biomes-------
setwd(here("data-processed"))
load("biomes_14_usa_48.RData")

## USA------
names(biomes_14_usa_48)
biomes_14_usa_48_raster_biome_name = rasterize(
  biomes_14_usa_48, #vector to be rasterized
  ls_2019_usa_48_wrangle, #target raster
  field = "BIOME_NAME")
terra::writeRaster(
  biomes_14_usa_48_raster_biome_name,
  overwrite=TRUE,
  filename = "biomes_14_usa_48_raster_biome_name.tif" 
)

#to examine the issue of some pixels having missing pixels, look at vector
#vs raster of biomes
plot(biomes_14_usa_48_raster_biome_name)

biomes_14_usa_48_raster_biome_name %>% 
  as_tibble() %>% 
  group_by(BIOME_NAME) %>% 
  summarise(n=n())

biomes_14_usa_48_raster_biome_name %>% 
  raster::raster() %>% 
  mapview(zcol = "BIOME_NAME")


## Global-------
setwd(here("data-processed"))
load("ecoregions.RData")
load("biomes_14.RData")
setwd(here("data-input", "ls-global-2019-alt-dl"))
ls_2019_global = terra::rast("landscan-global-2019-colorized.tif")

biomes_14_raster_biome_name = rasterize(
  biomes_14, #vector to be rasterized
  ls_2019_global, #target raster
  field = "BIOME_NAME")

setwd(here("data-processed"))
terra::writeRaster(
  biomes_14_raster_biome_name,
  overwrite=TRUE,
  filename = "biomes_14_raster_biome_name.tif" 
)


# Rasterize countries-------
## USA-----
setwd(here("data-processed"))
ls_2019_usa_48_wrangle = terra::rast("ls_2019_usa_48_wrangle.tif") 
names(countries)
table(countries$name_en)
#Use the version that has been merged with the UN data so that we use the
#column name "country_name_en"
source(here("scripts", "merge-un-countries-geo.R"))
countries_joined_with_un_pop_deaths_2019_pared_usa
names(countries_joined_with_un_pop_deaths_2019_pared_usa)
countries_usa_48 = countries_joined_with_un_pop_deaths_2019_pared_usa %>% 
  filter(country_name_en == "United States of America") %>% 
  st_intersection(usa_boundaries_cont_48_union)

countries_usa_48 %>% mapview()
countries_usa_48_raster_country_name_en = rasterize(
  countries_usa_48, #vector to be rasterized
  ls_2019_usa_48_wrangle, #target raster
  field = "country_name_en")
setwd(here("data-processed"))
terra::writeRaster(
  countries_usa_48_raster_country_name_en,
  overwrite=TRUE,
  filename = "countries_usa_48_raster_country_name_en.tif" 
)

## Global----
countries_joined_with_un_pop_deaths_2019_pared #created above when script is sourced
setwd(here("data-input", "ls-global-2019-alt-dl"))
ls_2019_global = terra::rast("landscan-global-2019-colorized.tif")

table(countries_joined_with_un_pop_deaths_2019_pared$country_name_en)
countries_raster_country_name_en = rasterize(
  countries_joined_with_un_pop_deaths_2019_pared, #vector to be rasterized
  ls_2019_global, #target raster
  field = "country_name_en")
setwd(here("data-processed"))
terra::writeRaster(
  countries_raster_country_name_en,
  overwrite=TRUE,
  filename = "countries_raster_country_name_en.tif" 
)


