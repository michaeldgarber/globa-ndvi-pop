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

#These are created here
#~global-data-ndvi-pop/scripts/merge-ndvi-ls.R
merge_pop_ndvi_co = terra::rast("merge_pop_ndvi_co.tif") #Landscan population data and NDVI - CO
merge_pop_ndvi_usa_48 = terra::rast("merge_pop_ndvi_usa_48.tif") #for USA

#Rasterize with respect to the landscan pop. raster.

# Rasterize GUB vector files---------
## Colorado-------
#To merge the values from gub_colorado, I think I need to rasterize it first.
#See rasterize description in Lovelace 
#https://geocompr.robinlovelace.net/raster-vector.html#rasterization
gub_colorado_raster_orig_fid = rasterize(
  gub_colorado,
  merge_pop_ndvi_co,
  #It'd be cool to add multiple values in one go, but I guess not.
  #https://stackoverflow.com/questions/68227745/how-to-rasterise-with-multiple-field-values
  field = "ORIG_FID")

#some checks on resolution, origin, etc.
res(gub_colorado_raster_orig_fid)
res(merge_pop_ndvi_co)
ext(gub_colorado_raster_orig_fid)
origin(gub_colorado_raster_orig_fid)
origin(merge_pop_ndvi_co)
gub_colorado_raster_orig_fid %>% as_tibble() %>% summary()
gub_colorado_raster_orig_fid %>% raster::raster() %>% mapview()

## USA-------
setwd(here("data-processed"))
load("gub_usa_48.RData")
gub_usa_48_raster_orig_fid = rasterize(
  gub_usa_48,
  merge_pop_ndvi_usa_48,
  #It'd be cool to add multiple values in one go, but I guess not.
  #https://stackoverflow.com/questions/68227745/how-to-rasterise-with-multiple-field-values
  field = "ORIG_FID") 

terra::writeRaster(
  gub_usa_48_raster_orig_fid,
  overwrite=TRUE,
  filename = "gub_usa_48_raster_orig_fid.tif" 
)


# Rasterize ecoregions & biomes-------
setwd(here("data-processed"))
load("biomes_14_usa_48.RData")

## USA------
names(biomes_14_usa_48)
biomes_14_usa_48_raster_biome_name = rasterize(
  biomes_14_usa_48,
  merge_pop_ndvi_usa_48,
  #It'd be cool to add multiple values in one go, but I guess not.
  #https://stackoverflow.com/questions/68227745/how-to-rasterise-with-multiple-field-values
  field = "BIOME_NAME")
terra::writeRaster(
  biomes_14_usa_48_raster_biome_name,
  overwrite=TRUE,
  filename = "biomes_14_usa_48_raster_biome_name.tif" 
)

biomes_14_usa_48_raster_biome_name %>% 
  as_tibble() %>% 
  group_by(BIOME_NAME) %>% 
  summarise(n=n())

biomes_14_usa_48_raster_biome_name %>% 
  raster::raster() %>% 
  mapview(zcol = "BIOME_NAME")



## World-------
setwd(here("data-processed"))
load("ecoregions.RData")