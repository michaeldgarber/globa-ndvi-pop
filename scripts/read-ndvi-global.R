# Read global NDVI data
#The purpose of this script is to read in the global NDVI.
# January 9th, 2022

library(tidyverse)
library(here)
library(terra)
#The data is located here:
#green-space-global-project/global-data-ndvi-pop/data-input/ndvi_2019_Global.tif

#I can follow my processes here:
#~green-space-denver-project/green-space-denver/scripts/1b_wrangle_check_landsat_ndvi_denver.R

# Read data-----
#Comment: based on their file names, it looks like everything is from 2019
setwd(here("data-input"))
ndvi_2019_global = terra::rast("ndvi_2019_Global.tif") %>% 
  #Feb 8 2023: rename here and then all others won't need this step
  tidyterra::rename(ndvi_2019 = ndvi_2019_Global) 

ndvi_2019_global
#ndvi_2019_global %>% plot() #cool. works.

# Restrict to Colorado --------
source(here("scripts", "generate-boundaries-states-countries.R")) 
names(ndvi_2019_global)
ndvi_2019_colorado = ndvi_2019_global %>% 
  terra::crop(colorado_boundary)

ndvi_2019_colorado %>%
  raster::raster() %>%
  mapview(layer.name = "ndvi_2019")

## Save ------
#note unusual syntax because it's a raster.
setwd(here("data-processed"))
terra::writeRaster(
  ndvi_2019_colorado,
  overwrite=TRUE,
  #  datatype = "INT1U", #see Robin's discussion of what this means. default OK
  filename = "ndvi_2019_colorado.tif" 
)

# Restrict to USA-------
ndvi_2019_usa_48 = ndvi_2019_global %>% 
  terra::crop(usa_boundaries_cont_48) 

## Save ------
#note unusual syntax because it's a raster.
setwd(here("data-processed"))
terra::writeRaster(
  ndvi_2019_usa_48,
  overwrite=TRUE,
  filename = "ndvi_2019_usa_48.tif" 
)


