#merge-ndvi-ls
#How can I merge the two now?

#January 26th, 2023
#I'm updating this script so that it merges the following rasters:
# ls_2019 is always the target raster
#NDVI
#GUB (rasterized from vector)
# ecoregion / biome ( rasterized from vector)

# Feb 8, 2023
# Note it would be less repetitive code to begin with global and then filter everything down,
# but beause of how long the global analysis takes to run, I'm going to get everything set up 
# with USA first and then run the global code later.

library(sf)
library(terra)
library(here)
library(tidyverse)
library(mapview)
library(tidyterra)

# USA----
## Load USA files----
#that which everything will be merged to
setwd(here("data-processed"))
ls_2019_usa_48_wrangle = terra::rast("ls_2019_usa_48_wrangle.tif")
ndvi_2019_usa_48 = terra::rast("ndvi_2019_usa_48.tif")
biomes_14_usa_48_raster_biome_name = terra::rast("biomes_14_usa_48_raster_biome_name.tif")
gub_usa_48_raster_orig_fid = terra::rast("gub_usa_48_raster_orig_fid.tif")
countries_usa_48_raster_country_name_en = terra::rast(
  "countries_usa_48_raster_country_name_en.tif")

#checks
res(biomes_14_usa_48_raster_biome_name)
dim(biomes_14_usa_48_raster_biome_name)
dim(ls_2019_usa_48_wrangle)
dim(ndvi_2019_usa_48)
res(ndvi_2019_usa_48)
res(ls_2019_usa_48_wrangle)
res(gub_usa_48_raster_orig_fid)
dim(gub_usa_48_raster_orig_fid)
res(countries_usa_48_raster_country_name_en)
dim(countries_usa_48_raster_country_name_en)


## Resample NDVI to be like landscan-------
# Use re-sample to make the resolution of the NDVI raster the same as the other.
#Note I don't have to do this for the rasterized vectors because they were rasterized
#with respect to ls_2019_usa_48
ndvi_2019_usa_48_resample = resample(
  ndvi_2019_usa_48, #raster to be resampled 
  ls_2019_usa_48_wrangle #target resolution (for resolution)
  )

## Combine them using c()--------
pop_ndvi_gub_biome_usa_48 = c(
  ls_2019_usa_48_wrangle, 
  ndvi_2019_usa_48_resample,
  gub_usa_48_raster_orig_fid, #city boundaries
  biomes_14_usa_48_raster_biome_name, #biomes
  countries_usa_48_raster_country_name_en #country boundaries
  )

#save
setwd(here("data-processed"))
terra::writeRaster(
  pop_ndvi_gub_biome_usa_48,
  overwrite=TRUE,
  filename = "pop_ndvi_gub_biome_usa_48.tif" 
)


names(pop_ndvi_gub_biome_usa_48)
pop_ndvi_gub_biome_usa_48$ndvi_2019 %>% 
  raster::raster() %>% 
  mapview()
pop_ndvi_gub_biome_usa_48$pop_cat_1_8 %>% 
  raster::raster() %>% 
  mapview()
pop_ndvi_gub_biome_usa_48$ORIG_FID %>% 
  raster::raster() %>% 
  mapview()
pop_ndvi_gub_biome_usa_48$BIOME_NAME %>% 
  raster::raster() %>% 
  mapview()


# Globally-----
## Load global files-----
#Note the procedure here will be somewhat distinct, because we're going to do the
#wrangling of the landscan data in a subsequent step, so, here, just load
#the "unwrangled" Landscan data.
setwd(here("data-input", "ls-global-2019-alt-dl"))
ls_2019_global = terra::rast("landscan-global-2019-colorized.tif")
names(ls_2019_global)
#source this rather than repeat the code
source(here("scripts", "read-ndvi-global.R"))
ndvi_2019_global
setwd(here("data-processed"))
biomes_14_raster_biome_name = terra::rast("biomes_14_raster_biome_name.tif")
gub_raster_orig_fid = terra::rast("gub_raster_orig_fid.tif")
countries_raster_country_name_en = terra::rast("countries_raster_country_name_en.tif")

## Check dimensions / resolution
dim(ls_2019_global)
res(ls_2019_global)
ls_2019_global

dim(biomes_14_raster_biome_name)
res(biomes_14_raster_biome_name)

dim(gub_raster_orig_fid)
res(gub_raster_orig_fid)

dim(countries_raster_country_name_en)
res(countries_raster_country_name_en)


## Resample NDVI to be like landscan-------
# Use re-sample to make the resolution of the NDVI raster the same as the other.
#Note I don't have to do this for the rasterized vectors because they were rasterized
#with respect to ls_2019_usa_48
ndvi_2019_global_resample = resample(
  ndvi_2019_global, #raster to be resampled 
  ls_2019_global #target resolution (for resolution)
)
dim(ndvi_2019_global_resample)
res(ndvi_2019_global_resample)

## Combine them using c()--------
names(pop_ndvi_gub_biome)
pop_ndvi_gub_biome = c(
  ls_2019_global,  #landscan pop. categories (not yet wrangled here - global only)
  ndvi_2019_global_resample, #ndvi
  gub_raster_orig_fid, #city boundaries
  biomes_14_raster_biome_name, #biomes
  countries_raster_country_name_en #country boundaries
)

#save
setwd(here("data-processed"))
terra::writeRaster(
  pop_ndvi_gub_biome,
  overwrite=TRUE,
  filename = "pop_ndvi_gub_biome.tif" 
)



