#Filename: summary-global-maps
#March 14, 2023

#Broken off from here
#so that I can source this without it taking forever
#Pop. density, income category, 
#reproject for better global map.
#see world_wintri in https://r.geocompx.org/reproj-geo-data.html
library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
library(sf)
library(tmap)
load("biomes_14_simplified.RData")#don't use. it creates weird streaks.
load("biomes_14.RData")
setwd(here("data-processed"))
biomes_14_winkel = biomes_14 %>% 
  st_transform(crs = "+proj=wintri")

names(biomes_14)
#Following Robin's source code here:
#https://github.com/geocompx/geocompr/blob/main/07-reproj.Rmd
world_wintri_gr = st_graticule(
  lat = c(-89.9, seq(-80, 80, 20), 89.9)) |>
  st_transform(crs = "+proj=wintri")
tm_shape(world_wintri_gr) + tm_lines(col = "gray") +
  tm_shape(biomes_14_winkel)+
  tm_fill(
    col = "BIOME_NAME", 
    palette = viridis::turbo(n=n_distinct(biomes_14$BIOME_NAME))
  )
#streaks are fixed.
