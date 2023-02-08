#This loads the city boundaries aka global urban boundaries (GUB)
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
  st_transform(4326) 
nrow(gub)
names(gub)

gub
# could save it to the data processed folder as an R object.
#actually wait til you make any changes to it.

#gub %>% mapview() #great - works!
#gub %>% plot()

# Restrict to Colorado-----
source(here("scripts", "generate-boundaries-states-countries.R")) 

#And I do this mutate 3 times so make a function
mutate_gub = function(df){
  df %>% 
    mutate(
      area_m2 = st_area(geometry),
      area_km2 = area_m2*1e-6 ,
      
      #Find the 5th and 10th percentile by area to see about excluding,
      #although ultimately it should be an absolute, not relative, cutoff
      area_km2_quantile_5 = quantile(area_km2, .05)
    )
}
gub_colorado = gub %>% 
  st_intersection(colorado_boundary) %>% 
  mutate_gub()
    
#okay, it looks like the measure they give us is in square km
setwd(here("data-processed"))
save(gub_colorado, file = "gub_colorado.RData")

#okay, so to avoid these small fringe-like islands, we could just filter
#to areas above a certain value. How about the 5th percentile and above?
summary(gub_colorado$area_km2_quantile_5)
gub_colorado %>%
  filter(area_km2>=area_km2_quantile_5) %>% 
  mapview(zcol = "area_km2")

# Restrict to Georgia----
#I'm curious how Georgia looks
#loaded above

georgia_boundary %>% mapview()
st_crs(gub)
gub_georgia = gub %>% 
  st_intersection(georgia_boundary) %>% 
  mutate_gub()

gub_georgia %>% mapview()
nrow(gub)
#Restrict to USA-------
sf::sf_use_s2(FALSE)
usa_boundaries_cont_48_union %>% mapview()
usa_boundaries_cont_48 %>% mapview()
gub_usa_48 = gub %>% #whatever I did sped this up considerably.
  st_make_valid() %>% 
  st_intersection(usa_boundaries_cont_48_union)%>% 
  st_make_valid() %>% 
  mutate_gub()
gub_usa_48
nrow(gub_usa_48)
setwd(here("data-processed"))
save(gub_usa_48, file = "gub_usa_48.RData")


