#Script for limiting to state boundaries and other territories
#Rather than doing this in every script, easier to just run once
library(tidyverse)
library(USAboundaries)
library(mapview)
library(sf)

# US states----------
state_boundaries = USAboundariesData::states_contemporary_hires %>% 
  st_simplify(dTolerance = 100)
object.size(state_boundaries) #making file size smaller.
#no simplification: 5302128 bytes
#dTolerance 10:     4303520 bytes
#dTolerance 100:    2445472 bytes (so half the size, great)
state_boundaries %>% mapview()
usa_boundaries_cont_48 = state_boundaries %>% 
  filter(name !="Alaska") %>% 
  filter(name !="Hawaii") %>% 
  filter(name !="Guam") %>% 
  filter(name != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(name != "Puerto Rico") %>% 
  filter(name != "American Samoa") %>% 
  filter(name !="United States Virgin Islands")

#unary union this
usa_boundaries_cont_48_union = usa_boundaries_cont_48 %>% 
  st_union() %>% 
  st_make_valid()

nrow(usa_boundaries_cont_48)
#usa_boundaries_cont_48 %>% mapview()
colorado_boundary = state_boundaries %>% 
  filter(name == "Colorado")
michigan_boundary = state_boundaries %>% 
  filter(name == "Michigan")
georgia_boundary = state_boundaries %>% 
  filter(name == "Georgia")

# Country boundaries-----
#There are lots of packages that have data on country boundaries.
#This one seems good; it was used here:
#https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html
#https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
#install.packages("rnaturalearth")
library(rnaturalearth)
library(sf)
library(mapview)
library(tidyverse)
countries = rnaturalearth::ne_countries(returnclass = "sf") 

class(countries)
mapview(countries, zcol = "wb_a2")

# countries %>% 
#   filter(name_en == "United States of America") %>% 
#   mapview(zcol = "name_en")