#Script for limiting to state boundaries and other territories
#Rather than doing this in every script, easier to just run once
library(tidyverse)
library(USAboundaries)
library(mapview)
library(sf)
library(here)

# US states----------
state_boundaries = USAboundariesData::states_contemporary_hires 
  #actually don't use st_simplify here because it creates little slivers between the boundaries
  #when you then go to union it below


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
  filter(name !="United States Virgin Islands") %>% 
  mutate(cont_48=1)
usa_boundaries_cont_48
class(usa_boundaries_cont_48)

#unary union this
usa_boundaries_cont_48_union = usa_boundaries_cont_48 %>% 
  group_by(cont_48) %>% 
  summarise(n_states_us=n()) %>% 
  ungroup() %>% 
  st_as_sf()  %>% #getting a notice that it's not valid, so try
  st_make_valid() 

class(usa_boundaries_cont_48)
nrow(usa_boundaries_cont_48)
usa_boundaries_cont_48_union %>% mapview()
colorado_boundary = usa_boundaries_cont_48 %>% 
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

# Global cities----
#maybe just download these?
#https://simplemaps.com/data/world-cities
#from https://simplemaps.com/data/world-cities


## Source 1----
setwd(here("data-input","simplemaps_worldcities_basicv1"))
cities_pt = readr::read_delim("worldcities.csv") %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  rename(
    city_id=id,
    city_name = city,
    city_population = population)

setwd(here("data-processed"))
save(cities_pt, file = "cities_pt.RData")
nrow(cities_pt)
names(cities_pt)
cities_pt %>% 
  st_set_geometry(NULL)

#for a given city id (use id, not name), what is country?
lookup_city_id_country = cities_pt %>% 
  st_set_geometry(NULL) %>% 
  distinct(country,city_id)

setwd(here("data-processed"))
save(lookup_city_id_country, file ="lookup_city_id_country.RData")

## Source 2------
#alternate might be
#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/
#I'm going to use the above for a few reason: looks to have been modified recently, 
#it follows more of an open-source ethos
library(here)
library(readr)
setwd(here("data-input","cities-public-opendatasoft"))
#Note the use of read_delim() instead of read_csv()
stringr::str_split
cities_geonames = readr::read_delim(
  "geonames-all-cities-with-a-population-1000.csv", 
  delim=";") %>% 
  #split the coordinates column into lat/lon
  #str_split_i extracts only a single piece from a string
  #https://stringr.tidyverse.org/reference/str_split.html
  mutate(
    lng = str_split_i(Coordinates, pattern=",", 2),
    lat = str_split_i(Coordinates, pattern=",", 1),
    #use ASCII name as the main name, as it avoids accents
    city_name =`ASCII Name`#have to use back ticks because there are spaces
  ) %>% 
  rename(
    geoname_id = `Geoname ID`,
    country_name = `Country name EN`,
    city_population = `Population`,
    city_elevation = `Elevation`,
    city_date_modified= `Modification date`,
    city_name_accents = `Name`#with possible accents per local name
  ) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  #Now only pick some of those to use
  dplyr::select(geoname_id, starts_with("city"), contains("_name")) %>% 
  arrange(desc(city_population))

setwd(here("data-processed"))
save(cities_geonames, file = "cities_geonames.RData")

nrow(cities_geonames)
names(cities_geonames)
# cities_geonames %>% View()
cities_geonames %>%
  filter(city_population>1000000) %>%
  mapview()
names(cities_geonames)


# lookup for city population
lookup_city_id_city_population=cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(geoname_id, city_population)

nrow(cities_geonames)
nrow(lookup_city_id_city_population)

#lookup for city name
lookup_geoname_id_city_name = cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  distinct(geoname_id,city_name)

#for a given city id (use id, not name), what is country?
lookup_geoname_id_country_name = cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  distinct(geoname_id,country_name)

