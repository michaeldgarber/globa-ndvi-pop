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
#Also see https://github.com/nvkelso/natural-earth-vector

#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("rnaturalearthdata")#needed for higher resolution country
                                      #boundaries
library(rnaturalearthdata)
library(rnaturalearthhires)#needed for extra high resolution country boundaries
library(sf)
library(mapview)
library(tidyverse)
#Feb 22, 2023: I didn't download the natural earth data at
#a fine enough scale.
#I need to download at a scale of 10 meters, not 110.
#See
#https://www.naturalearthdata.com
#https://github.com/ropensci/rnaturalearth
#Note the default scale was small, which was excluding
#lots of coastal areas due to low resolution
countries = rnaturalearth::ne_countries(
#  scale = "large",#
  scale = "large",#corresponds to a 10-meter scale - see
  #https://rdrr.io/cran/rnaturalearth/man/ne_countries.html
  returnclass = "sf") %>% 
  #wrangle the "income_group" category a little
  mutate(
    #income_grp is 5 categories: high-income OECD, high-income nonOECD, upper middle
    #lower middle, and low.
    #Make one that's just 4 categories collapsing top 2
    income_grp_4 = case_when(
        income_grp=="1. High income: OECD" ~ "1-High income",
        income_grp=="2. High income: nonOECD" ~ "1-High income",
        income_grp=="3. Upper middle income" ~ "2-Upper middle income",
        income_grp=="4. Lower middle income" ~ "3-Lower middle income",
        income_grp=="5. Low income" ~ "4-Low income"
      ),
    #Adding this Oct 6, 2023 for clearer presentation of the second category
    income_grp_5_rename=case_when(
      income_grp=="1. High income: OECD" ~ "1. High income: OECD",
      income_grp=="2. High income: nonOECD" ~ "2. High income: non-OECD",
      income_grp=="3. Upper middle income" ~ "3. Upper middle income",
      income_grp=="4. Lower middle income" ~ "4. Lower middle income",
      income_grp=="5. Low income" ~ "5. Low income"
    )
  )

names(countries)

## Define a look-up for income group-----
#See comments below  - using data already in the countries file for now Mar 7 2023
lookup_income_grp = countries %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  distinct(name_en, income_grp, income_grp_4,income_grp_5_rename) 

setwd(here("data-processed"))
save(lookup_income_grp,file="lookup_income_grp.RData")

lookup_income_grp

lookup_income_grp_5_rename=lookup_income_grp %>% 
  distinct(income_grp, income_grp_4,income_grp_5_rename)
save(lookup_income_grp_5_rename,file="lookup_income_grp_5_rename.RData")

lookup_income_grp
class(countries)
names(countries)
head(countries)
#Bingo. The issue is that my country file isn't detailed enough,
#and it's excluding some coastal cities as a result.

#mapview(countries, zcol = "wb_a2")


# countries %>% 
#   filter(name_en == "United States of America") %>% 
#   mapview(zcol = "name_en")

#Explore some of the iso codes for joining with other country-level data
iso_codes = countries %>% 
  st_set_geometry(NULL) %>% 
  distinct(iso_a3, iso_a3_eh)

#Note the data do already have an income_grp. I wonder what the source is?
table(countries$income_grp)

countries %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(starts_with("name_en"), starts_with("iso_a3")) %>% 
  arrange(iso_a3_eh) %>% 
  print(n=300)

countries %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(income_grp, income_grp_4) %>% 
  summarise(n=n())


table(countries$economy)
#It looks like the one I should use is iso_a3_eh
#iso_codes %>% View()
table(iso_codes$iso_a3_eh)

# World Bank classification------
# Downloaded 2022 data
# see readme here
setwd(here("data-input","world-bank-income-classification"))
library(readxl)
countries_wb_class = read_xlsx("wb_class_edits.xlsx") %>% 
  #call code iso_a3_eh so it matches
  mutate(iso_a3_eh = code) 


table(countries_wb_class$income_group)#L, LM, UM, H

# Make sure that World Bank class will link to the countries loaded above
countries_w_wb = countries %>% 
  left_join(countries_wb_class, by = "iso_a3_eh")

table(countries_w_wb$income_grp)
table(countries_w_wb$income_group)

countries_w_wb %>% 
  st_set_geometry(NULL) %>% 
  group_by(income_group) %>% 
  summarise(n=n())
#41 missings. Okay, let's just use the stock one provided (even though I don't know source).
#Not worth time rn to figure out Mar 7 2023

countries_w_wb %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  filter(is.na(income_group)==TRUE) %>% 
  dplyr::select(starts_with("name_en"),starts_with("iso_a3"), starts_with("inco"))
  
countries_w_wb %>% 
  st_set_geometry(NULL) %>% 
  group_by(income_grp) %>% 
  summarise(n=n())


#Link with
# Global cities----
#maybe just download these?
#https://simplemaps.com/data/world-cities
#from https://simplemaps.com/data/world-cities
#Note there is a "pro" version for $200


## Simplemaps (source 1)----
setwd(here("data-input","simplemaps-worldcities-basicv1"))
cities_simplemaps_basic = readr::read_delim("worldcities.csv") %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  rename(
    city_id_simplemaps=id,
    city_name_simplemaps = city,
    city_population_simplemaps = population,
    country_name_simplemaps=country
  )

cities_simplemaps_basic
setwd(here("data-processed"))
save(cities_simplemaps_basic, file = "cities_simplemaps_basic.RData")
#42,905 - Oct 6, 2023: I wonder if this would pick up those missing from source 2

#for a given city id (use id, not name), what is country?
lookup_city_id_simple_maps_country = cities_simplemaps_basic %>% 
  st_set_geometry(NULL) %>% 
  distinct(country_name_simplemaps,city_id_simplemaps)

#previously called lookup_city_id_country
setwd(here("data-processed"))
save(lookup_city_id_simple_maps_country, file ="lookup_city_id_simple_maps_country.RData")


## Simplemaps Pro------
#Oct 6, 2023 - note this cost $200
setwd(here("data-input","simplemaps-worldcities-pro"))
cities_simplemaps_pro = readr::read_delim("worldcities.csv") %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  rename(
    city_id_simplemaps=id,
    city_name_simplemaps = city,
    city_population_simplemaps = population,
    country_name_simplemaps=country,
    admin_name_simplemaps=admin_name,#eg province or state
    iso3_simplemaps=iso3,#3-letter country abbrev
    admin_type_simplemaps=admin_type,
    admin_code_simplemaps=admin_code
  )

names(cities_simplemaps_pro)

setwd(here("data-processed"))
save(cities_simplemaps_pro, file = "cities_simplemaps_pro.RData")
table(cities_simplemaps_pro$country_name_simplemaps)
# cities_simplemaps_pro %>% 
#   filter(country_name_simplemaps=="United States") %>% 
#   filter(admin_name_simplemaps=="Michigan") %>% 
#   mapview()

names(cities_simplemaps_pro)
cities_simplemaps_pro
nrow(cities_simplemaps_pro)

lookup_all_vars_simplemaps_nogeo=cities_simplemaps_pro %>% 
  dplyr::select(city_id_simplemaps,ends_with("simplemaps")) %>% 
  st_set_geometry(NULL)

names(lookup_all_vars_simplemaps_nogeo)

## geonames (source 2)------
#alternate might be
#https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/
#I'm going to use the above for a few reason: looks to have been modified recently, 
#it follows more of an open-source ethos
#Updated October 9, 2023
library(here)
library(readr)
setwd(here("data-input","cities-public-opendatasoft"))
#Note the use of read_delim() instead of read_csv()
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
    city_name_geonames =`ASCII Name`#have to use back ticks because there are spaces
  ) %>% 
  rename(
    #To keep track of the data source, I'm adding "geonames" as a suffix,
    #since I'm dealing with so many data sources
    geoname_id = `Geoname ID`,
    country_name_geonames = `Country name EN`,
    city_population_geonames = `Population`,
    city_elevation_geonames = `Elevation`,
    city_date_modified_geonames= `Modification date`,
    city_name_accents_geonames = `Name`,#with possible accents per local name
    admin1_code_geonames=`Admin1 Code`,
    admin2_code_geonames=`Admin2 Code`

  ) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  dplyr::select(geoname_id, contains("geoname")) %>% 
  arrange(desc(city_population_geonames))

setwd(here("data-processed"))
save(cities_geonames, file = "cities_geonames.RData")

nrow(cities_geonames)#141121 on Oct 9, 2023
names(cities_geonames)
# cities_geonames %>% View()
cities_geonames %>%
  filter(city_population_geonames>1000000) %>%
  mapview()

cities_geonames %>% 
  filter(country_name_geonames=="United States") %>% 
  View()

#lookup for all geonames vars from the id
lookup_all_vars_geonames_nogeo=cities_geonames %>% 
  st_set_geometry(NULL) 

lookup_all_vars_geonames_nogeo %>% 
  filter(country_name_geonames=="United States")

# lookup for city population
lookup_city_id_city_population_geonames=cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(geoname_id, starts_with("city_pop"))


#lookup for city name
lookup_geoname_id_city_name = cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  distinct(geoname_id,city_name_geonames)

#for a given city id (use id, not name), what is country?
lookup_geoname_id_country_name = cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  distinct(geoname_id,country_name_geonames)



# Read City of Chicago data for example-----
library(sf)
library(here)
setwd(here("data-input"))
#https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-City/ewy2-6yfk
chicago_city_boundaries =st_read("city-of-chicago-boundaries") %>% 
  st_transform(4326)
chicago_city_boundaries
#chicago_city_boundaries %>% mapview()