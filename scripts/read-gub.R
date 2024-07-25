#This loads the city boundaries aka global urban boundaries (GUB)
#and calculates their area
#Began January 10, 2022
#Edits Oct 6, 2023 re. city names

library(raster)
library(terra)
library(here)
library(tidyverse)
library(mapview)
library(tidyterra)
library(sf)

# Load global urban boundary data-------
setwd(here("data-input", "global-urban-boundaries"))
getwd()
gub = st_read(dsn ="GUB_Global_2018") %>%
  st_transform(4326) %>% 
  #Oct 9, 2023: to avoid ambiguity with other area calculations, I'm adding a suffix
  mutate(  #measure area this way
    area_m2_gub = as.numeric(st_area(geometry)),
    area_km2_gub = area_m2_gub*1e-6 
  )
setwd(here("data-processed"))
save(gub, file = "gub.RData")
load("gub.RData")
nrow(gub)
names(gub)

gub %>% 
  st_set_geometry(NULL) %>% 
  ggplot()+
  geom_histogram(
    aes(area_km2_gub),bins=100)

#gub %>% mapview() #great - works!
#gub %>% plot()

#Link gub with countries to be sure
#This will take a while so be careful
gub_link_with_countries=gub %>% 
  st_intersection(countries)
gub_link_with_countries_nogeo=gub_link_with_countries %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()


names(gub_link_with_countries_nogeo)
countries_in_gub_data_iso_a3=gub_link_with_countries_nogeo %>% 
  group_by(iso_a3) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  rename(iso3_alpha_code=iso_a3) %>% 
  mutate(gub_present=1)

#how many countries with un data?
names(un_pop_deaths_2019)
nrow(un_pop_deaths_2019)
un_pop_deaths_2019
nrow(un_pop_deaths_2019)
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  left_join(countries_in_gub_data_iso_a3,by="iso3_alpha_code") %>% 
  group_by(gub_present) %>% 
  summarise(n=n())
un_pop_deaths_2019 %>% 
  left_join(countries_in_gub_data_iso_a3,by="iso3_alpha_code") %>% 
  filter(is.na(gub_present)==T) %>% 
  dplyr::select(contains("name")) %>% 
  View()
n_distinct(un_pop_deaths_2019$iso3_alpha_code)
gub_link_with_countries %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>%
  group_by(name_en) %>% 
  summarise(n=n()) %>% 
  print(n=200)

#confirm no data for british virgin islands?
countries
countries_bvi=countries %>% 
  filter(name_long=="British Virgin Islands")

gub_bvi=gub %>% 
  st_intersection(countries_bvi)

nrow(cities_simplemaps_pro)


## Lookups and modified versions-------
#a version without geometry for presentations
gub_nogeo = gub %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
setwd(here("data-processed"))
save(gub_nogeo, file = "gub_nogeo.RData")
#load("gub_nogeo.RData")
nrow(gub_nogeo)
summary(gub_nogeo$area_km2_gub)
gub_nogeo
### geometry lookup------
# lookup for the geometry
lookup_gub_orig_fid_geo = gub %>% 
  dplyr::select(ORIG_FID, geometry)

setwd(here("data-processed"))
save(lookup_gub_orig_fid_geo, file = "lookup_gub_orig_fid_geo.RData")

### lookup for area of GUB--------
lookup_gub_area_km2 = gub %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(ORIG_FID, starts_with("area"))

save(lookup_gub_area_km2, file = "lookup_gub_area_km2.RData")

# Link with city name data-----
## First link with simplemaps data------
#Changes here October 6, 2023
#8 pm
#I need a better way to keep track of the data source for city names.
#I have to add a suffix.

#845 pm
#I'm making simplemaps pro the main source ($200 USD) to avoid missings in geonames

source(here("scripts","read-boundaries-states-countries.R"))

#This takes longer than it did previously, but hopefully that means it's getting more matches.
gub_city_name_simplemaps = gub %>% 
  st_join(cities_simplemaps_pro) %>% 
  #if there are many points within a given polygon,
  #take the one with the largest population. 
  #that will be the corresponding name
  group_by(ORIG_FID) %>% 
  arrange(desc(city_population_simplemaps)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    city_name_simplemaps_miss=case_when(
      is.na(city_name_simplemaps)==T~1,
      TRUE~0
    )
  )

setwd(here("data-processed"))
save(gub_city_name_simplemaps,file="gub_city_name_simplemaps.RData")

#no geo version
gub_city_name_simplemaps_nogeo=gub_city_name_simplemaps %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() 

#save this
setwd(here("data-processed"))
save(gub_city_name_simplemaps_nogeo,file="gub_city_name_simplemaps_nogeo.RData")

### checks--------
gub_orig_fid_many_names=gub_city_name_simplemaps %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  group_by(ORIG_FID) %>% 
  summarise(n=n()) %>% 
  mutate(orig_fid_many_names = case_when(n>1~1,TRUE~0))

#good - none
gub_orig_fid_many_names %>% 
  filter(orig_fid_many_names==1)

# how many missing?
gub_city_name_simplemaps_nogeo %>% 
  group_by(city_name_simplemaps_miss) %>% 
  summarise( n=n()) %>% 
  ungroup() %>% 
  mutate(
    n_total=sum(n),
    prop=n/n_total)

#number of distinct countries in GUB data
gub_city_name_simplemaps_nogeo %>% 
  group_by(country_name_simplemaps) %>% 
  summarise(n=n())

### check missing names-----
#checks
table(gub_city_name_simplemaps_nogeo$city_name_simplemaps_miss)
#okay, still a fair number of missing but maybe not as many?

###lookups----

#lookup for the simplemaps code
lookup_gub_city_id_simplemaps=gub_city_name_simplemaps_nogeo %>% 
  distinct(ORIG_FID, city_id_simplemaps)%>% 
  as_tibble()

setwd(here("data-processed"))
save(lookup_gub_city_id_simplemaps,file="lookup_gub_city_id_simplemaps.RData")

#lookup for whether simplemaps is miss for that orig_fid
lookup_gub_simplemaps_miss=gub_city_name_simplemaps_nogeo %>% 
  distinct(ORIG_FID, city_name_simplemaps_miss) %>% 
  as_tibble()

setwd(here("data-processed"))
save(lookup_gub_simplemaps_miss,file="lookup_gub_simplemaps_miss.RData")
#lookup for whether simplemaps is missing

#note the rest of the simplemaps stuff can be linked in via the lookup created
#when the simplemaps data was loaded


## Link with geonames data-------
gub_city_name_geonames = gub %>% 
  st_join(cities_geonames) %>% 
  group_by(ORIG_FID) %>% 
  arrange(desc(city_population_geonames)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    city_name_geonames_miss=case_when(
      is.na(city_name_geonames)==T~1,
      TRUE~0
    )
  )

setwd(here("data-processed"))
save(gub_city_name_geonames,file="gub_city_name_geonames.RData")
gub_city_name_geonames
table(gub_city_name_geonames$city_name_geonames_miss)
setwd(here("data-processed"))
#load("gub_city_name_geonames.RData")
gub_city_name_geonames_nogeo=gub_city_name_geonames %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#what proportion of the GUBs have a name in the geonames data?
gub_city_name_geonames_nogeo %>% 
  group_by(city_name_geonames_miss) %>% 
  summarise( n=n()) %>% 
  ungroup() %>% 
  mutate(
    n_total=sum(n),
    prop=n/n_total)

###lookups----
#lookup for the geonames code
lookup_gub_geoname_id=gub_city_name_geonames_nogeo %>% 
  distinct(ORIG_FID, geoname_id)

setwd(here("data-processed"))
save(lookup_gub_geoname_id,file="lookup_gub_geoname_id.RData")

#lookup for whether geonames is miss for that orig_fid
lookup_gub_geonames_miss=gub_city_name_geonames_nogeo %>% 
  distinct(ORIG_FID, city_name_geonames_miss)

setwd(here("data-processed"))
save(lookup_gub_geonames_miss,file="lookup_gub_geonames_miss.RData")

## Fill in missings with geonames data------
#Can I fill in the missings with the geonames data?
names(gub_city_name_simplemaps)

lookup_gub_city_name_simplemaps_miss_geonames_present=gub_city_name_simplemaps %>% 
  filter(city_name_simplemaps_miss==1) %>% 
  left_join(lookup_gub_geoname_id,by="ORIG_FID") %>% 
  left_join(lookup_geoname_id_city_name,by="geoname_id") %>% 
  left_join(lookup_gub_geonames_miss,by="ORIG_FID") %>% 
  dplyr::select(ORIG_FID, contains("city_name"), 
                contains("city_id_simple"),
                contains("geoname_id")) %>% 
  filter(city_name_geonames_miss==0)

save(lookup_gub_city_name_simplemaps_miss_geonames_present,
     file="lookup_gub_city_name_simplemaps_miss_geonames_present.RData")

names(lookup_gub_city_name_simplemaps_miss_geonames_present)
nrow(lookup_gub_city_name_simplemaps_miss_geonames_present)

lookup_gub_city_name_simplemaps_miss_geonames_present


# Checks ---------
## Check out specific cities------out specific cities--------

### Tokyo----
# What ORIG_FID is Tokyo?
gub_city_name_geonames %>% 
  filter(city_name_geonames == "Tokyo")#2238

### Schenzhen-----
#What city does 2238 correspond to? It has a very large area
gub_city_name_geonames %>% 
  filter(ORIG_FID == "2238") #Shenzhen

### New York City v. Manhattan-------
#For the example, I should do New York.
#Which city is New York?
#begin with pixels of new york, then summarize to new york, etc
#or maybe Chicago since I know it better, and it might be easier to see
gub_city_name_geonames %>% 
  filter(city_name_geonames == "New York City")
gub %>% 
  filter(ORIG_FID=="60310") %>% 
  mapview()

cities_simplemaps_pro %>%
  filter(city_name_simplemaps=="Karaj")
cities_simplemaps_pro %>% 
  filter(city_name_simplemaps=="New York") %>% 
  mapview()
cities_simplemaps_pro %>% 
  filter(city_name_simplemaps=="Manhattan") %>% 
  mapview()

gub_city_name_simplemaps %>% 
  filter(city_name_simplemaps=="New York")
gub %>% 
  filter(ORIG_FID=="60268") %>% 
  mapview()#darn - this is long island.

#Manhattan. Oh - different Manhattan (Kansas)
gub %>% 
  filter(ORIG_FID=="56569") %>% 
  mapview()

# Manhattan, Illinois (phew)
gub %>% 
  filter(ORIG_FID=="55694") %>% 
  mapview()


gub_city_name_geonames %>% 
  filter(city_name_geonames == "Chicago")




gub %>% 
  filter(ORIG_FID=="55685") %>% 
  mapview()


## Reviewer 2 comment - Iranian cities-------
## Reviewer 2 comment: why do these cities not appera?
#e.g., in Iran There is no data for Ahvaz [Ahwaz] or #
#Yazd or Karaj, but for a very small city of Hashtgerd). 
cities_simplemaps_pro %>%
  filter(city_name_simplemaps=="Karaj") %>% 
  mapview()

#The reason Karaj is not included is presumably because it falls into Tehran
setwd(here("data-processed"))
load("gub_city_name_simplemaps_nogeo.RData")
gub_city_name_simplemaps_nogeo %>% 
  filter(city_name_simplemaps=="Tehran")


load("gub.RData")
gub %>% 
  filter(ORIG_FID==32914) %>% 
  mapview()

cities_simplemaps_pro %>%
  filter(city_name_simplemaps=="Yazd") %>% 
  mapview()
gub_city_name_simplemaps_nogeo %>% 
  filter(city_name_simplemaps=="Yazd")

yazd_city_name=cities_simplemaps_pro %>%
  filter(city_name_simplemaps=="Yazd")


#what about Yazd?
# yazd_city_name %>% 
#   st_intersection(gub) %>% 
#   mapview()
#It looks like Yazd is not in the data.
#Let's see what data we have over Iran
load("gub_city_name_geonames.RData")

#interestingly..no GUB data?
#let's check out the GUB data over Iran
#From the read-boundaries-states-countries.R code, we have country boundaries
names(countries)
iran_geo=countries %>% 
  filter(name_en=="Iran") %>% 
  dplyr::select(name_en) %>% 
  rename(country_name_en=name_en)

gubs_in_iran=gub %>% 
  st_intersection(iran_geo)

#No GUBs for Yazd, Iran
gubs_in_iran %>% mapview()

gub_city_name_geonames %>% 
  filter(country_name_simplemaps=="Iran")


## Intersect GUBs with geonames cities--------

#how many in the geonames data don't have a match in gub?
nrow(cities_geonames)
cities_geonames_intersect_gub=cities_geonames %>% 
  st_intersection(gub) %>% 
  mutate(geoname_intersects_gub=1)


nrow(cities_geonames)
nrow(cities_geonames_intersect_gub)
#Use the geonames dataset of cities with a population above 1,000

cities_geonames_intersect_gub_nogeo=cities_geonames_intersect_gub %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(geoname_intersects_gub=1) %>% 
  dplyr::select(geoname_id,geoname_intersects_gub)

names(cities_geonames)

cities_geonames %>% 
  filter(country_name_geonames=="United States") %>% 
  mapview()
cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  left_join(cities_geonames_intersect_gub_nogeo,by="geoname_id") %>% 
  mutate(geoname_is_in_gub=case_when(
    geoname_intersects_gub==1~1,
    TRUE~0
  )) %>% 
  group_by(geoname_is_in_gub) %>% 
  summarise(n=n())

cities_geoname_whether_intersects_gub=cities_geonames %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  left_join(cities_geonames_intersect_gub_nogeo,by="geoname_id") %>% 
  mutate(geoname_is_in_gub=case_when(
    geoname_intersects_gub==1~1,
    TRUE~0
  )) 

#save this so I can load it elsewhere
setwd(here("data-processed"))
save(cities_geoname_whether_intersects_gub, 
     file = "cities_geoname_whether_intersects_gub.RData")


#This analysis also exists in supp-tables
cities_geoname_whether_intersects_gub_country_summary=cities_geoname_whether_intersects_gub %>% 
  group_by(country_name_geonames,geoname_is_in_gub) %>% 
  summarise(
    n_cities=n(),
    city_pop_geonames_sum=sum(city_population_geonames,na.rm=T),
    city_pop_geonames_mean=mean(city_population_geonames,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(country_name_geonames) %>% 
  mutate(n_total_cities=sum(n_cities,na.rm=T),
         prop_of_cities=n_cities/n_total_cities,
         city_pop_geonames_sum_country=sum(city_pop_geonames_sum,na.rm=T),
         prop_of_pop=city_pop_geonames_sum/city_pop_geonames_sum_country
         ) %>% 
  ungroup()

cities_geoname_whether_intersects_gub_country_summary


#save this so I can print it as a table
setwd(here("data-processed"))
save(cities_geoname_whether_intersects_gub_country_summary, 
     file = "cities_geoname_whether_intersects_gub_country_summary.RData")


cities_geoname_whether_intersects_gub_country_summary %>% 
  arrange(country_name_geonames) %>% 
  print(n=400)


cities_geoname_whether_intersects_gub_country_summary %>% 
  dplyr::select(
    contains("country_name"),
    geoname_is_in_gub, n_cities, contains("prop"),contains("mean")) %>% 
  print(n=200)

cities_geoname_whether_intersects_gub_country_summary %>% 
#  filter(geoname_is_in_gub==1) %>% 
  arrange(prop) %>% 
  print(n=200)


#visualize them
lookup_geoname_is_in_gub=cities_geoname_whether_intersects_gub %>% 
  dplyr::select(geoname_id, contains("gub"))

table(lookup_geoname_is_in_gub$geoname_is_in_gub)
#how do those cities without a GUB look in the US?
cities_geonames %>% 
  left_join(lookup_geoname_is_in_gub,by="geoname_id") %>% 
  filter(country_name_geonames=="United States") %>% 
  mutate(geoname_is_in_gub_char=as.character(geoname_is_in_gub)) %>% 
  mapview(zcol="geoname_is_in_gub_char")

#How do they look in Iran?
table(cities_geonames$country_name_geonames)
cities_geonames %>% 
  left_join(lookup_geoname_is_in_gub,by="geoname_id") %>% 
  filter(country_name_geonames=="Iran, Islamic Rep. of") %>% 
  mutate(geoname_is_in_gub_char=as.character(geoname_is_in_gub)) %>% 
  mapview(zcol="geoname_is_in_gub_char")

#How many countries are represented in the GUB data?
n_distinct(cities_geonames$country_name_geonames)
cities_geonames
cities_geoname_whether_intersects_gub_country_summary %>% 
  group_by(geoname_is_in_gub)

#174 of the 230 have any data
cities_geoname_whether_intersects_gub %>% 
  filter(geoname_is_in_gub==1) %>% 
  group_by(country_name_geonames) %>% 
  summarise(n=n()) %>% 
  nrow()

cities_geoname_whether_intersects_gub %>% 
  filter(geoname_is_in_gub==1) %>% 
  group_by(country_name_geonames) %>% 
  summarise(n=n()) %>% 
  print(n=200)

# Restrict to Colorado-----
source(here("scripts", "read-boundaries-states-countries.R")) 

gub_colorado = gub %>% 
  st_intersection(colorado_boundary) 


#okay, it looks like the measure they give us is in square km
setwd(here("data-processed"))
save(gub_colorado, file = "gub_colorado.RData")

#okay, so to avoid these small fringe-like islands, we could just filter
#to areas above a certain value. How about the 5th percentile and above?
summary(gub_colorado$area_km2_quantile_5)
gub_colorado %>%
  mapview(zcol = "area_km2")

#Make a Colorado one that's simplified (smaller size) for visualization
gub_colorado_simplified = gub_colorado %>% 
  st_transform(2232) %>% 
  st_simplify(dTolerance = 500) %>% #simplify before mapview
  st_transform(4326)

gub_colorado_simplified %>% mapview()
save(gub_colorado_simplified, file = "gub_colorado_simplified.RData")
object.size(gub_colorado_simplified)
object.size(gub_colorado)
# Restrict to Georgia----
#I'm curious how Georgia looks
#loaded above

georgia_boundary %>% mapview()
st_crs(gub)
gub_georgia = gub %>% 
  st_intersection(georgia_boundary)

gub_georgia %>% mapview()
nrow(gub)
#Restrict to USA-------
sf::sf_use_s2(FALSE)
usa_boundaries_cont_48_union %>% mapview()
usa_boundaries_cont_48 %>% mapview()
gub_usa_48 = gub %>% #whatever I did sped this up considerably.
  st_make_valid() %>% 
  st_intersection(usa_boundaries_cont_48_union)%>% 
  st_make_valid() 

gub_usa_48
nrow(gub_usa_48)
setwd(here("data-processed"))
save(gub_usa_48, file = "gub_usa_48.RData")

#where is 54733
gub_usa_48 %>% filter(ORIG_FID=="54733") %>% mapview()




