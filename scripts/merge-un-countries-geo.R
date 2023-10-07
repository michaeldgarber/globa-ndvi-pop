#This script works on merging the UN death / pop data with the 
#country data geometry
#Feb 9 2023

# Source scripts----
library(here)
source(here("scripts", "read-united-nations-gbd-data.R")) #formerly called read-united-nations-deaths.R
source(here("scripts", "read-boundaries-states-countries.R"))

# Initial checks----
# The name that seems to most closely match the United Nations data 
#in the country vector data is "name_en"
table(countries$name_en)
names(countries)

#How many rows in the country data?
nrow(countries)

#How many rows in the UN data?
nrow(un_pop_deaths_2019)
table(un_pop_deaths_2019$type)
table(un_pop_deaths_2019$region_subregion_country_or_area)
un_pop_deaths_2019

# Merge-------
names(un_pop_deaths_2019)
names(countries)
#what happens if I try to link based on those two columns?
countries_joined_with_un_pop_deaths = countries %>% 
  #get rid of population variables here to avoid confusion
  #with the UN estimates
  dplyr::select(-contains("pop_")) %>% 
  dplyr::select(-contains("type")) %>% 
  #Feb 9 2023 12:55 pm - fixed the names so they should all join now
  #make sure white space is trimmed off.
  mutate(name_en=trimws(name_en)) %>% 
  left_join(un_pop_deaths_2019, by = c("name_en"="country_name_en")) %>% 
  #an indicator for whether the join was successful
  mutate(
    join_worked = case_when(
      is.na(deaths_thousands_20_plus_both_sexes_num) ~ 0,
      TRUE ~1
    ),
    #October 6, 2023: updating country name to keep track of the data source
    #this ultimately comes from rnatural earth, so let's use ne as a suffix
    #Actually major backtrack. It's too hard to change now. Keep it _en
    #and just remember that it's from natural earth
    country_name_en = name_en 
  ) 

setwd(here("data-processed"))
save(countries_joined_with_un_pop_deaths , file = "countries_joined_with_un_pop_deaths.RData")

names(countries_joined_with_un_pop_deaths)
table(countries_joined_with_un_pop_deaths$join_worked)
table(countries_joined_with_un_pop_deaths$Year) #2019. no need to specify in name
#How many of the 177 joined? Okay, cool, just 30 didn't join, so go through them manually.
countries_to_check = countries_joined_with_un_pop_deaths  %>% 
  st_set_geometry(NULL) %>% 
  filter(join_worked ==0) %>% 
  dplyr::select(name_en) %>% 
  arrange(name_en)

countries_to_check #okay.
countries %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(name_en) %>% 
  arrange(name_en)
countries_to_check #go back to the UN code.


# A pared-down version-----
#a version with fewer variables
countries_joined_with_un_pop_deaths_pared = countries_joined_with_un_pop_deaths  %>% 
  dplyr::select(
    starts_with("country_name"), 
    contains("death"), 
    contains("pop_"), 
    contains("join")
    )

countries_joined_with_un_pop_deaths_pared
setwd(here("data-processed"))
save(
  countries_joined_with_un_pop_deaths_pared, 
     file = "countries_joined_with_un_pop_deaths_pared.RData"
  )

#a version without geometry
countries_joined_with_un_pop_deaths_pared_nogeo = countries_joined_with_un_pop_deaths_pared %>% 
  st_set_geometry(NULL) %>% 
  dplyr::as_tibble()


# Restrict to USA for some analyses------
countries_joined_with_un_pop_deaths_pared_usa = countries_joined_with_un_pop_deaths %>% 
  dplyr::select(
    starts_with("country_name"), 
    contains("death"), 
    contains("pop_"), 
    contains("join")) %>% 
  filter(country_name_en=="United States of America")

countries_joined_with_un_pop_deaths_pared_usa
names(countries_joined_with_un_pop_deaths_pared_usa)
