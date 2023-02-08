#Read United Nations population data

#January 23, 2023
#Note that I also need the mortality data. Following methods noted in David's manuscript with Chinese colleagues, 
#I'm using the "deaths by select age groups" dataset under the "mortality" tab
#https://population.un.org/wpp/Download/Standard/Mortality/

#Note I had to delete the heading and modify it slightly so that it would load easier.
#I just care about the 20+ data, so I removed the other columns.
#Note reported deaths are in the thousands.
library(here)
library(tidyverse)
library(readxl)
setwd(here("data-input", "united-nations-mortality-data"))
un_deaths_2019 = read_excel("un-deaths-age-groups-both-sexes-modified.xlsx") %>% 
  mutate(deaths_absolute_20_plus_both_sexes = deaths_thousands_20_plus_both_sexes_num*1000) %>% 
  filter(Year==2019) %>% 
  mutate(id = row_number())

#Save
setwd(here("data-processed"))
save(un_deaths_2019, file = "un_deaths_2019.RData")

nrow(un_deaths_2019)
#Load population data as well so we can get a rate
setwd(here("data-input", "united-nations-mortality-data"))
un_pop_2019 = read_excel("un-pop-age-groups-both-sexes-modified.xlsx") %>% 
  mutate(
    pop_20_plus_both_sexes_absolute = pop_20_plus_both_sexes_thousands_num*1000,
    pop_total_absolute = pop_total_thousands_num*1000,
    pop_ratio_20_plus=pop_20_plus_both_sexes_absolute/pop_total_absolute
    ) %>% 
  #remove character suffixes
  dplyr::select(-contains("_char")) %>% 
  filter(Year==2019) %>% 
  mutate(id = row_number())

#un_pop_2019 %>% View()
names(un_pop_2019)
setwd(here("data-processed"))
save(un_pop_2019, file = "un_pop_2019.RData")

un_pop_2019_for_join = un_pop_2019 %>% 
  dplyr::select(id, starts_with("pop"))
  
un_pop_deaths_2019 = un_deaths_2019 %>% 
  left_join(un_pop_2019_for_join, by = "id") %>% 
  mutate(
    death_rate_20_plus = deaths_absolute_20_plus_both_sexes/pop_20_plus_both_sexes_absolute,
    death_rate_20_plus_per_1000 = death_rate_20_plus*1000
  )

setwd(here("data-processed"))
save(un_pop_deaths_2019, file = "un_pop_deaths_2019.RData")

un_pop_deaths_2019_usa = un_pop_deaths_2019 %>% 
  filter(region_subregion_country_or_area=="United States of America") %>% 
  #simplify columns. year is 2019 per above.
  dplyr::select(starts_with("region"), starts_with("death"), starts_with("pop"))


names(un_pop_deaths_2019_usa)