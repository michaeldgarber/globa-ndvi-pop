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

# Load deaths data by country------
setwd(here("data-input", "united-nations-mortality-data"))
un_deaths_2019 = read_excel("un-deaths-age-groups-both-sexes-modified.xlsx") %>% 
  mutate(deaths_absolute_20_plus_both_sexes = deaths_thousands_20_plus_both_sexes_num*1000) %>% 
  filter(Year==2019) %>% 
  mutate(row_id = row_number()) %>% 
  dplyr::select(-contains("_char"))  #remove _char suffixes

names(un_deaths_2019)
#Save
setwd(here("data-processed"))
save(un_deaths_2019, file = "un_deaths_2019.RData")
un_deaths_2019 %>% 
  n_distinct("Index")


# Load population data by country-----
#Load population data as well so we can get a rate
setwd(here("data-input", "united-nations-mortality-data"))
un_pop_2019 = read_excel("un-pop-age-groups-both-sexes-modified.xlsx") %>% 
  mutate(
    pop_20_plus_both_sexes_absolute = pop_20_plus_both_sexes_thousands_num*1000,
    pop_total_absolute = pop_total_thousands_num*1000,
    pop_ratio_20_plus=pop_20_plus_both_sexes_absolute/pop_total_absolute
    ) %>% 
  dplyr::select(-contains("_char")) %>% #remove _char suffixes
  filter(Year==2019) %>% 
  mutate(row_id = row_number())

un_deaths_2019 %>% 
  n_distinct("un_pop_2019")

nrow(un_pop_2019)
#un_pop_2019 %>% View()
names(un_pop_2019)
un_pop_2019
table(un_pop_2019$region_subregion_country_or_area)

#explore ratio of 20 plus
summary(un_pop_2019$pop_ratio_20_plus)

setwd(here("data-processed"))
save(un_pop_2019, file = "un_pop_2019.RData")

un_pop_2019_for_join = un_pop_2019 %>% 
  dplyr::select(Index, starts_with("pop")) #join by index, actually

# Link them and data wrangling------
un_pop_2019_for_join
nrow(un_deaths_2019) #same number of rows, so a row-number-based ID should work.
names(un_deaths_2019)
un_pop_deaths_2019 = un_deaths_2019 %>% 
  left_join(un_pop_2019_for_join, by = "Index") %>% 
  mutate(
    death_rate_20_plus = deaths_absolute_20_plus_both_sexes/
      pop_20_plus_both_sexes_absolute,
    death_rate_20_plus_per_1000 = death_rate_20_plus*1000
  ) %>% 
  #There is a column called "Type" in UN pop & death data to indicate whether the 
  #column "region_subregion_country_or_area"
  #is a country or a broader region
  #If it's a country, its value is "Country/Area". Change that in the other code first.
  filter(Type == "Country/Area") %>% 
  #We have to change the names of 30 countries so that they join
  #with the country geometry file. Go through them one by one.
  #See merge-un-counties-geo.R for which didn't link
  mutate(
    #make sure white space is trimmed off.
    region_subregion_country_or_area=trimws(region_subregion_country_or_area),
    country_name_en =
      case_when(
        #note Antarctica is not in the UN data - at least as a country - can ignore
        region_subregion_country_or_area == "Bolivia (Plurinational State of)" ~ "Bolivia",
        region_subregion_country_or_area == "Brunei Darussalam" ~ "Brunei",
        region_subregion_country_or_area == "Czechia" ~ "Czech Republic",
        region_subregion_country_or_area == "Timor-Leste" ~ "East Timor",
        region_subregion_country_or_area == "Eswatini" ~ "eSwatini",#strange spelling in country file
        region_subregion_country_or_area == "Falkland Islands (Malvinas)" ~ "Falkland Islands",
        #French Southern and Antarctic Lands not in UN data
        region_subregion_country_or_area == "Iran (Islamic Republic of)" ~ "Iran",
        region_subregion_country_or_area == "Côte d'Ivoire" ~ "Ivory Coast",
        region_subregion_country_or_area == "Kosovo (under UNSC res. 1244)" ~ "Kosovo",
        region_subregion_country_or_area == "Lao People's Democratic Republic" ~ "Laos",
        region_subregion_country_or_area == "Republic of Moldova" ~ "Moldova",
        region_subregion_country_or_area == "Dem. People's Republic of Korea" ~ "North Korea",
        region_subregion_country_or_area == "State of Palestine" ~ "Palestine",
        region_subregion_country_or_area == "China" ~ "People's Republic of China",
        region_subregion_country_or_area == "North Macedonia" ~ "Republic of Macedonia",
        #okay, note that in the countries file, there is 
        # Democratic Republic of the Congo as well as Republic of the Congo  
        #Republic of Congo is the one without a match, which corresponds to simply "Congo"
        region_subregion_country_or_area == "Congo" ~ "Republic of the Congo",
        region_subregion_country_or_area == "Russian Federation" ~ "Russia",
        #Note there doesn't seem to be a match for Somaliland in the country vector file:
        #https://www.actionaid.org.uk/about-us/where-we-work/somaliland/somalia-somaliland-differences-explained
        region_subregion_country_or_area == "Republic of Korea" ~ "South Korea",
        region_subregion_country_or_area == "Syrian Arab Republic" ~ "Syria",
        region_subregion_country_or_area == "China, Taiwan Province of China" ~ "Taiwan",
        region_subregion_country_or_area == "United Republic of Tanzania" ~ "Tanzania",
        region_subregion_country_or_area == "Bahamas" ~ "The Bahamas",
        region_subregion_country_or_area == "Gambia" ~ "The Gambia",
        region_subregion_country_or_area == "Türkiye" ~ "Turkey",
        #apparently disputed, but included in Cyprus by most: https://en.wikipedia.org/wiki/Northern_Cyprus
        region_subregion_country_or_area == "Cyprus" ~ "Turkish Republic of Northern Cyprus",
        region_subregion_country_or_area == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
        region_subregion_country_or_area == "Viet Nam" ~ "Vietnam",
        
        #if else, set it to:
        TRUE ~ region_subregion_country_or_area
      )
  )

setwd(here("data-processed"))
save(un_pop_deaths_2019, file = "un_pop_deaths_2019.RData")

#to get the list of UN countries in which to search
un_pop_deaths_2019 %>%
  dplyr::select(region_subregion_country_or_area) %>%
  arrange(region_subregion_country_or_area) #%>%
#  View()

names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"), starts_with("region")) %>%
  arrange(country_name_en) %>% 
  print(n=250)

nrow(un_pop_deaths_2019)



un_pop_deaths_2019_usa = un_pop_deaths_2019 %>% 
  filter(region_subregion_country_or_area=="United States of America") %>% 
  #simplify columns. year is 2019 per above.
  dplyr::select(starts_with("region"), starts_with("death"), starts_with("pop"))


names(un_pop_deaths_2019_usa)