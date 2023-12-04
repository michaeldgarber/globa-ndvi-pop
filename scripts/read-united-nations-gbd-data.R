#Read United Nations population data

#January 23, 2023
#Note that I also need the mortality data. Following methods noted in David's manuscript with Chinese colleagues, 
#I'm using the "deaths by select age groups" dataset under the "mortality" tab
#https://population.un.org/wpp/Download/Standard/Mortality/

#Note I had to delete the heading and modify it slightly so that it would load easier.
#I just care about the 20+ data, so I removed the other columns.
#Note reported deaths are in the thousands.

#May 15th, 2023
#I'm here again for the WHO urban health project. For that project, I need to do an empirical analysis
#comparing GBD deaths and proportion urban. Given the proportion urban comes from 
#UN, I will be able to use many of this country-level work.

#As a result, I'm renaming the script name from
#read-united-nations-deaths.R
#to
#read-united-nations-gbd-data

#Sep 14, 2023
#Updating script to include life expectancy as well


library(here)
library(tidyverse)
library(readxl)

# Load deaths data by country------
#May 15, 2023: call n_deaths n_deaths - easier
setwd(here("data-input", "united-nations-mortality-data"))
un_deaths_2019 = read_excel("un-deaths-age-groups-both-sexes-modified.xlsx") %>% 
  rename(location_code=`Location code`) %>% 
  rename_with(tolower) %>% #May 15th, 2023 - this is amazing. all lowercase
  mutate(n_deaths_20_plus_both_sexes = deaths_thousands_20_plus_both_sexes_num*1000) %>% 
  filter(year==2019) %>% 
  mutate(row_id = row_number()) %>% 
  dplyr::select(-contains("_char"))  #remove _char suffixes

names(un_deaths_2019)
#Save
setwd(here("data-processed"))
save(un_deaths_2019, file = "un_deaths_2019.RData")
un_deaths_2019 %>% 
  n_distinct("index")


# Load population data by country-----
#Load population data as well so we can get a rate
#Note population data corresponds to
#'Total Population, as of 1 July (thousands) 
# #In the total population file available here
# https://population.un.org/wpp/Download/Standard/MostUsed/
setwd(here("data-input", "united-nations-mortality-data"))
un_pop_2019 = read_excel("un-pop-age-groups-both-sexes-modified.xlsx") %>% 
  rename(location_code=`Location code`) %>% #fix this
  rename_with(tolower) %>% #May 15th, 2023 - this is amazing. all lowercase
  mutate(
    pop_20_plus_both_sexes_absolute = pop_20_plus_both_sexes_thousands_num*1000,
    pop_total_absolute = pop_total_thousands_num*1000,
    pop_ratio_20_plus=pop_20_plus_both_sexes_absolute/pop_total_absolute
    ) %>% 
  dplyr::select(-contains("_char")) %>% #remove _char suffixes
  filter(year==2019) %>% 
  mutate(row_id = row_number())

names(un_pop_2019)

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



# Load proportion urban by country-------
#This is for the WHO urban health paper, but it won't hurt to have it in this same pipeline.
setwd(here("data-input","united-nations-urban-and-gbd"))
un_prop_urban = read_excel("WUP2018-F21-Proportion_Urban_Annual-modified.xlsx") %>% 
  #Call the Country code location code as it is above
  rename(location_code=`Country code`) %>% 
  rename_with(tolower) %>%  #May 15th, 2023 - this is amazing. all lowercase
  #Note this is from 2019 - don't need to say in variable name
  rename(perc_urban=perc_urban_2019) %>% 
  mutate(
    #I changed this from prop_urban to prop_urban_pop to distinguish from proportion
    #of urban deaths
    prop_urban_pop=perc_urban/100
  )

un_prop_urban_for_join=un_prop_urban %>% 
  dplyr::select(starts_with("location"),contains("urban"))

un_prop_urban_for_join
names(un_prop_urban_for_join)

#Okay, it looks like I can link by location code - great
#Downloaded from
#https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

# Load a country-name  - code - look-up table---------
#May 15th 2023
#This workbook is simply a copy of these three columns in the UN deaths data. 
#That data was long-form with a variable for year, so this needs to distinctify that.
setwd(here("data-input", "united-nations-country-lookup"))
lookup_un_country_code_three_letter_code= read_excel("country-code-lookup.xlsx") %>% 
  rename_with(tolower) %>%  #May 15th, 2023 - this is amazing. all lowercase
  rename(
    location_code=`location code`,
    iso3_alpha_code=`iso3 alpha-code`,
    iso2_alpha_code=`iso2 alpha-code`,
         ) %>% 
  distinct()
lookup_un_country_code_three_letter_code %>% 
  arrange(iso2_alpha_code)

names(lookup_un_country_code_three_letter_code)
nrow(lookup_un_country_code_three_letter_code)


# Load World Bank income classification------
setwd(here("data-input", "world-bank-income-classification"))
wb_class = read_excel("wb_class_edits.xlsx") %>% 
  rename(
    #wb for world bank to remember that it came from that data
    country_name_wb=economy,
    iso3_alpha_code=code,#presumably this is true
    region_wb_dl=region,#region_wb conflicts downstream. call it region_wb_dl for downloaded
    income_grp_wb=income_group
    ) %>% 
  #Make wb_class a factor for plotting
  mutate(income_grp_wb_fac=fct_relevel(
    income_grp_wb,
    "High income","Upper middle income","Lower middle income","Low income")
    )

table(wb_class$income_grp_wb)
table(wb_class$income_grp_wb_fac)

#Will this link with the pop data?
names(un_deaths_2019)
class(un_deaths_2019$location_code)
class(lookup_un_country_code_three_letter_code$location_code)
un_pop_deaths_2019_w_wb=un_deaths_2019 %>%
  left_join(lookup_un_country_code_three_letter_code,by="location_code") %>% 
  left_join(wb_class, by ="iso3_alpha_code")

names(un_pop_deaths_2019_w_wb)

un_pop_deaths_2019_w_wb %>% 
  filter(type == "Country/Area") %>% 
  dplyr::select(starts_with("location"),contains("code"),
                contains("wb"),contains("country_name")) %>% 
  print(n=300)

# Load Gini coefficient data----
#from World Bank
#Added Dec 4, 2023
#The data are wide form where countries are the row and the data are
#in the year
setwd(here("data-input","world-bank-gini-coefficient"))
gini_by_country=read_excel("gini-by-country-modified.xlsx") %>% 
  #Call the three-letter abbreviation this iso3...var name
  #so it links elsewhere
  rename(iso3_alpha_code=`Country Code`) %>% 
  #remove vars I don't need
  dplyr::select(-`Country Name`, -`Indicator Name`, -`Indicator Code`) %>% 
  pivot_longer(
    !iso3_alpha_code, names_to = "year", values_to = "gini") %>% 
  #Remove all missings
  filter(is.na(gini)==F) %>% 
  #and now just take the most recent value
  group_by(iso3_alpha_code) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  #and can rename year so it travels with the gini coefficient
  rename(gini_most_recent_year=year) %>% 
  ungroup() %>%  #done
  #create tertiles and quartiles of the gini coefficient
  mutate(
    gini_tertile=cut_number(gini,n=3),
    gini_quartile=cut_number(gini,n=4)
  )

table(gini_by_country$gini_tertile)
table(gini_by_country$gini_quartile)

#save this as a look-up for elsewhere
setwd(here("data-processed"))
save(gini_by_country,file="gini_by_country.RData")

#for later on, I need a lookup with the iso3

# Load GBD data by country------
#deaths and DALYs (numbers, percentages, and rates – all ages, both sexes, 2019) 
#for all countries and by WHO region.
setwd(here("data-input","united-nations-urban-and-gbd"))
gbd_by_pais=read_excel("GBD_deaths_by_country.xlsx") %>% 
  rename(location_name_gbd=location) %>% 
  #To improve link, change name of a few
  mutate(
    #still need CÃ´te d'Ivoire
    #    Democratic People's Republic of Korea
    country_name_gbd=case_when(
      location_name_gbd=="Taiwan (Province of China)"~"China, Taiwan Province of China",
      location_name_gbd=="Micronesia (Federated States of)" ~"Micronesia (Fed. States of)",
      location_name_gbd=="Turkey"~"Türkiye",
      location_name_gbd=="Palestine"~"State of Palestine",
      location_name_gbd== "CÃ´te d'Ivoire"~"Côte d'Ivoire",
      location_name_gbd=="Democratic People's Republic of Korea"~"Dem. People's Republic of Korea",
      TRUE ~location_name_gbd
    )) %>% 
  mutate(gbd_flag=1)

gbd_by_pais
## GBD death rate----------
#May 19th 2023 Note the reported rate is per 100,000! see GBD website
#https://vizhub.healthdata.org/gbd-results/
gbd_by_pais_deaths_rate=gbd_by_pais %>% 
  filter(measure=="Deaths") %>% 
  filter(metric=="Rate")

#a wide-form version by country

gbd_by_pais_death_rate_wide=gbd_by_pais%>% 
  filter(measure=="Deaths") %>% 
  filter(metric=="Rate") %>% 
  #I have to include the _gbd suffix because elsewhere I have death rates as obtained from gbd
  #pt for point estimate
  rename(
    death_rate_per_100k_gbd_pt=val,
    death_rate_per_100k_gbd_ul=upper,#upper limit
    death_rate_per_100k_gbd_ll=lower#upper limit
  ) %>% 
  mutate(
    #I'd like a version that's not per 100k so I can re-calculate
    #counts more simply
    death_rate_gbd_pt=death_rate_per_100k_gbd_pt/100000,
    death_rate_gbd_ul=death_rate_per_100k_gbd_ul/100000 ,
    death_rate_gbd_ll=death_rate_per_100k_gbd_ll/100000  
  ) %>% 
  dplyr::select(contains("country_name"),contains("location"), contains("death_rate"),gbd_flag)


#gbd_by_pais_death_rate_wide %>% View()
#gbd_by_pais_deaths_rate %>% View()
gbd_by_pais_deaths_rate %>% 
  group_by(gbd_flag) %>% 
  summarise(n=n())#204

#Can GBD be linked using name?
un_pop_deaths_2019_w_gbd=un_deaths_2019 %>% 
  #  left_join(gbd_by_pais_deaths_rate,by=c("country_name_en"="location_name"))
  #This only misses 6
  left_join(gbd_by_pais_death_rate_wide,by=c("region_subregion_country_or_area"="country_name_gbd"))

names(un_pop_deaths_2019_w_gbd)
un_pop_deaths_2019_w_gbd_yes_link=un_pop_deaths_2019_w_gbd %>% 
  filter(is.na(gbd_flag)==FALSE) %>% 
  mutate(linked_w_un=1) %>% 
  dplyr::select(starts_with("location"),linked_w_un)

un_pop_deaths_2019_w_gbd_yes_link
gbd_by_pais_death_rate_wide_didnt_link=gbd_by_pais_death_rate_wide %>% 
  left_join(un_pop_deaths_2019_w_gbd_yes_link,by="location_name_gbd") %>% 
  filter(is.na(linked_w_un)==TRUE) %>% 
  dplyr::select(contains("gbd")) 

gbd_by_pais_death_rate_wide_didnt_link
#Now link this with the original GBD - which didn't link?

## GBD n_deaths------
#I want total number of deaths as well. Use their numbers, as it may be age-adjusted, etc.,
#Confirmed this is the absolute number and not expressed per 1,000 etc.
#and may not be a simple n/population
#gbd_by_pais_n_deaths_wide
#I call these n_deaths elsewhere (formerly deaths_absolute) - use that framework again
gbd_by_pais_n_deaths_wide=gbd_by_pais%>% 
  filter(measure=="Deaths") %>% 
  filter(metric=="Number") %>% 
  rename(
    n_deaths_gbd_pt=val,
    n_deaths_gbd_ul=upper,#upper limit
    n_deaths_gbd_ll=lower#upper limit
  ) %>% 
  dplyr::select(country_name_gbd, contains("n_deaths"))

gbd_by_pais_n_deaths_wide

gbd_by_pais_n_deaths_wide
#Note this
#region_subregion_country_or_area
#gets more countries than
#country_name_en below
un_pop_deaths_2019_w_gbd %>% 
  group_by(gbd_flag) %>% 
  summarise(n=n())
un_pop_deaths_2019_w_gbd %>% 
  filter(is.na(gbd_flag)==TRUE) %>% 
  dplyr::select(starts_with("region")) %>% 
  arrange(region_subregion_country_or_area) %>% 
  print(n=40)

un_pop_deaths_2019_w_gbd %>% 
  dplyr::select(starts_with("region")) %>% 
  arrange(region_subregion_country_or_area) %>% 
#  View()
  print(n=300)

## GBD life expectancy-------
setwd(here("data-input","united-nations-urban-and-gbd"))
#note - both sexes, life expectancy at birth, year 2019
gbd_by_pais_le_wide=read_excel("GBD_LE_by_country.xlsx") %>% 
  dplyr::select(location_name, val, upper, lower) %>% 
  rename(le_birth_pt=val,
         le_birth_ul=upper,
         le_birth_ll=lower,
         location_name_gbd=location_name #to be consistent with how I name it earlier
         )

gbd_by_pais_le_wide

## Link all of the GBD data------
names(gbd_by_pais_death_rate_wide)
names(gbd_by_pais_n_deaths_wide)
gbd_wide=gbd_by_pais_death_rate_wide %>% 
  left_join(gbd_by_pais_n_deaths_wide,by="country_name_gbd") %>% 
  left_join(gbd_by_pais_le_wide,by="location_name_gbd")


# Link GBD data to UN data and further data wrangling------
un_pop_2019_for_join = un_pop_2019 %>% 
  dplyr::select(location_code, starts_with("pop")) #join by index, actually

un_pop_2019_for_join
nrow(un_deaths_2019) #same number of rows, so a row-number-based ID should work.
names(un_deaths_2019)
names(un_prop_urban)
un_prop_urban %>% 
  arrange(location_code)
un_deaths_2019 %>% 
  arrange(location_code)
#life expectancy added as of Sep 14, 2023
un_pop_deaths_2019 = un_deaths_2019 %>% 
  left_join(gbd_wide,by=c("region_subregion_country_or_area"="country_name_gbd")) %>% 
  left_join(lookup_un_country_code_three_letter_code,by="location_code") %>% 
  left_join(wb_class, by ="iso3_alpha_code") %>% 
  left_join(un_pop_2019_for_join, by = "location_code") %>% #link with this May 15th 2023
  left_join(un_prop_urban_for_join,by="location_code") %>% 
  left_join(gini_by_country, by ="iso3_alpha_code") %>% #added Dec 4, 2023
  mutate(
    death_rate_20_plus = n_deaths_20_plus_both_sexes/
      pop_20_plus_both_sexes_absolute,
    death_rate_20_plus_per_1000 = death_rate_20_plus*1000,
    
    #recalculate the number of deaths using GBD rate and UN population
    #to see how close it is to the other GBD estimates.
    #Calculated from UN pop and the reported rate
    n_deaths_gbd_un_pop_pt=death_rate_gbd_pt*pop_total_absolute,#pop comes from UN above
    n_deaths_gbd_un_pop_ul=death_rate_gbd_ul*pop_total_absolute,#pop comes from UN above
    n_deaths_gbd_un_pop_ll=death_rate_gbd_ll*pop_total_absolute,#pop comes from UN above
    
    #ratio of the two n_deaths_gbd_atl measures so I can easily compare
    n_deaths_gbd_pt_ratio_of_measures=n_deaths_gbd_pt/n_deaths_gbd_un_pop_pt
  ) %>% 
  #There is a column called "Type" in UN pop & death data to indicate whether the 
  #column "region_subregion_country_or_area"
  #is a country or a broader region
  #If it's a country, its value is "Country/Area". Change that in the other code first.
  filter(type == "Country/Area") %>% 
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
        #apparently disputed, but included in Cyprus by most: 
        #https://en.wikipedia.org/wiki/Northern_Cyprus
        #For simplicity and to minimize missings, include all of these in Cyprus
#        region_subregion_country_or_area == "Cyprus" ~ "Turkish Republic of Northern Cyprus",
        region_subregion_country_or_area == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
        region_subregion_country_or_area == "Viet Nam" ~ "Vietnam",

        # Nov 19, 2023: making a few more edits so they
        #join downstream
        region_subregion_country_or_area=="China, Hong Kong SAR"~ "Hong Kong",
        region_subregion_country_or_area=="China, Macao SAR"~"Macau",
        
        #if else, set it to:
        TRUE ~ region_subregion_country_or_area
      ),
    
    #Calculate number of deaths in urban settings, assuming overall rate is the same
    #in urban vs rural - and add bounds as well
    n_deaths_urban_gbd_pt=prop_urban_pop*n_deaths_gbd_pt,
    n_deaths_urban_gbd_ll=prop_urban_pop*n_deaths_gbd_ll,
    n_deaths_urban_gbd_ul=prop_urban_pop*n_deaths_gbd_ul,
    
    #and do the same thing using the alternate n_deaths_calc
    n_deaths_urban_gbd_un_pop_pt=prop_urban_pop*n_deaths_gbd_un_pop_pt,
    n_deaths_urban_gbd_un_pop_ll=prop_urban_pop*n_deaths_gbd_un_pop_ll,
    n_deaths_urban_gbd_un_pop_ul=prop_urban_pop*n_deaths_gbd_un_pop_ul,
    
    #also would be good to have total population urban for ref
    pop_urban=pop_total_absolute*prop_urban_pop,
    
    #so that I can apply the same method throughout, add this here
    irr_ur=1 #so I can use the same formula
  ) %>% 
  #remove anything with a dash
  dplyr::select(-contains("-"))

setwd(here("data-processed"))
save(un_pop_deaths_2019, file = "un_pop_deaths_2019.RData")

# checks and vis------
#un_pop_deaths_2019 %>% View()
#to get the list of UN countries in which to search
# un_pop_deaths_2019 %>%
#   dplyr::select(region_subregion_country_or_area) %>%
#   arrange(region_subregion_country_or_area) %>%
#   View()

names(un_pop_deaths_2019)
table(un_pop_deaths_2019$country_name_en)
table(un_pop_deaths_2019$iso3_alpha_code)
n_distinct(un_pop_deaths_2019$country_name_en)
n_distinct(un_pop_deaths_2019$location_code)
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"), starts_with("region"),contains("wb")) %>%
  arrange(country_name_en) %>% 
  print(n=250)

nrow(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"),contains("code")) %>% 
  arrange(location_code)

nrow(gini_by_country)
#how many of the gini coefficients linked?
#all of them. nice.
un_pop_deaths_2019 %>% 
  filter(is.na(gini)==F) %>% 
  nrow()


# un_pop_deaths_2019 %>% 
#   distinct(country_name_en) %>% View()

### lookup for country name and population-----
#I need this to adjust the Landscan data - Nov 19, 2023

lookup_country_name_en_pop_total_absolute=un_pop_deaths_2019 %>% 
  dplyr::select(country_name_en, pop_total_absolute)

#lookup_country_name_en_pop_total_absolute %>% View()

setwd(here("data-processed"))
save(
  lookup_country_name_en_pop_total_absolute,
     file="lookup_country_name_en_pop_total_absolute.RData")
lookup_country_name_en_pop_total_absolute %>% arrange(desc(pop_total_absolute))

#check. does this add up to 7.7 billion?
lookup_country_name_en_pop_total_absolute %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_total_absolute=sum(pop_total_absolute,na.rm=T))

#How close are the two n_deaths GBD calculations?
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"),contains("n_deaths_gbd")) 

names(un_pop_deaths_2019)
#okay good - most are 1, but not all
un_pop_deaths_2019 %>% 
  ggplot(aes(x=n_deaths_gbd_pt_ratio_of_measures))+
  geom_histogram()

#so it's probably best to use the number of deaths using the rate* pop. estimates
#so that I can be internally consistent

## overall number of deaths annually - crude estimate, assuming IRR=1
table(un_pop_deaths_2019$irr_ur)
un_pop_deaths_2019 %>% 
  filter(irr_ur==1) %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    n_deaths_gbd_pt=sum(n_deaths_gbd_pt,na.rm=TRUE),
    pop_total_absolute=sum(pop_total_absolute,na.rm=TRUE),
    pop_urban=sum(pop_urban,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(death_rate_gbd_pt=n_deaths_gbd_pt/pop_total_absolute,
         n_deaths_urban_gbd_pt=death_rate_gbd_pt*pop_urban
         )

names(un_pop_deaths_2019)
##lookup country name - 3-letter code------
lookup_country_name_en_iso3_alpha_code=un_pop_deaths_2019 %>% 
  distinct(country_name_en,iso3_alpha_code)

  
## Plots of perc urban vs ------
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_20_plus))+
  geom_point(aes(colour=income_grp_wb_fac))

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_gbd_pt))+
  geom_point(aes(colour=income_grp_wb_fac))+
  geom_smooth()

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_gbd_pt))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows=vars(income_grp_wb_fac))

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=n_deaths_gbd_pt))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows=vars(income_grp_wb_fac))

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_20_plus))+
  geom_point(aes(colour=region_wb_dl))

un_pop_deaths_2019_usa = un_pop_deaths_2019 %>% 
  filter(region_subregion_country_or_area=="United States of America") %>% 
  #simplify columns. year is 2019 per above.
  dplyr::select(starts_with("region"), starts_with("death"), starts_with("pop"))

un_pop_deaths_2019 %>% 
  group_by(gbd_flag) %>% 
  summarise(n=n())

## total number of deaths - urban - by country------
#assuming rate is the same in urban and rural

un_pop_deaths_2019 %>% nrow()
un_pop_deaths_2019 %>% 
  arrange(desc(n_deaths_urban_gbd_pt)) %>% 
  dplyr::select(contains("country_name"),contains("n_deaths_urban_gbd_pt"),contains("urban")) %>% 
  print(n=250)
un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=n_deaths_urban_gbd_pt))+
  geom_point(aes(colour=region_wb_dl))

#overall summary
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(n_deaths_urban_gbd_pt=sum(n_deaths_urban_gbd_pt,na.rm=TRUE))
#32 million in 2019

#By World Bank region
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  group_by(region_wb_dl) %>% 
  summarise(
    # n_deaths_urban_gbd_pt=sum(n_deaths_urban_gbd_pt,na.rm=TRUE),
    # n_deaths_urban_gbd_ll=sum(n_deaths_urban_gbd_ll,na.rm=TRUE),
    # n_deaths_urban_gbd_ul=sum(n_deaths_urban_gbd_ul,na.rm=TRUE),
    n_deaths_urban_gbd_un_pop_pt=sum(n_deaths_urban_gbd_un_pop_pt,na.rm=TRUE),
    n_deaths_urban_gbd_un_pop_ll=sum(n_deaths_urban_gbd_un_pop_ll,na.rm=TRUE),
    n_deaths_urban_gbd_un_pop_ul=sum(n_deaths_urban_gbd_un_pop_ul,na.rm=TRUE)
    ) %>% 
  ungroup() %>% 
  arrange(desc(n_deaths_urban_gbd_un_pop_pt))

un_pop_deaths_2019 %>% 
  group_by(region_wb_dl) %>% 
  summarise(n_deaths_gbd_pt=sum(n_deaths_gbd_pt,na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(n_deaths_gbd_pt))

#Top 10 countries
un_pop_deaths_2019 %>% 
  dplyr::select(starts_with("country"), starts_with("n_deaths")) %>% 
  arrange(desc(n_deaths_urban_gbd_pt)) 

79/77

## life expectancy by region (unweighted mean)
names(un_pop_deaths_2019)
#Total population data where le_birth_pt is non-missing
un_pop_deaths_2019 %>% 
  mutate(pop_le_product=pop_total_absolute*le_birth_pt) %>% 
  group_by(region_wb_dl) %>% 
  summarise(
    sum_of_pop_le_product =sum(pop_le_product,na.rm=TRUE),
    pop_total_absolute_sum=sum(pop_total_absolute,na.rm=TRUE),
    le_birth_pt_mean=mean(le_birth_pt, na.rm=TRUE),
    #I've actually never used the weighted mean function before, but it seems to work.
    #I should check it to make sure it's right.
    #https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/weighted.mean
    le_birth_pt_mean_weighted=weighted.mean(
      x=le_birth_pt,
      w=pop_total_absolute,
      na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(le_birth_pt_mean_weighted_calc=sum_of_pop_le_product /pop_total_absolute_sum)
#they're the same, almost. neat. use your direct calculation.

# Try different urban-rural rate ratios--------
names(un_pop_deaths_2019)
#This can be a function where I vary this
#irr_ur_val=2


estimate_urban_ir=function(irr_ur_val){
  
  hi=un_pop_deaths_2019 %>% 
    mutate(
      #Try different urban-rural rate ratios
      #See equation in paper.
      irr_ur=irr_ur_val,
      irr_ur_cat=as.character(irr_ur),
      death_rate_urban_gbd_pt= (death_rate_gbd_pt*irr_ur)/((prop_urban_pop*irr_ur)+(1-prop_urban_pop)),
      death_rate_urban_gbd_ll= (death_rate_gbd_ll*irr_ur)/((prop_urban_pop*irr_ur)+(1-prop_urban_pop)),
      death_rate_urban_gbd_ul= (death_rate_gbd_ul*irr_ur)/((prop_urban_pop*irr_ur)+(1-prop_urban_pop)),
      
      #Note multiply by the urban population
      n_deaths_urban_gbd_un_pop_pt=death_rate_urban_gbd_pt*pop_urban,
      n_deaths_urban_gbd_un_pop_ll=death_rate_urban_gbd_ll*pop_urban,
      n_deaths_urban_gbd_un_pop_ul=death_rate_urban_gbd_ul*pop_urban#Fixed this June 30, 2023
    ) 
  return(hi)
  
}

#List of possible IRRs
#Decide on equidistant ratio scales
1/1.5

1/1.1
irr_ur_list=c((1/1.5),(1/1.25),1,1.25,1.5)
#Run function lots of times, and stack the results on top of one another
un_pop_deaths_2019_irr_ur=irr_ur_list %>% 
  map_dfr(estimate_urban_ir)

table(un_pop_deaths_2019_irr_ur$irr_ur)
un_pop_deaths_2019_irr_ur
names(un_pop_deaths_2019_irr_ur)
#Now compare
un_pop_deaths_2019_irr_ur %>% 
  group_by(region_wb_dl) %>% 
  summarise(
    n_deaths_urban_gbd_un_pop_pt=sum(n_deaths_urban_gbd_un_pop_pt,na.rm=TRUE),
    n_deaths_urban_gbd_un_pop_ll=sum(n_deaths_urban_gbd_un_pop_ll,na.rm=TRUE),
    n_deaths_urban_gbd_un_pop_ul=sum(n_deaths_urban_gbd_un_pop_ul,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(desc(n_deaths_urban_gbd_un_pop_pt))
  
