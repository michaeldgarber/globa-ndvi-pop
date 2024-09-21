#Read United Nations population data

#January 23, 2023
#Note that I also need the mortality data. Following methods noted in David's 
#manuscript with Chinese colleagues, 
#I'm using the "deaths by select age groups" dataset under the "mortality" tab
#https://population.un.org/wpp/Download/Standard/Mortality/

#Note I had to delete the heading and modify it slightly so that it would load easier.
#I just care about the 20+ data, so I removed the other columns.
#Note reported deaths are in the thousands.

#May 15th, 2023
#I'm here again for the WHO urban health project. For that project, 
#I need to do an empirical analysis comparing GBD deaths and proportion urban.
#Given the proportion urban comes from UN, I will be able to use many of this 
#country-level work.

#As a result, I'm renaming the script name from
#read-united-nations-deaths.R
#to
#read-united-nations-gbd-data

#Sep 14, 2023
#Updating script to include life expectancy as well

#Revised Jan 10, 2024 to include WHO mortality data


library(here)
library(tidyverse)
library(readxl)

# Load deaths data by country------
#May 15, 2023: call n_deaths n_deaths - easier
#Dec 5, 2023: I'm adding a "un" for united nations to keep track
setwd(here("data-input", "united-nations-mortality-data"))
un_deaths_2019 = read_excel("un-deaths-age-groups-both-sexes-modified.xlsx") %>% 
  rename(location_code_un=`Location code`) %>% 
  rename(iso3_alpha_code=`ISO3 Alpha-code`) %>% 
  rename_with(tolower) %>% #May 15th, 2023 - this is amazing. all lowercase
  filter(type=="Country/Area") %>% 
  #Dec 5: removing the "both_sexes" for simplicity
  mutate(n_deaths_ac_un_20_plus = deaths_thousands_20_plus_both_sexes_num*1000) %>% 
  filter(year==2019) %>% 
  mutate(row_id = row_number()) %>% 
  dplyr::select(-contains("_char"),-contains("both_sexes"))  %>% #remove _char suffixes
  #the iso3 and iso2 codes don't seem to work here, so remove them.
  #also remove notes. and remove the word variant, as I never use it
  dplyr::select(-contains("iso3"),-contains("iso2"),-contains("notes"),
                -contains("sdmx"),-contains("variant"),
                #don't need type anymore either
                -contains("type"))
  

names(un_deaths_2019)
un_deaths_2019
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
#Dec 5, 2023: avoiding the "both sexes thing
setwd(here("data-input", "united-nations-mortality-data"))
un_pop_2019 = read_excel("un-pop-age-groups-both-sexes-modified.xlsx") %>% 
  #Update Jan 16, 2023: 
  #calling this location_code_un to avoid confusion with location_id_gbd
  rename(location_code_un=`Location code`) %>% #fix this
  rename_with(tolower) %>% #May 15th, 2023 - this is amazing. all lowercase
  filter(type=="Country/Area") %>% 
  #added Dec 5, 2023
  rename(pop_20_plus_thousands_num=pop_20_plus_both_sexes_thousands_num) %>% 
  mutate(
    #Dec 5, 2023: we don't need "absolute". let's call it _un, though,
    #to keept rack of source
    pop_20_plus_un = pop_20_plus_thousands_num*1000,
    pop_total_un = pop_total_thousands_num*1000,
    #it's more of a proportion than a ratio. calling it that now
    pop_prop_20_plus=pop_20_plus_un/pop_total_un
    ) %>% 
  dplyr::select(-contains("_char"),#remove _char suffixes
                -contains("sdmx"),#remove this too
                -contains("notes"),
                -contains("thousands")#I never use these
                ) %>% 
  filter(year==2019) %>% 
  mutate(row_id = row_number())

#table(un_deaths_2019$type)
un_deaths_2019 %>% 
  n_distinct("un_pop_2019")

nrow(un_pop_2019)
#un_pop_2019 %>% View()
names(un_pop_2019)
un_pop_2019
table(un_pop_2019$region_subregion_country_or_area)

#explore ratio of 20 plus
summary(un_pop_2019$pop_prop_20_plus)

setwd(here("data-processed"))
save(un_pop_2019, file = "un_pop_2019.RData")

#Try another one Jan 16, 2024
#with data by 5-yr age groups
setwd(here("data-input", "united-nations-mortality-data"))
un_pop_2019_alt = read_excel("un-pop-by-5-yr-group-modified.xlsx") 

un_pop_2019_alt

# Load proportion urban by country-------
#This is for the WHO urban health paper, but it won't hurt to have it in this same pipeline.
setwd(here("data-input","united-nations-urban"))
un_prop_urban = read_excel("WUP2018-F21-Proportion_Urban_Annual-modified.xlsx") %>% 
  #Call the Country code location code as it is above
  rename(location_code_un=`Country code`) %>% 
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
nrow(un_prop_urban)

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
    location_code_un=`location code`,
    iso3_alpha_code=`iso3 alpha-code`,
    iso2_alpha_code=`iso2 alpha-code`,
         ) %>% 
  distinct()

lookup_un_country_code_three_letter_code %>% 
  arrange(iso2_alpha_code)

names(lookup_un_country_code_three_letter_code)
nrow(lookup_un_country_code_three_letter_code)

names(cities_geonames)

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
table(wb_class$region_wb_dl)

#Will this link with the pop data?
names(un_deaths_2019)
class(un_deaths_2019$location_code_un)
class(lookup_un_country_code_three_letter_code$location_code_un)
un_pop_deaths_2019_w_wb=un_deaths_2019 %>%
  dplyr::select(-contains("iso3")) %>% 
  dplyr::select(-contains("iso2")) %>% 
  dplyr::select(-contains("notes")) %>% 
  left_join(lookup_un_country_code_three_letter_code,by="location_code_un") %>% 
  left_join(wb_class, by ="iso3_alpha_code")

names(un_pop_deaths_2019_w_wb)

un_pop_deaths_2019_w_wb %>% 
#  filter(type == "Country/Area") %>% 
  dplyr::select(starts_with("location"),contains("code"),
                contains("wb"),contains("country_name")) %>% 
  print(n=300)

# Load and wrangle WHO 2019 mortality data------
#This data was emailed to me by Pier but presumably could be obtained from the WHO
#data portal here
#https://platform.who.int/mortality/themes/theme-details/MDB/all-causes

## Load all-cause mortality data from WHO------
setwd(here("data-input", "who-mortality-data"))
who_deaths_by_country_2019 = read_excel("who-2019-mortality-by-country.xlsx") %>% 
  #elsewhere in this pipeline, I use this iso3_alpha_code
  rename(iso3_alpha_code=iso3) %>% 
  #we can restrict to sex==b for both
  filter(sex=="b") %>% 
  #Don't need  year or cause (all)
  dplyr::select(-contains("year")) %>% 
  dplyr::select(-contains("cause")) %>% 
  mutate(
    #Jan 18, 2024:
    #To make it easier to copy and paste and find and replace, 
    #I'm going to add a subscript called "_ac" for all-cause, 
    #which I'll then be able to replace with "na" for non-accidental.
    #calculate mortality rate
    death_rate_ac_who_pt=dths/pop,
    #we can make age numeric so it's easy to filter.
    #This will lose the "all". that's okay.
    age_char=age,
    age=as.numeric(age_char)#keep age_char for calcs below Jan 18, 2024
  ) %>% 
  #Don't need sex, as we know it's both, and don't need name
  #also don't need region and income information perhaps use later, 
  #so can create a lookup below
  dplyr::select(
    -contains("sex"),-contains("whoname"),
    -contains("whoreg"),-contains("wbinc")
                )

#how many countries?
n_distinct(who_deaths_by_country_2019$iso3_alpha_code)
table(who_deaths_by_country_2019$age)
table(who_deaths_by_country_2019$iso3_alpha_code)
who_deaths_by_country_2019

#I need this below
pop_by_age_country=who_deaths_by_country_2019 %>% 
  filter(is.na(age)==F) %>% 
  dplyr::select(contains("iso"),age,pop)

pop_by_age_country
#how many age categories? 5-years to 85+
who_deaths_by_country_2019 %>% 
  group_by(age) %>% 
  summarise(n=n())
who_deaths_by_country_2019 %>% print(n=100)
#what's the total population in this dataset?
who_deaths_by_country_2019 %>% 
  filter(age=="all") %>% 
  mutate(dummy=1) %>% 
  summarise(pop=sum(pop,na.rm=T))

who_deaths_by_country_2019 %>% 
  filter(age=="all") %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(n_deaths_ac=sum(dths))

#in case this is useful, a lookup to wb income by alpha code
setwd(here("data-input", "who-mortality-data"))
lookup_iso3_alpha_code_wbinc19=read_excel("who-2019-mortality-by-country.xlsx") %>% 
  #elsewhere in this pipeline, I use this iso3_alpha_code
  rename(iso3_alpha_code=iso3) %>% 
  distinct(iso3_alpha_code,wbinc19) %>% 
  as_tibble()

lookup_iso3_alpha_code_wbinc19

## Load data on unintentional injuries-------
#Doing this Jan 18, 2024
#see here
#https://platform.who.int/mortality/themes/theme-details/topics/topic-details/MDB/unintentional-injuries
setwd(here("data-input", "who-mortality-data","who-deaths-injuries"))
#ui for unintentional injury
who_deaths_ui_by_country_2019 = read_excel("who-deaths-unintentional-inj-sex-age-country-year.xlsx") %>% 
  filter(Year==2019) %>% 
  filter(Sex=="All") %>%  #limit to all sexes %>% 
  #rename country code to iso3_alpha_code
  #so it can be linked
  rename(iso3_alpha_code=`Country Code`) %>% 
  #make everything lowercase
  rename_with(tolower) %>% 
  #don't need country name or region code
  dplyr::select(-contains("region"),-contains("country name")) %>% 
  #rename this. ui for unintentional injury
  rename(
    n_deaths_ui_who_pt=number,
    #it would be good to keep this one too
    #proportion of total deaths that are unintentional injury
    perc_ui_n_deaths_ac_who_pt=`percentage of cause-specific deaths out of total deaths`
    ) %>% 
  #reformat the age to link with above
  #This works
  #https://stringr.tidyverse.org/articles/stringr.html#getting-and-setting-individual-characters
  mutate(age=
           as.numeric(
           str_sub(`age group code`,4,5)),
         prop_ui_n_deaths_ac_who_pt=perc_ui_n_deaths_ac_who_pt/100
  ) %>% 
  #now limit to age 30+
  filter(age>=30) %>% 
  #and just keep the data that we need for the link
  dplyr::select(
    contains("iso"),
    age,
    contains("n_death"),
    contains("prop")
  )


who_deaths_ui_by_country_2019

prop_ui_n_deaths_ac_who_pt_mean_by_age=who_deaths_ui_by_country_2019 %>% 
  group_by(age) %>% 
  summarise(
    prop_ui_n_deaths_ac_who_pt_mean_over_country=mean(prop_ui_n_deaths_ac_who_pt,na.rm=T),
    prop_ui_n_deaths_ac_who_pt_sd_over_country=sd(prop_ui_n_deaths_ac_who_pt,na.rm=T)
  ) %>% 
  ungroup()

prop_ui_n_deaths_ac_who_pt_mean_by_age

# table(who_deaths_ui_by_country_2019$`age group code`)
# names(who_deaths_ui_by_country_2019)
# who_deaths_ui_by_country_2019 %>% 
#   group_by(`age group code`,age) %>% 
#   summarise(n=n()) %>% 
#   print(n=100)

## Load data on all injuries--------
# Per Crouse et al, the study of Canadian cities, ICD-10 codes included in non-accidental are
#A through R, which does exclude suicide and other violent events, so basically all
#injuries should be excluded. This will take our overall number of deaths prevented
#down by quite a bit.
#inj for injury
#June 24, 2024:
#Here's what I'm going to do. For each country, take the latest available data.
#Because it's the proportion of the total, not the grand total, it might be okay.
setwd(here("data-input", "who-mortality-data","who-deaths-injuries"))
#It used to be called who_deaths_inj_by_country_2019. Let's call it instead
#who_deaths_inj_by_country_latest
who_deaths_inj_by_country_latest = read_excel("who-deaths-all-inj-sex-age-country-year.xlsx") %>% 
#  filter(Year==2019) %>% 
  filter(Sex=="All") %>%  #limit to all sexes %>% 
  #rename country code to iso3_alpha_code
  #so it can be linked
  rename(iso3_alpha_code=`Country Code`) %>% 
  #make everything lowercase
  rename_with(tolower) %>% 
  #rename this. ui for unintentional injury
  rename(
    n_deaths_inj_who_pt=number,
    #it would be good to keep this one too
    #proportion of total deaths that are unintentional injury
    perc_inj_n_deaths_ac_who_pt=`percentage of cause-specific deaths out of total deaths`,
    year_injury_data=year,
  ) %>% 
  #reformat the age to link with above
  #This works
  #https://stringr.tidyverse.org/articles/stringr.html#getting-and-setting-individual-characters
  mutate(age=
           as.numeric(
             str_sub(`age group code`,4,5)),
         prop_inj_n_deaths_ac_who_pt=perc_inj_n_deaths_ac_who_pt/100
  ) %>% 
  #now limit to age 30+
  filter(age>=30) %>% 
  #now take the latest year within country age group
  group_by(iso3_alpha_code,age) %>% 
  arrange(desc(year_injury_data)) %>% 
  slice(1:5) %>% #top 5 most recent years...
  #and just keep the data that we need for the link
  #don't need country name or region code
  dplyr::select(-contains("region"),-contains("country name")) %>% 
  dplyr::select(
    contains("iso"),
    contains("year"),
    age,
    contains("n_death"),
    contains("prop")
  ) %>% 
  #okay, now I can remove anything newer than 2020, as there are no countries with 2020 that do
  #not also have 2019
  ungroup() %>% 
  filter(year_injury_data<2020) %>% 
  filter(year_injury_data>2009) %>% 
  #and now take each country's most recent year
  group_by(iso3_alpha_code,age) %>% 
  arrange(desc(year_injury_data)) %>% 
  slice(1) %>% 
  ungroup()


names(who_deaths_inj_by_country_latest)
#who_deaths_inj_by_country_latest %>% View()
max(who_deaths_inj_by_country_latest$year_injury_data)
#Either 2019 or the latest for that country
who_deaths_inj_by_country_latest %>% 
  group_by(iso3_alpha_code) %>% 
  summarise(year_max=max(year_injury_data)) %>% 
  nrow()

(183-116)/183
who_deaths_inj_by_country_latest %>% 
  group_by(iso3_alpha_code) %>% 
  summarise(year_max=max(year_injury_data)) %>% 
  arrange(year_max) %>% 
  print(n=200)
  
who_deaths_inj_by_country_latest %>% 
  group_by(iso3_alpha_code) %>% 
  summarise(year_max=max(year_injury_data)) %>% 
  ggplot(aes(x=year_max))+
  geom_histogram()

65/183
#how many have 2019 data?
who_deaths_inj_by_country_latest %>% 
  filter(year_injury_data==2019) %>% 
  group_by(iso3_alpha_code) %>% 
  summarise(n=n()) %>% 
  nrow()


    

sum(pop_by_age_country$pop)
#now how many unique countries does that give us?
#we're up to 118
n_distinct(who_deaths_inj_by_country_latest$iso3_alpha_code)

#Let's get a histogram of the proportion due to injury
who_deaths_inj_by_country_latest %>% 
  ggplot(aes(x=prop_inj_n_deaths_ac_who_pt))+
  geom_histogram()+
  facet_grid(rows="age")

### proportion injury related by age group-----
prop_inj_n_deaths_ac_who_pt_mean_by_age=who_deaths_inj_by_country_latest %>%
  left_join(pop_by_age_country,by=c("iso3_alpha_code","age")) %>% 
  group_by(age) %>% 
  #this should be a weighted mean. can weight by the number of deaths
  #as it's proportionally equivalent to pop here
  summarise(
    prop_inj_n_deaths_ac_who_pt_mean_over_country=mean(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_mean_wt_over_country=weighted.mean(
      x=prop_inj_n_deaths_ac_who_pt,
      w=n_deaths_inj_who_pt,
      na.rm=T),
    prop_inj_n_deaths_ac_who_pt_med_over_country=median(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_sd_over_country=sd(prop_inj_n_deaths_ac_who_pt,na.rm=T)
  ) %>% 
  ungroup()

#maybe unweighted makes sense for the purposes of imputation?
#That's more likely to be the value of a given country, I suppose,
#or even the median could work
prop_inj_n_deaths_ac_who_pt_mean_by_age

#compare that with
prop_ui_n_deaths_ac_who_pt_mean_by_age

#alright mean and med are about the same. mean works.
#Link the two to calculate non-accidental (NA mortality)
78/183
n_countries_with_all_cause_mortality_2019=n_distinct(who_deaths_by_country_2019$iso3_alpha_code)
n_countries_with_all_cause_mortality_2019-78
n_distinct(who_deaths_inj_by_country_latest$iso3_alpha_code)

n_distinct(pop_by_age_country$iso3_alpha_code)

### proportion injury related by age group and world bank class-----
#June 24, 2024
#Reviewers commented that the proportion of missing data was high for unintentional injury, which is true.
#and that the assumption to use the age-group-specific average is perhaps not appropriate.
#Let's see how well we can stratify. Do we have representation here in every income class?
table(wb_class$income_grp_wb_fac)
prop_inj_n_deaths_ac_who_pt_mean_by_age_wb_class=who_deaths_inj_by_country_latest %>%
  left_join(pop_by_age_country,by=c("iso3_alpha_code","age")) %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(age,income_grp_wb_fac) %>% 
  summarise(
    n=n(),
    prop_inj_n_deaths_ac_who_pt_mean_over_country=mean(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_mean_wt_over_country=weighted.mean(
      x=prop_inj_n_deaths_ac_who_pt,
      w=n_deaths_inj_who_pt,
      na.rm=T),
    prop_inj_n_deaths_ac_who_pt_med_over_country=median(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_sd_over_country=sd(prop_inj_n_deaths_ac_who_pt,na.rm=T)
  ) %>% 
  ungroup() 


#June 24, 2024: we now have representation in every country, but only 1
#low-income country. Which one is it? It's Syria.
#I don't think we can reliably say that Syria has
prop_inj_n_deaths_ac_who_pt_mean_by_age_wb_class %>% View()
who_deaths_inj_by_country_latest %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  filter(income_grp_wb_fac=="Low income")
  

#what about just by wb class?
prop_inj_n_deaths_ac_who_pt_mean_by_wb_class=who_deaths_inj_by_country_latest %>%
  left_join(pop_by_age_country,by=c("iso3_alpha_code","age")) %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(income_grp_wb_fac) %>% 
  summarise(
    prop_inj_n_deaths_ac_who_pt_mean_over_country=mean(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_mean_wt_over_country=weighted.mean(
      x=prop_inj_n_deaths_ac_who_pt,
      w=n_deaths_inj_who_pt,
      na.rm=T),
    prop_inj_n_deaths_ac_who_pt_med_over_country=median(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_sd_over_country=sd(prop_inj_n_deaths_ac_who_pt,na.rm=T)
  ) %>% 
  ungroup() 

prop_inj_n_deaths_ac_who_pt_mean_by_wb_class %>% View()

### what about by region?
prop_inj_n_deaths_ac_who_pt_mean_by_age_region_wb_dl=who_deaths_inj_by_country_latest %>%
  left_join(pop_by_age_country,by=c("iso3_alpha_code","age")) %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(age,region_wb_dl) %>% 
  summarise(
    n_countries=n(),
    prop_inj_n_deaths_ac_who_pt_mean_over_country=mean(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_mean_wt_over_country=weighted.mean(
      x=prop_inj_n_deaths_ac_who_pt,
      w=n_deaths_inj_who_pt,
      na.rm=T),
    prop_inj_n_deaths_ac_who_pt_med_over_country=median(prop_inj_n_deaths_ac_who_pt,na.rm=T),
    prop_inj_n_deaths_ac_who_pt_sd_over_country=sd(prop_inj_n_deaths_ac_who_pt,na.rm=T)
  ) %>% 
  ungroup()

n_distinct(prop_inj_n_deaths_ac_who_pt_mean_by_age_region_wb_dl$region_wb_dl)
prop_inj_n_deaths_ac_who_pt_mean_by_age_region_wb_dl %>% View()




## Fit model to impute missing injury proportion-------
### Try 1: overall model---------
#should we try a model for the imputed data?
names(who_deaths_inj_by_country_latest)
who_deaths_inj_by_country_latest %>% 
  ggplot(aes(x=prop_inj_n_deaths_ac_who_pt))+
  geom_histogram()+
  facet_grid(rows="age")+
  xlab("Proportion of deaths due to injury")+
  theme_bw(base_size = 12)

#save this for the supplemental material
setwd(here("plots"))
ggsave("plot_prop_deaths_inj_by_age.png", 
       height=5, width=5)

who_deaths_inj_by_country_latest_for_model=who_deaths_inj_by_country_latest %>% 
  left_join(wb_class,by="iso3_alpha_code") 
  
table(who_deaths_inj_by_country_latest_for_model$region_wb_dl)
table(who_deaths_inj_by_country_latest_for_model$income_grp_wb)
glm_prop_inj=glm(prop_inj_n_deaths_ac_who_pt~age+region_wb_dl+income_grp_wb,
                 family=gaussian,
                 na.action="na.exclude",
                 data=who_deaths_inj_by_country_latest_for_model)

glm_prop_inj

#Impute
who_deaths_inj_by_country_to_impute=who_deaths_by_country_2019 %>% 
  filter(age>=30) %>% #note this will exclude "all" as well
  left_join(who_deaths_inj_by_country_latest,by=c("iso3_alpha_code","age")) %>% 
  filter(is.na(prop_inj_n_deaths_ac_who_pt)==T) %>%
  left_join(wb_class,by="iso3_alpha_code") %>% 
  dplyr::select(contains("iso3"),contains("age"),contains("region_wb_dl"),contains("income_grp_wb"))



#there are some negatives. I should fit the model in each age group.
who_deaths_inj_by_country_imputed=as_tibble(
  stats::predict(glm_prop_inj,
                 who_deaths_inj_by_country_to_impute,
                 type="response",
                 na.action = "na.exclude" ,
                 se.fit = TRUE)
)

who_deaths_inj_by_country_imputed %>% View()
who_deaths_inj_by_country_to_impute %>% View()

### Fit model in each age group------
#Let's fit the model in each age group. It will be less susceptible to bias I think
table(who_deaths_inj_by_country_latest$age)

who_deaths_inj_by_country_latest_for_model_30=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==30)
who_deaths_inj_by_country_latest_for_model_35=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==35)
who_deaths_inj_by_country_latest_for_model_40=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==40)
who_deaths_inj_by_country_latest_for_model_45=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==45)
who_deaths_inj_by_country_latest_for_model_50=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==50)
who_deaths_inj_by_country_latest_for_model_55=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==55)
who_deaths_inj_by_country_latest_for_model_60=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==60)
who_deaths_inj_by_country_latest_for_model_65=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==65)
who_deaths_inj_by_country_latest_for_model_70=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==70)
who_deaths_inj_by_country_latest_for_model_75=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==75)
who_deaths_inj_by_country_latest_for_model_80=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==80)
who_deaths_inj_by_country_latest_for_model_85=who_deaths_inj_by_country_latest_for_model %>% 
  filter(age==85)


# Fit models
#This is nice because you get the global average and then it bumps around a little based
#on the country's demographics
glm_prop_inj_30=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_30)

glm_prop_inj_30

glm_prop_inj_35=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_35)

glm_prop_inj_35

glm_prop_inj_40=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_40)

glm_prop_inj_40

glm_prop_inj_45=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_45)

glm_prop_inj_45

glm_prop_inj_50=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_50)

glm_prop_inj_50

glm_prop_inj_55=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_55)

glm_prop_inj_55

glm_prop_inj_60=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_60)

glm_prop_inj_60

glm_prop_inj_65=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_65)

glm_prop_inj_65

glm_prop_inj_70=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_70)

glm_prop_inj_70

glm_prop_inj_75=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_75)

glm_prop_inj_75

glm_prop_inj_80=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_80)

glm_prop_inj_80

glm_prop_inj_85=glm(prop_inj_n_deaths_ac_who_pt~region_wb_dl+income_grp_wb,
                    family=gaussian,
                    na.action="na.exclude",
                    data=who_deaths_inj_by_country_latest_for_model_85)

glm_prop_inj_85


#### Predict values in each age group--------
#Update July 1, 2024
#I can leave in the true values so that I can assess residuals where the data are known
who_deaths_inj_by_country_to_impute_age_strat=who_deaths_by_country_2019 %>% 
  left_join(who_deaths_inj_by_country_latest,by=c("iso3_alpha_code","age")) %>% 
  #  filter(is.na(prop_inj_n_deaths_ac_who_pt)==T) %>%
  left_join(wb_class,by="iso3_alpha_code") %>% 
  dplyr::select(contains("iso3"),contains("age"),contains("region_wb_dl"),contains("income_grp_wb"),
                contains("prop_inj_n_deaths_ac_who_pt"))

##### 30-------
who_deaths_inj_by_country_to_impute_30=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==30) 

who_deaths_inj_by_country_imputed_30=as_tibble(
  stats::predict(glm_prop_inj_30,
                 who_deaths_inj_by_country_to_impute_30,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_30)
  
who_deaths_inj_by_country_imputed_30 %>% View()

who_deaths_inj_by_country_imputed_30

##### 35-------
who_deaths_inj_by_country_to_impute_35=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==35) 

who_deaths_inj_by_country_to_impute_35

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_35=as_tibble(
  stats::predict(glm_prop_inj_35,
                 who_deaths_inj_by_country_to_impute_35,
                 #                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_35)


who_deaths_inj_by_country_imputed_35 

##### 40-------
who_deaths_inj_by_country_to_impute_40=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==40) 

who_deaths_inj_by_country_to_impute_40

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_40=as_tibble(
  stats::predict(glm_prop_inj_40,
                 who_deaths_inj_by_country_to_impute_40,
                 type="response",
          #       na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_40)


who_deaths_inj_by_country_imputed_40 


##### 45-------
who_deaths_inj_by_country_to_impute_45=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==45) 

who_deaths_inj_by_country_to_impute_45

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_45=as_tibble(
  stats::predict(glm_prop_inj_45,
                 who_deaths_inj_by_country_to_impute_45,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_45)


who_deaths_inj_by_country_imputed_45

##### 50-------
who_deaths_inj_by_country_to_impute_50=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==50) 

who_deaths_inj_by_country_to_impute_50

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_50=as_tibble(
  stats::predict(glm_prop_inj_50,
                 who_deaths_inj_by_country_to_impute_50,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_50)


who_deaths_inj_by_country_imputed_50


##### 55-------
who_deaths_inj_by_country_to_impute_55=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==55)  

who_deaths_inj_by_country_to_impute_55

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_55=as_tibble(
  stats::predict(glm_prop_inj_55,
                 who_deaths_inj_by_country_to_impute_55,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_55)


who_deaths_inj_by_country_imputed_55


##### 60-------
who_deaths_inj_by_country_to_impute_60=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==60) 

who_deaths_inj_by_country_to_impute_60

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_60=as_tibble(
  stats::predict(glm_prop_inj_60,
                 who_deaths_inj_by_country_to_impute_60,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_60)


who_deaths_inj_by_country_imputed_60


##### 65-------
who_deaths_inj_by_country_to_impute_65=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==65) 

who_deaths_inj_by_country_to_impute_65

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_65=as_tibble(
  stats::predict(glm_prop_inj_65,
                 who_deaths_inj_by_country_to_impute_65,
                 type="response",
        #         na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_65)


who_deaths_inj_by_country_imputed_65

##### 70-------
who_deaths_inj_by_country_to_impute_70=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==70) 

who_deaths_inj_by_country_to_impute_70

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_70=as_tibble(
  stats::predict(glm_prop_inj_70,
                 who_deaths_inj_by_country_to_impute_70,
                 type="response",
      #           na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_70)


who_deaths_inj_by_country_imputed_70

##### 75-------
who_deaths_inj_by_country_to_impute_75=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==75) 

who_deaths_inj_by_country_to_impute_75

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_75=as_tibble(
  stats::predict(glm_prop_inj_75,
                 who_deaths_inj_by_country_to_impute_75,
                 type="response",
               #  na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_75)


who_deaths_inj_by_country_imputed_75

##### 80-------
who_deaths_inj_by_country_to_impute_80=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==80) 

who_deaths_inj_by_country_to_impute_80

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_80=as_tibble(
  stats::predict(glm_prop_inj_80,
                 who_deaths_inj_by_country_to_impute_80,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_80)


who_deaths_inj_by_country_imputed_80

##### 85-------
who_deaths_inj_by_country_to_impute_85=who_deaths_inj_by_country_to_impute_age_strat %>% 
  filter(age==85) 

who_deaths_inj_by_country_to_impute_85 

#there are some negatives. I should fit the model in each age group.
#prop_inj_n_deaths_ac_who_pt
who_deaths_inj_by_country_imputed_85=as_tibble(
  stats::predict(glm_prop_inj_85,
                 who_deaths_inj_by_country_to_impute_85,
                 type="response",
#                 na.action = "na.exclude" ,
                 se.fit = TRUE)
) %>% 
  #If it's available, I call it this, so we can call it
  #"imp_pt" for imputed and imp_se for standard error of the fit
  rename(
    prop_inj_n_deaths_ac_who_imp_pt=fit,
    prop_inj_n_deaths_ac_who_imp_se=se.fit
  ) %>% 
  bind_cols(who_deaths_inj_by_country_to_impute_85)


who_deaths_inj_by_country_imputed_85

### Build the dataset of predicted values back together------
#and link with those where imputation was not necessary

who_deaths_inj_by_country_imputed_all_ages=who_deaths_inj_by_country_imputed_30 %>% 
  bind_rows(
    who_deaths_inj_by_country_imputed_35,
    who_deaths_inj_by_country_imputed_40,
    who_deaths_inj_by_country_imputed_45,
    who_deaths_inj_by_country_imputed_50,
    who_deaths_inj_by_country_imputed_55,
    who_deaths_inj_by_country_imputed_60,
    who_deaths_inj_by_country_imputed_65,
    who_deaths_inj_by_country_imputed_70,
    who_deaths_inj_by_country_imputed_75,
    who_deaths_inj_by_country_imputed_80,
    who_deaths_inj_by_country_imputed_85
  ) %>% 
  #can remove the residual.scale. It's .116 %>% 
  dplyr::select(-contains("residual.scale")) %>% 
  mutate(
    #calculate a residual as truth minus predicted
    #where it's available
    prop_inj_n_deaths_ac_who_resid=prop_inj_n_deaths_ac_who_pt-prop_inj_n_deaths_ac_who_imp_pt
  )

who_deaths_inj_by_country_imputed_all_ages
names(who_deaths_inj_by_country_imputed_all_ages)
#summarize residuals
summary(who_deaths_inj_by_country_imputed_all_ages$prop_inj_n_deaths_ac_who_resid)
who_deaths_inj_by_country_imputed_all_ages %>% 
  filter(is.na(prop_inj_n_deaths_ac_who_resid)==F) 
who_deaths_inj_by_country_imputed_all_ages %>% 
  ggplot(aes(x=prop_inj_n_deaths_ac_who_resid))+
  geom_histogram()+
  theme_bw(base_size = 14)+
  xlab("Residual:\nactual proportion minus predicted proportion")

#save this for supplement
setwd(here("plots"))
ggsave("plot_hist_model_residual_all_cause.png", 
       height=5, width=5)


who_deaths_inj_by_country_imputed_all_ages %>% 
  ggplot(aes(x=prop_inj_n_deaths_ac_who_resid))+
  geom_histogram()+
  facet_grid(rows="age_char")


who_deaths_inj_by_country_imputed_all_ages %>% 
  ggplot(aes(x=prop_inj_n_deaths_ac_who_resid))+
  geom_histogram()+
  facet_grid(rows="income_grp_wb")



who_deaths_inj_by_country_imputed_all_ages %>% 
  group_by(age_char) %>% 
  summarise(
    prop_inj_n_deaths_ac_who_resid_m=mean(prop_inj_n_deaths_ac_who_resid,na.rm=T),
    prop_inj_n_deaths_ac_who_resid_med=median(prop_inj_n_deaths_ac_who_resid,na.rm=T))

who_deaths_inj_by_country_imputed_all_ages %>% 
  group_by(income_grp_wb) %>% 
  summarise(
    prop_inj_n_deaths_ac_who_resid_m=mean(prop_inj_n_deaths_ac_who_resid,na.rm=T),
    prop_inj_n_deaths_ac_who_resid_med=median(prop_inj_n_deaths_ac_who_resid,na.rm=T))

who_deaths_inj_by_country_imputed_all_ages %>% 
  filter(income_grp_wb=="Low income") %>% 
  filter(is.na(prop_inj_n_deaths_ac_who_resid)==F) %>% 
  dplyr::select(contains("prop_inj"),everything())

#omit wb and age stuff before the join as well as the actual value
names(who_deaths_inj_by_country_imputed_all_ages)
who_deaths_inj_by_country_imputed_all_ages_for_join=who_deaths_inj_by_country_imputed_all_ages %>% 
  dplyr::select(-contains("prop_inj_n_deaths_ac_who_pt")) %>% 
  dplyr::select(-contains("wb"))
  
names(who_deaths_inj_by_country_imputed_all_ages_for_join)

# Link WHO data and age-standardize the mortality rates---------

#Jan 16, 2024: updating to use 30+ rather than 20+
#Limit to 20+ and do some more wrangling
#Comment: to be more conservative, we might consider restricting to 30+
#June 26, 2024 updated to include the updated imputed proportion injury data

#Now I can left join this with the rest of the data
#The dataset with the non-missing data
who_deaths_by_country_2019_30_plus =who_deaths_by_country_2019 %>% 
  dplyr::select(-contains("age_char")) %>% 
  filter(age>=30) %>% #note this will exclude "all" as well
  #total over all age groups to get the proportion in each age group
  group_by(iso3_alpha_code) %>% 
  #note these proportions are among adults 30+
  mutate(pop_all_ages=sum(pop,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(age_prop=pop/pop_all_ages) %>% 
  left_join(who_deaths_ui_by_country_2019,by=c("iso3_alpha_code","age")) %>% #Don't think I'm using this
  left_join(who_deaths_inj_by_country_latest,by=c("iso3_alpha_code","age")) %>% 
  left_join(who_deaths_inj_by_country_imputed_all_ages_for_join,by=c("iso3_alpha_code","age")) %>% 
  mutate(
    #indicator variables for missingness
    miss_n_deaths_ui=case_when(
    is.na(n_deaths_ui_who_pt)==T~1,
    TRUE ~0
  ),
  miss_n_deaths_inj=case_when(
    is.na(n_deaths_inj_who_pt)==T~1,
    TRUE ~0
  ),
  #what about proportion rather number?
  #This can be used as a weight on the overall death rate
  miss_prop_inj_n_deaths_ac_who_pt=case_when(
    is.na(prop_inj_n_deaths_ac_who_pt)==T~1,
    TRUE~0
  )) %>% 
  #June 26, 2024: here we're imputing the age-group-specific value
  #using the model above instead of just taking the global mean as before.
  #This fills in this column with the imputed value if it's missing.
  #I have a flag if I need to keep track of which one is imputed
  
  #July 2, 2024: I'm going to consider uncertainty in the imputed values by resampling
  #the predicted values according to their standard errors
  mutate(
    prop_inj_n_deaths_ac_who_pt=case_when(
      miss_prop_inj_n_deaths_ac_who_pt==1~prop_inj_n_deaths_ac_who_imp_pt,
      TRUE ~prop_inj_n_deaths_ac_who_pt
          )
  ,
  #now, calculate the death rate for non-accidental as
  #(1-prop)*all-cause rate. call it na for non-accidental
  #Can ignore the unintentional injury
  death_rate_na_who_pt=death_rate_ac_who_pt*(1-prop_inj_n_deaths_ac_who_pt),
  
  #this needs to be re-calculated now so it works in sums below.
  #I have a flag that indicates when it's imputed
  #note here we're calculating non-accidental deaths, not injury deaths
  n_deaths_na_who_pt=dths*(1-prop_inj_n_deaths_ac_who_pt)
  )

names(who_deaths_by_country_2019_30_plus)
## Randomly vary the imputed values-------
#July 2, 2024:
#Here's what I'm going to do to consider uncertainty in the imputed values.
#I'm going to resample the predicted values and create confidence intervals here
#and then integrate those confidence intervals into the analysis-functions later.
#That way I won't have to resample the entire dataset downstream.
#Let's create a separate dataset that considers uncertainty and then link it int
random_vary_imputed_values=function(id_val){
  who_deaths_by_country_2019_30_plus_random=who_deaths_by_country_2019_30_plus %>% 
    dplyr::select(
      iso3_alpha_code,age,
      contains("prop_inj_n_deaths_ac_who_imp"),#picks up standard errors too
      contains("prop_inj_n_deaths_ac_who_pt"),
      contains("death_rate_ac_who_pt"),
      contains("dths")
    ) %>% 
    mutate(
      #randomly vary it here
      prop_inj_n_deaths_ac_who_pt_random=case_when(
        miss_prop_inj_n_deaths_ac_who_pt==1~rnorm(
          n=n(),
          mean=prop_inj_n_deaths_ac_who_imp_pt,
          sd=prop_inj_n_deaths_ac_who_imp_se
        ),
        TRUE ~prop_inj_n_deaths_ac_who_pt
      )
      ,
      #now, calculate the death rate for non-accidental as
      #(1-prop)*all-cause rate. call it na for non-accidental
      #Can ignore the unintentional injury
      death_rate_na_who_pt_random=death_rate_ac_who_pt*(1-prop_inj_n_deaths_ac_who_pt_random),
      
      #this needs to be re-calculated now so it works in sums below.
      #I have a flag that indicates when it's imputed
      #note here we're calculating non-accidental deaths, not injury deaths
      n_deaths_na_who_pt_random=dths*(1-death_rate_na_who_pt_random),
      imputation_id=id_val
    )
}
#Run it 1,000 times and calculate 95 CIs to integrate downstream
imputation_iterations= seq(from = 1, to = 1000, by = 1)
who_deaths_by_country_2019_30_plus_random_boot=imputation_iterations %>% 
  map_dfr(random_vary_imputed_values)

#calculate 2.5th and 97.5th percentiles for each age and country category
summary(who_deaths_by_country_2019_30_plus_random_boot$prop_inj_n_deaths_ac_who_pt_random)

who_deaths_by_country_2019_30_plus_random_summary=who_deaths_by_country_2019_30_plus_random_boot %>% 
  group_by(iso3_alpha_code,
           age) %>% 
  summarise(
    prop_inj_n_deaths_ac_who_ll=quantile(prop_inj_n_deaths_ac_who_pt_random,probs=.025,na.rm=T),
    prop_inj_n_deaths_ac_who_ul=quantile(prop_inj_n_deaths_ac_who_pt_random,probs=.975,na.rm=T),
    
    death_rate_na_who_ll=quantile(death_rate_na_who_pt_random,probs=.025,na.rm=T),
    death_rate_na_who_ul=quantile(death_rate_na_who_pt_random,probs=.975,na.rm=T),
    
    n_deaths_na_who_ll=quantile(n_deaths_na_who_pt_random,probs=.025,na.rm=T),
    n_deaths_na_who_ul=quantile(n_deaths_na_who_pt_random,probs=.975,na.rm=T)
  )

#The main thing we need here is the death_rate
who_deaths_by_country_2019_30_plus_random_summary
summary(who_deaths_by_country_2019_30_plus_random_summary$death_rate_na_who_ll)
summary(who_deaths_by_country_2019_30_plus_random_summary$death_rate_na_who_ul)




## Checks------
#how many countries?

n_distinct(who_deaths_by_country_2019_30_plus$iso3_alpha_code)
names(who_deaths_by_country_2019_30_plus)
who_deaths_by_country_2019_30_plus %>% 
  dplyr::select(contains("iso"),contains("age"),contains("death_rate")) %>% 
  print(n=1000)

who_deaths_by_country_2019_30_plus %>% 
  View()
summary(who_deaths_by_country_2019_30_plus$death_rate_na_who_pt)
who_deaths_by_country_2019_30_plus %>% 
  ggplot(aes(x=death_rate_na_who_pt))+
  geom_histogram()

who_deaths_by_country_2019_30_plus %>% 
  dplyr::select(contains("iso"),contains("age"),contains("death_rate")) %>% 
  group_by(age) %>% 
  summarise(death_rate_na_who_pt_mean=mean(death_rate_na_who_pt,na.rm=T))


#how many countries are missing this unintentional injury n, deaths?
who_deaths_by_country_2019_30_plus %>% 
  group_by(iso3_alpha_code,miss_n_deaths_ui) %>% 
  summarise(n=n()) %>% 
  print(n=100) %>% 
  group_by(miss_n_deaths_ui) %>% 
  summarise(n=n_distinct(iso3_alpha_code))

108/(76+108)
#wow, 108 countries are missing these data

#and all injury?
who_deaths_by_country_2019_30_plus %>% 
  group_by(iso3_alpha_code,miss_n_deaths_inj) %>% 
  summarise(n=n()) %>% 
  print(n=100) %>% 
  group_by(miss_n_deaths_inj) %>% 
  summarise(n=n_distinct(iso3_alpha_code))

#what about proportion? maybe fewer missing? nope, the same
who_deaths_by_country_2019_30_plus %>% 
  group_by(iso3_alpha_code,miss_prop_inj_n_deaths_ac_who_pt) %>% 
  summarise(n=n()) %>% 
  print(n=100) %>% 
  group_by(miss_prop_inj_n_deaths_ac_who_pt) %>% 
  summarise(n=n_distinct(iso3_alpha_code))


### Describe missingness patterns in non-accidental mortality----- 
table(who_deaths_inj_by_country_latest$miss_prop_inj_n_deaths_ac_who_pt)
#We have to begin with a 

miss_prop_inj_by_country=who_deaths_by_country_2019_30_plus %>% 
  group_by(iso3_alpha_code,miss_prop_inj_n_deaths_ac_who_pt) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  #remove seychelles, the one where some age cats are represented
  #but not all
  filter(iso3_alpha_code!="SYC") %>% 
  #and now collapse to only the countries
  group_by(iso3_alpha_code) %>% 
  summarise(miss_prop_inj_n_deaths_ac_who_pt=sum(miss_prop_inj_n_deaths_ac_who_pt))

#now classify these countries by their income status and region
wb_class %>% View()

table(miss_prop_inj_by_country$miss_prop_inj_n_deaths_ac_who_pt)


countries_missing_prop_inj_by_income_grp_wb_fac=miss_prop_inj_by_country %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(miss_prop_inj_n_deaths_ac_who_pt,income_grp_wb_fac) %>% 
  summarise(n_missing_prop_inj=n()) %>% 
  ungroup() %>% 
  filter(miss_prop_inj_n_deaths_ac_who_pt==1)

countries_missing_prop_inj_by_income_grp_wb_fac

miss_prop_inj_by_country %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(income_grp_wb_fac) %>% 
  summarise(n_countries_in_cat=n()) %>% 
  ungroup() %>% 
  left_join(countries_missing_prop_inj_by_income_grp_wb_fac) %>% 
  mutate(percent_of_cat_miss=n_missing_prop_inj/n_countries_in_cat)


#and do the same thing for region
wb_class

countries_missing_prop_inj_by_region_wb_dl=miss_prop_inj_by_country %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(miss_prop_inj_n_deaths_ac_who_pt,region_wb_dl) %>% 
  summarise(n_missing_prop_inj=n()) %>% 
  ungroup() %>% 
  filter(miss_prop_inj_n_deaths_ac_who_pt==1)

countries_missing_prop_inj_by_region_wb_dl

miss_prop_inj_by_country %>% 
  left_join(wb_class,by="iso3_alpha_code") %>% 
  group_by(region_wb_dl) %>% 
  summarise(n_countries_in_cat=n()) %>% 
  ungroup() %>% 
  left_join(countries_missing_prop_inj_by_region_wb_dl) %>% 
  mutate(percent_of_cat_miss=
           case_when(
             #north america has none missing so do this first
             is.na(n_missing_prop_inj)==T~0,
                     TRUE~ n_missing_prop_inj/n_countries_in_cat)
  )


  


n_distinct(miss_prop_inj_by_country$iso3_alpha_code)
miss_prop_inj_by_country %>% 
  print(n=200)

### Proportion age 30+--------
#Jan 16, 2024
#Doing it here rather than in the UN data, as I couldn't get that data to load
#also do it before the standardization so I can simply link this into that dataset
#for easier linking later
who_pop_all_ages_by_country=who_deaths_by_country_2019 %>% 
  filter(age_char=="all") %>% 
  dplyr::select(contains("iso"),contains("pop"))

who_pop_all_ages_by_country %>% arrange(desc(pop))
who_deaths_by_country_2019_30_plus %>% 
  group_by(age) %>% 
  summarise(n=n())
#begin with data that's limited to 30+
prop_pop_30_plus_who=who_deaths_by_country_2019_30_plus %>% 
  group_by(iso3_alpha_code) %>% 
  summarise(
    pop_30_plus_who_pt=sum(pop,na.rm=T)
  ) %>% 
  ungroup() %>% 
  left_join(who_pop_all_ages_by_country,by="iso3_alpha_code") %>% 
  mutate(pop_prop_30_plus_who_pt=pop_30_plus_who_pt/pop) %>% 
  #and just keep the proportion
  dplyr::select(contains("iso"),contains("prop"))

prop_pop_30_plus_who %>% arrange(desc(pop_prop_30_plus_who_pt))

summary(prop_pop_30_plus_who$pop_prop_30_plus_who_pt)

#and what's the overall age_prop across all countries?
#these are the weights for standardization
who_age_prop_all_countries= who_deaths_by_country_2019_30_plus %>% 
  group_by(age) %>% 
  summarise(pop_all_countries=sum(pop,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(
    pop_30_plus_all_countries=sum(pop_all_countries,na.rm=T),
    age_prop_all_countries=pop_all_countries/pop_30_plus_all_countries
    ) 


#the proportions are the weights for standardization
who_age_prop_all_countries
names(who_deaths_by_country_2019_30_plus)

## Do the standardization--------
#and summarize to the country level from the age-group-stratified level
#now do the standardization
names(who_deaths_by_country_2019_30_plus_random_summary)
who_deaths_by_country_2019_30_plus_std=who_deaths_by_country_2019_30_plus %>% 
  left_join(who_age_prop_all_countries,by="age") %>% 
  #and link in the confidence intervals
  left_join(who_deaths_by_country_2019_30_plus_random_summary,by=c("iso3_alpha_code","age")) %>% 
  #now calculate age-standardized rates.
  #Note, unlike for the GBD estimates, I don't have CIs, just the point estimates
  group_by(iso3_alpha_code) %>% 
  summarise(
    n_deaths_ac_who_pt=sum(dths,na.rm=T),
    
    #Add intervals here July 2, 2024
    n_deaths_na_who_pt=sum(n_deaths_na_who_pt,na.rm=T),
    n_deaths_na_who_ll=sum(n_deaths_na_who_ll,na.rm=T),
    n_deaths_na_who_ul=sum(n_deaths_na_who_ul,na.rm=T),
    
    pop_who_pt=sum(pop,na.rm=T),
    
    #age-standardize the all-cause death rate 
    death_rate_ac_who_30_plus_std_pt=weighted.mean(
      x=death_rate_ac_who_pt,
      w=age_prop_all_countries,
      na.rm=T
    ),
    
    #July 2, 2024: adding uncertainty here based on imputation model.
    #age-standardize the natural-cause death rate
    death_rate_na_who_30_plus_std_pt=weighted.mean(
      x=death_rate_na_who_pt,
      w=age_prop_all_countries,
      na.rm=T
    ),
    death_rate_na_who_30_plus_std_ll=weighted.mean(
      x=death_rate_na_who_ll,
      w=age_prop_all_countries,
      na.rm=T
    ),
    death_rate_na_who_30_plus_std_ul=weighted.mean(
      x=death_rate_na_who_ul,
      w=age_prop_all_countries,
      na.rm=T
    )
    
  ) %>% 
  ungroup() %>% 
  
  #unweighted death rate
  mutate(
    death_rate_ac_who_30_plus_crude_pt=n_deaths_ac_who_pt/pop_who_pt,

    #July 2, 2024: integrate uncertainty intervals based on imputed values
    death_rate_na_who_30_plus_crude_pt=n_deaths_na_who_pt/pop_who_pt,
    death_rate_na_who_30_plus_crude_ll=n_deaths_na_who_ll/pop_who_pt,
    death_rate_na_who_30_plus_crude_ul=n_deaths_na_who_ll/pop_who_pt
  ) %>% 
  #link in the proportion of the population 30+ for easier linking later on
  left_join(prop_pop_30_plus_who,by="iso3_alpha_code")


#who_deaths_by_country_2019_30_plus_std %>% View()
who_deaths_by_country_2019_30_plus_std %>% 
  dplyr::select(contains("iso"),contains("death_rate")) %>% 
  arrange(desc(death_rate_ac_who_30_plus_std_pt)) 

who_deaths_by_country_2019_30_plus_std %>% 
  arrange(desc(death_rate_ac_who_30_plus_crude_pt)) %>% 
  print(n=100)

who_deaths_by_country_2019_30_plus_std %>% 
  arrange(desc(pop_who_pt)) %>% 
  print(n=50)

who_deaths_by_country_2019 %>% 
  filter(age!="all") %>% 
  #total over all age groups to get the proportion in each age group
  group_by(iso3_alpha_code) %>% 
  mutate(pop_all_ages=sum(pop,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(age_prop=pop/pop_all_ages) 

who_deaths_by_country_2019_30_plus_std %>% 
  arrange(desc(death_rate_ac_who_30_plus_crude_pt)) %>% 
  dplyr::select(contains("iso"),contains("crude"))

0.0331*100000
#something different between the WHO and GBD estimatES
#what's the overall pop?
#3.9 billion
who_deaths_by_country_2019_30_plus_std %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_who_pt=sum(pop_who_pt,na.rm=T))



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
## GBD death rate----------
#May 19th 2023 Note the reported rate is per 100,000! see GBD website
#December 5, 2023:
#Two issues: rates are not among 20+ only. They're all adults. 
#That doesn't make sense.
#In addition, I should add an age adjustment.

#I'm revising the input file from GBD_deaths_by_country.xlsx
#to gbd_death_rate_by_country_age_adj
#Jan 16, 2024: I've revised the foldre structure a bit to separate
#IHME data from un

### overall and 20+------
setwd(here("data-input","ihme-mortality-population"))
gbd_death_rate_by_country=read_excel("gbd_death_rate_by_country_age_adj.xlsx") %>% 
  rename(location_name_gbd_orig=location) %>% #orig for original in case needed
  #To improve link with UN data, change name of a few.
  #Note there would be no need for this if GBD provided a standard iso3 alpha code.
  mutate(
    #still need CÃ´te d'Ivoire
    #    Democratic People's Republic of Korea
    location_name_gbd=case_when(
      location_name_gbd_orig=="Taiwan (Province of China)"~"China, Taiwan Province of China",
      location_name_gbd_orig=="Micronesia (Federated States of)" ~"Micronesia (Fed. States of)",
      location_name_gbd_orig=="Turkey"~"Türkiye",
      location_name_gbd_orig=="Palestine"~"State of Palestine",
      location_name_gbd_orig== "CÃ´te d'Ivoire"~"Côte d'Ivoire",
      location_name_gbd_orig=="C√¥te d'Ivoire"~"Côte d'Ivoire",#added Dec 5, 2023
      location_name_gbd_orig=="Democratic People's Republic of Korea"~"Dem. People's Republic of Korea",
      TRUE ~location_name_gbd_orig
    )) %>% 
  mutate(
    gbd_flag=1,
    #The age category will be used to create var names, so
    #clean it up a little so it looks nicer as a var suffix
         age_underscore=case_when(
           age=="20+ years"~"age_20_plus",
           age=="Age-standardized"~"age_std",#standardized,
           age=="All ages"~"age_all"
         )
  ) %>% 
  rename(
    #similarly for the "value". as of Dec 5, 2023,
    #we only have rates, so rename them as such so they appear better below
    death_rate_ac_per_100k_gbd_pt=val,
    death_rate_ac_per_100k_gbd_ul=upper,#upper limit
    death_rate_ac_per_100k_gbd_ll=lower#upper limit
  ) %>% 
  #and we don't need the cause, as it's all cause,
  #or sex, as it's both, or the metric, as they're all rates
  #or the year, as we know they're 2019
  dplyr::select(starts_with("location"),everything()) %>% 
  dplyr::select(-measure, -sex,-cause,-metric,-year,-age)

#a lookup for the original vs revised gbd location
lookup_location_name_gbd_location_name_gbd_orig=gbd_death_rate_by_country %>% 
  distinct(location_name_gbd,location_name_gbd_orig)

lookup_location_name_gbd_location_name_gbd_orig

lookup_location_name_gbd_location_name_gbd_orig
#gbd_death_rate_by_country %>% View()

names(gbd_death_rate_by_country)
gbd_death_rate_by_country
#table(gbd_death_rate_by_country$age)
#table(gbd_death_rate_by_country$age_underscore)
#Jan 16, 2024 - Removing the 20+ stuff. See old code if needed
#old/read-united-nations-gbd-data-pre-jan16-2024.R
gbd_death_rate_by_country_wide=gbd_death_rate_by_country %>% 
  #Modify these values so they work better as variable names
  pivot_wider(
    names_from=age_underscore,
    values_from = c(
      death_rate_ac_per_100k_gbd_pt, 
      death_rate_ac_per_100k_gbd_ul, 
      death_rate_ac_per_100k_gbd_ll)
    ) %>% 
  #cool, now I could calculate a standardized rate
  #I'm curious which country is the closest to the standard age distribution?
  mutate(
    death_rate_ac_per_100k_gbd_pt_std_ratio=death_rate_ac_per_100k_gbd_pt_age_std/
      death_rate_ac_per_100k_gbd_pt_age_all,

    death_rate_ac_gbd_pt_age_all=death_rate_ac_per_100k_gbd_pt_age_all/100000,
    death_rate_ac_gbd_ul_age_all=death_rate_ac_per_100k_gbd_ul_age_all/100000,
    death_rate_ac_gbd_ll_age_all=death_rate_ac_per_100k_gbd_ll_age_all/100000,
    
    death_rate_ac_gbd_pt_age_std=death_rate_ac_per_100k_gbd_pt_age_std/100000,
    death_rate_ac_gbd_ul_age_std=death_rate_ac_per_100k_gbd_ul_age_std/100000,
    death_rate_ac_gbd_ll_age_std=death_rate_ac_per_100k_gbd_ll_age_std/100000  
    )

names(gbd_death_rate_by_country_wide)
#gbd_death_rate_by_country_wide %>% View()

gbd_death_rate_by_country_wide %>% 
  group_by(gbd_flag) %>% 
  summarise(n=n())#204


### GBD death rate by age group: 30+----
#Jan 16, 2024:
#This is what we're using: 5-year age categories to match WHO data
#provided above. ensuring that we match fineness of age stratification
#will make a closer match
who_deaths_by_country_2019 %>% 
  group_by(age) %>% 
  summarise(n=n())

setwd(here("data-input","ihme-mortality-population"))
gbd_n_deaths_rate_30_plus=read_csv("ihme-mortality-by-country-age-2019-30-plus.csv") %>% 
 # filter(metric_name=="Rate") %>% 
  rename(location_name_gbd_orig=location_name) %>% 
  #calling location_id_gbd location_id_gbd to avoid confusion with other location codes
  rename(location_id_gbd=location_id) %>% 
  mutate(
    #I like Pier's data above, where the age ranges are simply indicated
    #by their lower bound
    #can make it numeric though
    age_cat=case_when(
      age_name=="30-34 years"~30,
      age_name=="35-39 years"~35,
      age_name=="40-44 years"~40,
      age_name=="45-49 years"~45,
      age_name=="50-54 years"~50,
      age_name=="55-59 years"~55,
      age_name=="60-64 years"~60,
      age_name=="65-69 years"~65,
      age_name=="70-74 years"~70,
      age_name=="75-79 years"~75,
      age_name=="80-84"~80,
      age_name=="85+ years"~85,
    )
  ) %>% 
  #remove sex id and sex name (both) and age_id and cause id
  #can remove year as well. it's 2019
  dplyr::select(
    -contains("sex"),-contains("age_id"),-contains("cause_id"),
    -year,-contains("metric_id"), -contains("measure_id")
    ) %>% 
  #can get rid of "age_name" as well, but keep separate for checking
  dplyr::select(-starts_with("age_name")) %>% 
  #can remove cause_name as well. we know this is all-cause
  dplyr::select(-starts_with("cause")) %>% 
  dplyr::select(starts_with("location"),starts_with("age"),everything())

gbd_n_deaths_rate_30_plus
gbd_n_deaths_rate_30_plus %>% 
  filter(metric_name=="Number") %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(n_deaths=sum(val))


#now limit to this measure. easy.
gbd_death_rate_30_plus=gbd_n_deaths_rate_30_plus %>% 
  filter(metric_name=="Rate") %>% 
  rename(
    death_rate_ac_per_100k_gbd_pt=val,
    death_rate_ac_per_100k_gbd_ul=upper,#upper limit
    death_rate_ac_per_100k_gbd_ll=lower#upper limit
  ) %>% 
  mutate(
    #death rate - not per 100k
    death_rate_ac_gbd_pt=death_rate_ac_per_100k_gbd_pt/100000,
    death_rate_ac_gbd_ul=death_rate_ac_per_100k_gbd_ul/100000,
    death_rate_ac_gbd_ll=death_rate_ac_per_100k_gbd_ll/100000
  ) %>% 
  dplyr::select(-contains("measure_name"),-contains("metric_name"))


gbd_death_rate_30_plus

# table(gbd_death_rate_30_plus$age_name,
#       gbd_death_rate_30_plus$age_cat
#       )
# gbd_death_rate_30_plus %>% 
#   group_by(age_name) %>% 
#   summarise(n=n())
# table(gbd_death_rate_30_plus$age_name,
#       gbd_death_rate_30_plus$age_cat)

### lookup GBD location_id_gbd---------
#let's get a location id - name lookup
lookup_location_name_gbd_orig_location_id_gbd=gbd_death_rate_30_plus %>% 
  distinct(location_id_gbd, location_name_gbd_orig) 

#a flag by location_id_gbd to indicate locations in above dataset
location_id_gbd_flag_gbd_death_rate=lookup_location_name_gbd_orig_location_id_gbd %>% 
  mutate(flag_gbd_death_rate=1) %>% 
  dplyr::select(location_id_gbd, contains("flag"))

location_id_gbd_flag_gbd_death_rate
lookup_location_name_gbd_orig_location_id_gbd
# table(gbd_death_rate_30_plus$age_name)  
# table(gbd_death_rate_30_plus$age_name,
#       gbd_death_rate_30_plus$age_cat
#       )
#gbd_death_rate_30_plus %>% View()



## GBD n_deaths------
### 30 plus-------
#Updating this to simply use data above
gbd_n_deaths_30_plus=gbd_n_deaths_rate_30_plus %>% 
  filter(metric_name=="Number") %>% 
  rename(
    #adding _ac for all_cause to differentiate from non-accidental (na)
    n_deaths_ac_gbd_pt=val,
    n_deaths_ac_gbd_ul=upper,#upper limit
    n_deaths_ac_gbd_ll=lower#upper limit
  ) %>% 
  dplyr::select(-contains("measure_name"),-contains("metric_name")) %>% 
  #and don't need location name either as we can link by id
  dplyr::select(-starts_with("location_name"))

gbd_n_deaths_30_plus
#Confirmed this is the absolute number and not expressed per 1,000 etc.
gbd_n_deaths_30_plus  %>% 
  arrange(desc(n_deaths_ac_gbd_pt))

gbd_n_deaths_30_plus %>% 
  group_by(age_cat) %>% 
  summarise(n_deaths_ac_gbd_pt=sum(n_deaths_ac_gbd_pt))

#This is all I need. Gather more later if needed. Delete the rest.
#I want total number of deaths as well. 
#Use their numbers, as it may be age-adjusted, etc.,
#and may not be a simple n/population
#Update Dec 5, 2023: I'm adding one for all deaths and one for just 20+
#I used to have this
### all------
#keeping this here to avoid breaking other code Jan 16, 2024
setwd(here("data-input","ihme-mortality-population"))
gbd_n_deaths_by_country_wide=read_excel("GBD_n_deaths_by_country.xlsx") %>% 
  rename(location_name_gbd_orig=location) %>% 
  mutate(
    age_underscore=case_when(
      age=="20+ years"~"age_20_plus",
      age=="All ages"~"age_all"
    )
  ) %>% 
  rename(
    n_deaths_ac_gbd_pt=val,
    n_deaths_ac_gbd_ul=upper,#upper limit
    n_deaths_ac_gbd_ll=lower#upper limit
  ) %>% 
  #and we don't need the cause, as it's all cause,
  #or sex, as it's both, or the metric, as they're all rates
  #or the year, as we know they're 2019
  dplyr::select(
    -measure, -sex,-cause,-metric,-year,-age
  ) %>% 
  #and now pivot it wider so that we have n_deaths for each age group
  #as a variable name
  pivot_wider(
    names_from=age_underscore,
    values_from = c(
      n_deaths_ac_gbd_pt, 
      n_deaths_ac_gbd_ul, 
      n_deaths_ac_gbd_ll)
  ) %>% 
  dplyr::select(-contains("20")) 

gbd_n_deaths_by_country_wide


## GBD population data-------
### all ages---------
#overall pop for the needed pop ratio
setwd(here("data-input","ihme-mortality-population"))
gbd_pop_all_ages=read_csv("ihme-pop-by-country-age-2019.csv") %>% 
  #calling location_id_gbd location_id_gbd to avoid confusion with other location codes
  rename(location_id_gbd=location_id) %>% 
  left_join(location_id_gbd_flag_gbd_death_rate,by="location_id_gbd") %>% 
  filter(flag_gbd_death_rate==1) %>%
  filter(age_group_name=="All Ages") %>% 
  #rename these. Update Jan 16, 2024...calling these pop instead of pop_total
  rename(
    pop_gbd_pt=val,
    pop_gbd_ul=upper,#upper limit
    pop_gbd_ll=lower#upper limit
  ) %>% 
  #and limit to both sexes
  filter(sex_name=="both") %>% 
  #now can get rid of some stuff
  dplyr::select(starts_with("location"),everything()) %>% 
  dplyr::select(-contains("sex"),-contains("metric"),-contains("measure"),
                -contains("year"),-contains("age_group_id")) %>% 
  dplyr::select(-starts_with("location_name"),-contains("flag"))


gbd_pop_all_ages
gbd_pop_all_ages %>% 
  arrange(desc(pop_gbd_pt))
  
gbd_pop_all_ages %>% 
  mutate(dummy=1) %>%  
  group_by(dummy) %>% 
  summarise(
    pop_gbd_pt=sum(pop_gbd_pt,na.rm=T),
    pop_gbd_ll=sum(pop_gbd_ll),
    pop_gbd_ul=sum(pop_gbd_ul),
    )

### 5-year age groups - 30 plus-----------
setwd(here("data-input","ihme-mortality-population"))
gbd_pop_by_age_30_plus=read_csv("ihme-pop-by-country-age-2019.csv") %>% 
  #calling location_id_gbd location_id_gbd to avoid confusion with other location codes
  rename(location_id_gbd=location_id) %>% 
  #limit to locations with mortality rate data
  left_join(location_id_gbd_flag_gbd_death_rate,by="location_id_gbd") %>% 
  filter(flag_gbd_death_rate==1) %>%
  #note pop text is different fro above, so make it match above
  mutate(
    age_cat=case_when(
      age_group_name=="30 to 34"~30,
      age_group_name=="35 to 39"~35,
      age_group_name=="40 to 44"~40,
      age_group_name=="45 to 49"~45,
      age_group_name=="50 to 54"~50,
      age_group_name=="55 to 59"~55,
      age_group_name=="60 to 64"~60,
      age_group_name=="65 to 69"~65,
      age_group_name=="70 to 74"~70,
      age_group_name=="75 to 79"~75,
      age_group_name=="80 to 84"~80,
      age_group_name=="85 plus"~85,
    )
  ) %>% 
  #rename these
  rename(
    pop_gbd_pt=val,
    pop_gbd_ul=upper,#upper limit
    pop_gbd_ll=lower#upper limit
  ) %>%    
  #now limit to age groups where age_cat is not missing
  filter(is.na(age_cat)==F) %>% 
  #and limit to both sexes
  filter(sex_name=="both") %>% 
  #now can get rid of some stuff
  dplyr::select(starts_with("location"),everything()) %>% 
  dplyr::select(-contains("sex"),-contains("metric"),-contains("measure"),
                -contains("year"),-contains("age_group_id")) %>% 
  #can also remove location_name, as the id will work,
  #nor do we need this "flag_gbd_death rate anymore
  dplyr::select(-starts_with("location_name"),-starts_with("flag_gbd")) %>% 
  #and finally can get rid of age_group_name now. keep separate for checking
  dplyr::select(-starts_with("age_group_name"))

#table(gbd_pop_by_age_30_plus$sex_name)
gbd_pop_by_age_30_plus
n_distinct(gbd_pop_by_age_30_plus$location_id_gbd)


gbd_pop_by_age_30_plus %>% 
  group_by(age_cat) %>% 
  summarise(n=n()) %>% 
  print(n=100)

# gbd_pop_by_age_30_plus %>% 
#   group_by(age_cat,age_group_name) %>% 
#   summarise(n=n()) %>% 
#   print(n=100)

### Proportion 30+ for weighting------
prop_pop_30_plus_gbd=gbd_pop_by_age_30_plus %>% 
  group_by(location_id_gbd) %>% 
  summarise(
    pop_30_plus_gbd_pt=sum(pop_gbd_pt,na.rm=T),
    pop_30_plus_gbd_ul=sum(pop_gbd_ul,na.rm=T),
    pop_30_plus_gbd_ll=sum(pop_gbd_ll,na.rm=T),
    ) %>% 
  left_join(gbd_pop_all_ages,by="location_id_gbd") %>% 
  mutate(
    pop_prop_30_plus_gbd_pt=pop_30_plus_gbd_pt/pop_gbd_pt,
    pop_prop_30_plus_gbd_ul=pop_30_plus_gbd_ul/pop_gbd_ul,
    pop_prop_30_plus_gbd_ll=pop_30_plus_gbd_ll/pop_gbd_ll
  ) %>% 
  #only keep the proportions and the location id
  dplyr::select(contains("location"),contains("prop"))

prop_pop_30_plus_gbd %>% 
  arrange(desc(pop_prop_30_plus_gbd_pt)) %>% 
  left_join(lookup_location_name_gbd_orig_location_id_gbd,by="location_id_gbd")

#compare with WHO data
prop_pop_30_plus_who %>% arrange(desc(pop_prop_30_plus_who_pt))

summary(prop_pop_30_plus_gbd$pop_prop_30_plus_gbd_pt)
nrow(prop_pop_30_plus_who)
nrow(prop_pop_30_plus_gbd)
nrow(un_pop_2019)

## GBD life expectancy-------
setwd(here("data-input","ihme-mortality-population"))
#note - both sexes, life expectancy at birth, year 2019
gbd_death_rate_by_country_le_wide=read_excel("GBD_LE_by_country.xlsx") %>% 
  dplyr::select(location_name, val, upper, lower) %>% 
  rename(le_birth_pt=val,
         le_birth_ul=upper,
         le_birth_ll=lower,
         location_name_gbd_orig=location_name #to be consistent with how I name it earlier
         )

gbd_death_rate_by_country_le_wide



## Linking GBD together--------
### country level-----
#Jan 16, 2024: not using this anymore. may later for urban health paper
#keeping for posterity

names(gbd_death_rate_by_country_wide)
names(gbd_n_deaths_by_country_wide)
names(gbd_death_rate_by_country_le_wide)
gbd_wide_all_ages=gbd_death_rate_by_country_wide %>%
  left_join(gbd_n_deaths_by_country_wide,by="location_name_gbd_orig") %>%
  left_join(gbd_death_rate_by_country_le_wide,by="location_name_gbd_orig") %>% 
  #only include the id for linking below
  left_join(lookup_location_name_gbd_orig_location_id_gbd,by="location_name_gbd_orig") %>% 
  dplyr::select(-contains("location_name"))  %>% 
  #Jan 16, 2024
  #actually, to avoid confusion, you should exclude all the standardized estimates here,
  #as they're different than how I did it manually
  dplyr::select(-contains("std")) %>% 
  #also remove the 20plus stuff, as it's distracting now
  dplyr::select(-contains("20_p")) %>% 
  dplyr::select(contains("location"),everything())

names(gbd_wide_all_ages)

### by country-age-------
#Jan 16, 2024: doing it this way
gbd_death_rate_30_plus
nrow(gbd_n_deaths_30_plus)
nrow(gbd_death_rate_30_plus)
nrow(gbd_pop_by_age_30_plus)
gbd_pop_by_age_30_plus %>% 
  group_by(age_cat) %>% 
  summarise(pop_gbd_pt=sum(pop_gbd_pt))
gbd_pop_by_age_30_plus %>%
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_gbd_pt=sum(pop_gbd_pt))

gbd_country_age_30_plus=gbd_death_rate_30_plus %>% 
  left_join(gbd_pop_by_age_30_plus,by=c("location_id_gbd","age_cat")) %>% 
  left_join(gbd_n_deaths_30_plus,by=c("location_id_gbd","age_cat")) %>% 
  #and now define weights for standardization as I did above with WHO data
  #total over all age groups to get the proportion in each age group
  group_by(location_id_gbd) %>% 
  #note these proportions are among adults 30+
  #just use pop point estimate
  #intentionally group_by() mutate().
  mutate(pop_all_ages_gbd_pt=sum(pop_gbd_pt,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(age_prop_gbd_pt=pop_gbd_pt/pop_all_ages_gbd_pt)

names(gbd_country_age_30_plus)
summary(gbd_country_age_30_plus$age_prop_gbd_pt)#this is the weights for stdization
gbd_country_age_30_plus

### age-standardize mortality rates-----

#First need to know the age distribution overall
gbd_age_prop_all_countries= gbd_country_age_30_plus %>% 
  group_by(age_cat) %>% 
  summarise(pop_all_countries=sum(pop_gbd_pt,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(
    pop_30_plus_all_countries=sum(pop_all_countries,na.rm=T),
    age_prop_all_countries=pop_all_countries/pop_30_plus_all_countries
  ) 

gbd_age_prop_all_countries %>%
  mutate(hi=sum(age_prop_all_countries))



gbd_age_prop_all_countries
gbd_country_age_30_plus
names(gbd_country_age_30_plus)
n_distinct(gbd_country_age_30_plus$location_id_gbd)
n_distinct(gbd_country_age_30_plus$age_cat)
12*204
nrow(gbd_country_age_30_plus)
gbd_deaths_by_country_2019_30_plus_std=gbd_country_age_30_plus %>% 
  left_join(gbd_age_prop_all_countries,by="age_cat") %>% 
  #now calculate age-standardized rates
  group_by(location_id_gbd) %>% 
  summarise(
    #Jan 17, 2024: include intervals
    n_deaths_ac_gbd_pt=sum(n_deaths_ac_gbd_pt,na.rm=T),
    n_deaths_ac_gbd_ll=sum(n_deaths_ac_gbd_ll,na.rm=T),
    n_deaths_ac_gbd_ul=sum(n_deaths_ac_gbd_ul,na.rm=T),
    
    #Jan 17, 2024: ah, I found my issue. I was summing the "all ages"
    #population over all ages again. error. this should fix it.
    pop_all_ages_gbd_pt=sum(pop_gbd_pt,na.rm=T),#summed over the 30+ age ranges
    #age-standardize the death rate 
    death_rate_ac_gbd_30_plus_std_pt=weighted.mean(
      x=death_rate_ac_gbd_pt,
      w=age_prop_all_countries
    ),
    death_rate_ac_gbd_30_plus_std_ll=weighted.mean(
      x=death_rate_ac_gbd_ll,
      w=age_prop_all_countries
    ),
    death_rate_ac_gbd_30_plus_std_ul=weighted.mean(
      x=death_rate_ac_gbd_ul,
      w=age_prop_all_countries
    ),
    
  ) %>% 
  ungroup() %>% 
  #unweighted death rate
  mutate(
    death_rate_ac_gbd_30_plus_crude_pt=n_deaths_ac_gbd_pt/pop_all_ages_gbd_pt,
    death_rate_ac_gbd_30_plus_crude_ll=n_deaths_ac_gbd_ll/pop_all_ages_gbd_pt,
    death_rate_ac_gbd_30_plus_crude_ul=n_deaths_ac_gbd_ul/pop_all_ages_gbd_pt,
  ) %>% 
  #and finally link in the gbd revised name for linking
  left_join(lookup_location_name_gbd_orig_location_id_gbd,
            by="location_id_gbd") %>% 
  left_join(lookup_location_name_gbd_location_name_gbd_orig,
            by="location_name_gbd_orig") %>% 
  #link in the proportion of the population 30 plus for use downstream
  left_join(prop_pop_30_plus_gbd,by="location_id_gbd") %>% 
  dplyr::select(starts_with("location"),everything())

#gbd_deaths_by_country_2019_30_plus_std %>% View()
gbd_deaths_by_country_2019_30_plus_std %>% 
  arrange(desc(death_rate_ac_gbd_30_plus_std_pt)) %>% #View()
  print(n=50)

gbd_deaths_by_country_2019_30_plus_std %>% 
  arrange(desc(death_rate_ac_gbd_30_plus_crude_pt)) %>% 
  print(n=100)

gbd_deaths_by_country_2019_30_plus_std %>% 
  dplyr::select(contains("location"),contains("crude"))

#something off. what's the total pop?
gbd_deaths_by_country_2019_30_plus_std %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_all_ages_gbd_pt=sum(pop_all_ages_gbd_pt,na.rm=T))
#yup, the total population is off. it was 47 billion
#fixed now. 3.9 billion again

who_deaths_by_country_2019_30_plus_std %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_who_pt=sum(pop_who_pt,na.rm=T))


# Linking GBD with UN and WHO country identifiers------
## lookup between gbd id and iso3--------
## Link GBD data with UN and WHO
#I need a way to link GBD with WHO. Ideally the common key would be is03 alpha code
lookup_location_id_gbd_iso3_alpha_code=un_deaths_2019 %>% 
  left_join(gbd_death_rate_by_country_wide,
            by=c("region_subregion_country_or_area"="location_name_gbd")
  ) %>% 
  dplyr::select(location_code_un, contains("gbd")) %>% 
  left_join(lookup_location_name_gbd_orig_location_id_gbd,
            by="location_name_gbd_orig") %>% 
  dplyr::select(contains("location")) %>% 
  left_join(lookup_un_country_code_three_letter_code,by="location_code_un") %>% 
  distinct(location_id_gbd,iso3_alpha_code)

#looks good. this works.
lookup_location_id_gbd_iso3_alpha_code %>% 
  print(n=300)

n_distinct(gbd_deaths_by_country_2019_30_plus_std$location_id_gbd)
n_distinct(lookup_location_id_gbd_iso3_alpha_code$location_id_gbd)
n_distinct(lookup_location_id_gbd_iso3_alpha_code$iso3_alpha_code)
nrow(lookup_location_id_gbd_iso3_alpha_code)

un_pop_2019_for_join = un_pop_2019 %>% 
  dplyr::select(starts_with("location_code"), 
                starts_with("pop")) #join by index, actually

## Final country-level dataset---------
#life expectancy added as of Sep 14, 2023
#Updated Jan 18, 2024: note the GBD data is all-cause
#and we now have WHO data including non-accidental
un_pop_deaths_2019 = un_deaths_2019 %>% 
  left_join(lookup_un_country_code_three_letter_code,by="location_code_un") %>% 
  left_join(wb_class, by ="iso3_alpha_code") %>% 
  left_join(un_pop_2019_for_join, by = "location_code_un") %>% #link with this May 15th 2023
  left_join(un_prop_urban_for_join,by="location_code_un") %>% 
  left_join(gini_by_country, by ="iso3_alpha_code") %>% #added Dec 4, 2023
  left_join(lookup_location_id_gbd_iso3_alpha_code,by="iso3_alpha_code") %>% 
  #and now all of the GBD data can be brought in
  left_join(gbd_deaths_by_country_2019_30_plus_std,by="location_id_gbd") %>% 
  left_join(gbd_wide_all_ages,by="location_id_gbd") %>% 
  #and the WHO data
  left_join(who_deaths_by_country_2019_30_plus_std,by="iso3_alpha_code") %>% 
  
  mutate(
    #recalculate the number of deaths using GBD rate and UN population
    #to see how close it is to the other GBD estimates.
    #Calculated from UN pop and the reported rate
    #Dec 5, 2023: different var names here
    n_deaths_ac_gbd_un_pop_pt=death_rate_ac_gbd_pt_age_all*pop_total_un,#pop comes from UN above
    n_deaths_ac_gbd_un_pop_ul=death_rate_ac_gbd_ul_age_all*pop_total_un,#pop comes from UN above
    n_deaths_ac_gbd_un_pop_ll=death_rate_ac_gbd_ll_age_all*pop_total_un,#pop comes from UN above
    
    #ratio of the two n_deaths_measures so I can easily compare
    n_deaths_ac_gbd_pt_ratio_of_measures=n_deaths_ac_gbd_pt_age_all/n_deaths_ac_gbd_un_pop_pt
  ) %>% 
  #There is a column called "Type" in UN pop & death data to indicate whether the 
  #column "region_subregion_country_or_area"
  #is a country or a broader region
  #If it's a country, its value is "Country/Area". Change that in the other code first.

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
    n_deaths_ac_urban_gbd_pt=prop_urban_pop*n_deaths_ac_gbd_pt_age_all,
    n_deaths_ac_urban_gbd_ul=prop_urban_pop*n_deaths_ac_gbd_ul_age_all,
    n_deaths_ac_urban_gbd_ll=prop_urban_pop*n_deaths_ac_gbd_ll_age_all,
    
    #and do the same thing using the alternate n_deaths_calc
    n_deaths_ac_urban_gbd_un_pop_pt=prop_urban_pop*n_deaths_ac_gbd_un_pop_pt,
    n_deaths_ac_urban_gbd_un_pop_ul=prop_urban_pop*n_deaths_ac_gbd_un_pop_ul,
    n_deaths_ac_urban_gbd_un_pop_ll=prop_urban_pop*n_deaths_ac_gbd_un_pop_ll,
    
    #also would be good to have total population urban for ref
    pop_urban=pop_total_un*prop_urban_pop,
  
    #July 18, 2024: I need an indicator variable for presence/absence of WHO mortality dta
    who_data_missing_country=case_when(
      is.na(n_deaths_ac_who_pt)==T~1,
      TRUE~0
    ),
    
    #so that I can apply the same method throughout, add this here
    irr_ur=1 #so I can use the same formula
  ) %>% 
  #remove anything with a dash
  dplyr::select(-contains("-")) %>% 
  dplyr::select(contains("location"),contains("_un"),contains("gbd"),
                contains("who"),
                everything())

setwd(here("data-processed"))
save(un_pop_deaths_2019, file = "un_pop_deaths_2019.RData")
names(un_pop_deaths_2019)
# un_pop_deaths_2019 %>% 
#   dplyr::select(country_name_en, contains("who")) %>% 
#   View()
table(un_pop_deaths_2019$who_data_missing_country)
un_pop_deaths_2019 %>% 
  filter(who_data_missing_country==1) %>% 
  arrange(country_name_en) %>% 
  dplyr::select(country_name_en) %>% 
  pull()

#get a look-up for the who data missing country variable so I can investigate
#why some GUBs still included even though they should be excluded
lookup_who_data_missing_country=un_pop_deaths_2019 %>% 
  distinct(country_name_en,who_data_missing_country)

lookup_who_data_missing_country
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
n_distinct(un_pop_deaths_2019$location_code_un)
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"), starts_with("region"),contains("wb")) %>%
  arrange(country_name_en) %>% 
  print(n=250)

#how many rows of the downward population weights?
nrow(un_pop_deaths_2019)
n_distinct(un_pop_deaths_2019$pop_prop_30_plus_gbd_pt)
n_distinct(un_pop_deaths_2019$pop_prop_30_plus_who_pt)

un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"),contains("code")) %>% 
  arrange(location_code_un)

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

lookup_country_name_en_pop_total_un=un_pop_deaths_2019 %>% 
  dplyr::select(country_name_en, pop_total_un)

#lookup_country_name_en_pop_total_un %>% View()

setwd(here("data-processed"))
save(
  lookup_country_name_en_pop_total_un,
     file="lookup_country_name_en_pop_total_un.RData")
lookup_country_name_en_pop_total_un %>% arrange(desc(pop_total_un))

#check. does this add up to 7.7 billion?
lookup_country_name_en_pop_total_un %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(pop_total_un=sum(pop_total_un,na.rm=T))

#How close are the two n_deaths GBD calculations?
un_pop_deaths_2019 %>% 
  dplyr::select(contains("name"),contains("n_deaths_ac_gbd")) 

names(un_pop_deaths_2019)
#okay good - most are 1, but not all
un_pop_deaths_2019 %>% 
  ggplot(aes(x=n_deaths_ac_gbd_pt_ratio_of_measures))+
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
    n_deaths_ac_gbd_pt_age_all=sum(n_deaths_ac_gbd_pt_age_all,na.rm=TRUE),
    pop_total_un=sum(pop_total_un,na.rm=TRUE),
    pop_urban=sum(pop_urban,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(death_rate_ac_gbd_pt_age_all=n_deaths_ac_gbd_pt_age_all/pop_total_un,
         n_deaths_ac_urban_gbd_pt=death_rate_ac_gbd_pt_age_all*pop_urban
         )

names(un_pop_deaths_2019)
##lookup country name - 3-letter code------
lookup_country_name_en_iso3_alpha_code=un_pop_deaths_2019 %>% 
  distinct(country_name_en,iso3_alpha_code)

  
## Plots of perc urban vs ------
names(un_pop_deaths_2019)
# un_pop_deaths_2019 %>% 
#   ggplot(aes(x=perc_urban,y=death_rate_un_20_plus))+
#   geom_point(aes(colour=income_grp_wb_fac))

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_ac_gbd_pt_age_all))+
  geom_point(aes(colour=income_grp_wb_fac))+
  geom_smooth()

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=death_rate_ac_gbd_pt_age_all))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows=vars(income_grp_wb_fac))

un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=n_deaths_ac_gbd_pt_age_all))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows=vars(income_grp_wb_fac))

# un_pop_deaths_2019 %>% 
#   ggplot(aes(x=perc_urban,y=death_rate_un_20_plus))+
#   geom_point(aes(colour=region_wb_dl))

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
  arrange(desc(n_deaths_ac_urban_gbd_pt)) %>% 
  dplyr::select(contains("country_name"),contains("n_deaths_ac_urban_gbd_pt"),contains("urban")) %>% 
  print(n=250)
un_pop_deaths_2019 %>% 
  ggplot(aes(x=perc_urban,y=n_deaths_ac_urban_gbd_pt))+
  geom_point(aes(colour=region_wb_dl))

#overall summary
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(n_deaths_ac_urban_gbd_pt=sum(n_deaths_ac_urban_gbd_pt,na.rm=TRUE))
#32 million in 2019

#By World Bank region
names(un_pop_deaths_2019)
un_pop_deaths_2019 %>% 
  group_by(region_wb_dl) %>% 
  summarise(
    # n_deaths_ac_urban_gbd_pt=sum(n_deaths_ac_urban_gbd_pt,na.rm=TRUE),
    # n_deaths_ac_urban_gbd_ll=sum(n_deaths_ac_urban_gbd_ll,na.rm=TRUE),
    # n_deaths_ac_urban_gbd_ul=sum(n_deaths_ac_urban_gbd_ul,na.rm=TRUE),
    n_deaths_ac_urban_gbd_un_pop_pt=sum(n_deaths_ac_urban_gbd_un_pop_pt,na.rm=TRUE),
    n_deaths_ac_urban_gbd_un_pop_ll=sum(n_deaths_ac_urban_gbd_un_pop_ll,na.rm=TRUE),
    n_deaths_ac_urban_gbd_un_pop_ul=sum(n_deaths_ac_urban_gbd_un_pop_ul,na.rm=TRUE)
    ) %>% 
  ungroup() %>% 
  arrange(desc(n_deaths_ac_urban_gbd_un_pop_pt))

un_pop_deaths_2019 %>% 
  group_by(region_wb_dl) %>% 
  summarise(n_deaths_ac_gbd_pt_age_all=sum(n_deaths_ac_gbd_pt_age_all,na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(n_deaths_ac_gbd_pt_age_all))

#Top 10 countries
un_pop_deaths_2019 %>% 
  dplyr::select(starts_with("country"), starts_with("n_deaths")) %>% 
  arrange(desc(n_deaths_ac_urban_gbd_pt)) 

79/77


## Mortality rate by country income group----
names(un_pop_deaths_2019)
table(un_pop_deaths_2019$income_grp_wb)
# un_pop_deaths_2019 %>% 
#   group_by(income_grp_wb_fac) %>% 
#   summarise(
#     death_rate_ac_per_100k_gbd_pt_age_20_plus=mean(
#       death_rate_ac_per_100k_gbd_pt_age_20_plus,na.rm=T),
#     death_rate_ac_per_100k_gbd_pt_age_std_20_plus=mean(
#       death_rate_ac_per_100k_gbd_pt_age_std_20_plus,na.rm=T
#     )
#   )

names(un_pop_deaths_2019)
## life expectancy by region (unweighted mean)
names(un_pop_deaths_2019)
#Total population data where le_birth_pt is non-missing
un_pop_deaths_2019 %>% 
  mutate(pop_le_product=pop_total_un*le_birth_pt) %>% 
  group_by(region_wb_dl) %>% 
  summarise(
    sum_of_pop_le_product =sum(pop_le_product,na.rm=TRUE),
    pop_total_un_sum=sum(pop_total_un,na.rm=TRUE),
    le_birth_pt_mean=mean(le_birth_pt, na.rm=TRUE),
    #I've actually never used the weighted mean function before, but it seems to work.
    #I should check it to make sure it's right.
    #https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/weighted.mean
    le_birth_pt_mean_weighted=weighted.mean(
      x=le_birth_pt,
      w=pop_total_un,
      na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(le_birth_pt_mean_weighted_calc=sum_of_pop_le_product /pop_total_un_sum)
#they're the same, almost. neat. use your direct calculation.

#Note to self: For urban-rural analysis, see
#global-ndvi-pop/scripts/analysis-urban-rural.R

