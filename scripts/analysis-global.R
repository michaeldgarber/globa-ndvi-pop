#Analysis-initial steps
# January 23, 2023
#Okay, here I'm making some decisions on what to do with the population categories.
#It might be simplest to just keep the categories as they are, 
#rather than group them further into tertiles,
#given they're already presented in categories
# Feb 21, 2023: now this script is just the global analysis

library(terra)
library(tidyterra)
library(mapview)
library(tidyverse)
library(here)
library(sf)
source(here("scripts", "analysis-functions.R"))
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups

#Dec 1, 2023: For the code that's used to create the "_not_miss"
#dataset, see
#scripts/final-data-combining.R

setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib_gub_not_miss.RData")
object.size(pop_ndvi_gub_biome_tib_gub_not_miss) 
nrow(pop_ndvi_gub_biome_tib_gub_not_miss)
names(pop_ndvi_gub_biome_tib_gub_not_miss)


#Of those with non-missing GUB, how many with NDVI below 0?
pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(ndvi_2019<0) %>% 
  nrow()
#0 

pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(is.na(ndvi_2019)) %>% 
  nrow()
#7550
7550/nrow(pop_ndvi_gub_biome_tib_gub_not_miss)

# pop_ndvi_gub_biome_tib_gub_not_miss %>% 
#   distinct(country_name_en) %>% 
#   View()



# Lookups-------

## lookups: country level -----------
# in case country is missing from city data
names(pop_ndvi_gub_biome_tib_gub_not_miss)
#note some of these overlap multiple countries, so do the below
#Oct 9, 2023: it's causing an error to have this include pop_cat_min_val.
#Exclude that
lookup_orig_fid_country_name_en=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(ORIG_FID,country_name_en) %>% 
  summarise(pop_cat_min_val=sum(pop_cat_min_val,na.rm=T)) %>% 
  group_by(ORIG_FID) %>% 
  arrange(desc(pop_cat_min_val)) %>% 
  slice(1) %>% #grab the top
  ungroup() %>% 
  arrange(ORIG_FID) %>% 
  dplyr::select(ORIG_FID, country_name_en)

lookup_orig_fid_country_name_en
setwd(here("data-processed"))
save(lookup_orig_fid_country_name_en,file="lookup_orig_fid_country_name_en.RData")


### Estimate country pop with LandScan vs UN estimates------

#Dec 1, 2023:
#as an overall check, what's the world population in 2019 according to LandScan
#If we sum over all cells?
names(pop_ndvi_gub_biome_tib_gub_not_miss)
total_world_pop_per_ls=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(
    pop_cat_min_val=sum(pop_cat_min_val,na.rm=T),
    pop_cat_max_val=sum(pop_cat_max_val,na.rm=T)
    )

total_world_pop_per_ls
mean(c(2,3))
mean(c(total_world_pop_per_ls$pop_cat_min_val,
     total_world_pop_per_ls$pop_cat_max_val))


#Nov 19 2023: here to calculate the ratio of UN pop vs Landscan upper bound
#Load this lookup, created in ~read-united-nations-gbd-data.R
lookup_country_name_en_pop_total_un
names(pop_ndvi_gub_biome_tib_gub_not_miss)
# pop_ndvi_gub_biome_tib_gub_not_miss %>% 
#   distinct(country_name_en) %>% 
#   View()
#ls for landscan; un for united nations
names(pop_ndvi_gub_biome_tib_gub_not_miss)
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_1_8)
country_pop_ls_un=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(country_name_en) %>% 
  summarise(
    pop_cat_mean_val_country_name_en = sum(pop_cat_mean_val,na.rm=TRUE),
    pop_cat_min_val_country_name_en = sum(pop_cat_min_val,na.rm=TRUE),
    pop_cat_max_val_country_name_en = sum(pop_cat_max_val,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  #Link the UN estimate by country
  left_join(lookup_country_name_en_pop_total_un,by="country_name_en") %>% 
  #now estimate ratios: at the country level,
  #what's the ratio of what UN says vs what LandScan values added up are?
  mutate(
    #again, ls means landscan, and un means united nations
    #Makes the most sense to express the ratio as UN vs the Landscan.
    #That way, I can simply apply this ratio as a weight to the estimates.
    
    ratio_pop_un_v_pop_cat_mean_ls=pop_total_un/pop_cat_mean_val_country_name_en,
    ratio_pop_un_v_pop_cat_max_ls=pop_total_un/pop_cat_max_val_country_name_en,
    ratio_pop_un_v_pop_cat_min_ls=pop_total_un/pop_cat_min_val_country_name_en
  )

#check the distribution of those ratios
summary(country_pop_ls_un$ratio_pop_un_v_pop_cat_mean_ls)
country_pop_ls_un %>% 
  ggplot(aes(x=ratio_pop_un_v_pop_cat_mean_ls))+
  geom_histogram()

summary(country_pop_ls_un$ratio_pop_un_v_pop_cat_max_ls)
country_pop_ls_un %>% 
  ggplot(aes(x=ratio_pop_un_v_pop_cat_max_ls))+
  geom_histogram()

#Interesting: so, maybe you can keep the min as is,
#as the ratio of the true pop and the min is such that
#the true pop is higher than the min, which we'd expect.
#Just scale the max.
summary(country_pop_ls_un$ratio_pop_un_v_pop_cat_min_ls)
country_pop_ls_un %>% 
  ggplot(aes(x=ratio_pop_un_v_pop_cat_min_ls))+
  geom_histogram()

#Nov 19, 2023
#I shouldn't use the mean; it's pointless at this stage. I can
#take the mean of the two estimates later.
#So, downstream in the analysis code, link by country,
#and then use this ratio to estimate pops at the city level using
#these country-level ratios

#This is the ratio we need. Then, downstream, calculate
#the mean between this value and the landscan min.

lookup_country_name_en_ratio_pop_un_v_pop_cat_max_ls=country_pop_ls_un %>% 
  dplyr::select(country_name_en,ratio_pop_un_v_pop_cat_max_ls)

#Nov 19; examine and fix missings
#Fixed. see above.
#lookup_country_name_en_ratio_pop_un_v_pop_cat_max_ls %>% View()

### examine pop issue excluding top category-------
#what if we exclude the top category?
#I may just want to adjust that top category. 
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_1_8)
country_pop_ls_un_exclude_top=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(pop_cat_1_8!=8) %>% 
  group_by(country_name_en) %>% 
  summarise(
    #I'm calling these 1-7 because they exclude the top 8
    pop_cat_1_7_mean_val_country_name_en = sum(pop_cat_mean_val,na.rm=TRUE),
    pop_cat_1_7_min_val_country_name_en = sum(pop_cat_min_val,na.rm=TRUE),
    pop_cat_1_7_max_val_country_name_en = sum(pop_cat_max_val,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  #Link the UN estimate by country
  left_join(lookup_country_name_en_pop_total_un,by="country_name_en") %>% 
  #now estimate ratios: at the country level,
  #what's the ratio of what UN says vs what LandScan values added up are?
  mutate(
    #again, ls means landscan, and un means united nations
    #Makes the most sense to express the ratio as UN vs the Landscan.
    #That way, I can simply apply this ratio as a weight to the estimates.
    
    ratio_pop_un_v_pop_cat_1_7_mean_ls=pop_total_un/pop_cat_1_7_mean_val_country_name_en,
    ratio_pop_un_v_pop_cat_1_7_max_ls=pop_total_un/pop_cat_1_7_max_val_country_name_en,
    ratio_pop_un_v_pop_cat_1_7_min_ls=pop_total_un/pop_cat_1_7_min_val_country_name_en
  )



#check again
summary(country_pop_ls_un_exclude_top$ratio_pop_un_v_pop_cat_1_7_mean_ls)
country_pop_ls_un_exclude_top %>% 
  ggplot(aes(x=ratio_pop_un_v_pop_cat_1_7_mean_ls))+
  geom_histogram()

#okay, so the culprit really is this top category, because
#when I exclude the top, the true UN pop is quite a bit higher
#than the landscan estimated value
summary(country_pop_ls_un_exclude_top$ratio_pop_un_v_pop_cat_1_7_max_ls)
country_pop_ls_un_exclude_top %>% 
  ggplot(aes(x=ratio_pop_un_v_pop_cat_1_7_max_ls))+
  geom_histogram()

###a factor that corrects the top LandScan bound only at the country level-----
#the algebra is this:
# bottom_7+top*weight=truth
# top*weight=truth-bottom_7
# 
# weight=(truth-bottom_7)/top
#I have the bottom 7 estimates above.
#Getting rid of the ratios but otherwise can simply join it in with the top cat below
country_pop_ls_un_exclude_top_for_join=country_pop_ls_un_exclude_top %>% 
  dplyr::select(-starts_with("ratio"))

country_pop_ls_un_exclude_top_for_join
#I need to make one for just the top now.
country_pop_ls_un_top_only=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(pop_cat_1_8==8) %>% 
  group_by(country_name_en) %>% 
  summarise(
    #I'm calling these 1-7 because they exclude the top 8
    pop_cat_8_mean_val_country_name_en = sum(pop_cat_mean_val,na.rm=TRUE),
    pop_cat_8_min_val_country_name_en = sum(pop_cat_min_val,na.rm=TRUE),
    pop_cat_8_max_val_country_name_en = sum(pop_cat_max_val,na.rm=TRUE)
  ) %>% 
  ungroup() 

names(country_pop_ls_un_exclude_top_for_join)
pop_cat_8_corrected_country_name=country_pop_ls_un_top_only %>% 
  left_join(country_pop_ls_un_exclude_top_for_join,by="country_name_en") %>% 
  mutate(
    #Now define the correction factor following algebra above.
    pop_cat_8_max_wt=(pop_total_un-pop_cat_1_7_max_val_country_name_en)/
      pop_cat_8_max_val_country_name_en
  )

#Distribution of weights
summary(pop_cat_8_corrected_country_name$pop_cat_8_max_wt)
#note there are a couple negative weights because sometimes
#the bottom 7 categories add up to more than the truth.
#if so, then the negative weight makes sense.
pop_cat_8_corrected_country_name %>% 
  ggplot(aes(x=pop_cat_8_max_wt))+
  geom_histogram()

pop_cat_8_corrected_country_name %>% View()

#Okay, now let's grab a look-up for just that weight at the country level
#Hmm, this does work, but notice that the US has negative weights.
#This is because the sum of the bottom 7 categories exceeds the true total at
#the country level. That would make city-specific results strange.
#Another idea might be to apply the weight to the top 2 categories
#together rather than just the top. This would allow us to still carry out
#the analysis at the city level. Things might get a little strange depending
#on the distribution of the 7s and 8s

#Or we could simply apply the weight across the board, using
#those that we calculated at first.
#That wouldn't differentially affect the pop. categories, 
#even if it's the case that most of the over-estimation
#is coming from the top categories. Let's do that approach.

## Lookups: GUB level (ORIG_FID)--------
source(here("scripts", "read-boundaries-states-countries.R"))#load some lookups
#Can't do this in the steps below because that's at the pixel level,
#whereas this needs to be at the city level
names(pop_ndvi_gub_biome_tib_gub_not_miss)
#note this isn't the scaled value.
#Rather than loading all of these lookups, it might be simpler just to say:
#source(here("scripts", "read-gub.R")) #don't. it takes a long time.
setwd(here("data-processed"))
load("lookup_gub_area_km2.RData")
load("lookup_gub_city_id_simplemaps.RData")
load("lookup_gub_simplemaps_miss.RData")
load("lookup_orig_fid_country_name_en.RData")
load("lookup_gub_geoname_id.RData")
load("lookup_gub_geonames_miss.RData")
#Oct 7, 2023: in this code, we can also create an indicator for
#whether there is a duplicate city name
#I had lots of analysis steps in the HIA, but many ought to be done here

#July 22, 2024: why not use the scaled populatoin values?

lookup_gub_several_vars = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(ORIG_FID) %>% 
  summarise(
    #have to name them something different so they can be joined
    #without duplicating column names
    pop_cat_mean_val_gub = sum(pop_cat_mean_val,na.rm=TRUE),
    pop_cat_min_val_gub = sum(pop_cat_min_val,na.rm=TRUE),
    pop_cat_max_val_gub = sum(pop_cat_max_val,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
#  left_join(lookup_gub_area_km2, by ="ORIG_FID") %>% #area of GUB #Oct 9 - don't need this here

  #Oct 7, 2023: lookups that were previously in the hia step,
  #but they can be here
  #Oct 6, 2023 new lookups
  #These look-ups are created in ~read-gub.R
  left_join(lookup_gub_city_id_simplemaps,by="ORIG_FID") %>% #simplemaps id
  #whether simplemaps name missing
  left_join(lookup_gub_simplemaps_miss,by="ORIG_FID") %>%
  
  #the rest of simplemaps stuff
  left_join(lookup_all_vars_simplemaps_nogeo,by="city_id_simplemaps") %>% 
  
  #link geonames stuff
  left_join(lookup_gub_geoname_id,by="ORIG_FID") %>% #geoname_id
  left_join(lookup_gub_geonames_miss,by="ORIG_FID") %>% 
  
  left_join(lookup_all_vars_geonames_nogeo,by="geoname_id") %>% 
  
  #add country name based on the vector data
  left_join(lookup_orig_fid_country_name_en,by="ORIG_FID") %>% 
  
  #Per Ana's book (Chapter 2), it can be important to classify cities by size
  #Some popular categories (figure 2.4, urban public health)
  #: fewer than 300k, 300k to 500k, 500k to 1 million, 1 to 5 million
  #The atlas of urban expansion also classifies cities (table 2.1) as
  #100k to 427k
  #427k to 1.57 m
  #5.715 m
  #5.714 m plus
  #Decisions for how to classify population - either based on the city_pop
  #from the other city-specific dataset or via a summed total of landscan
  #within the GUB. I think the more interpretable result will be via
  #the city-specific numbers, even if it doesn't correspond to the total GUB
  mutate(
    
    #a city name for either source
    city_name_either_source=case_when(
      is.na(city_name_simplemaps)==T~trimws(city_name_geonames),
      TRUE ~trimws(city_name_simplemaps)
    ),
    
    #both sources are missing
    city_name_miss_both_sources=case_when(
      is.na(city_name_either_source)==T~1,
      TRUE~0),
    
    #if city name is missing either source, then include the orig_fid
    city_name_either_source_force_not_miss=case_when(
      city_name_miss_both_sources==1~paste0("GUB ",as.character(ORIG_FID)),
      TRUE~city_name_either_source
    ),
    
    #add city name, country name for vis
    city_name_country_name=paste0(
      city_name_either_source_force_not_miss,", ",
      country_name_en),
    
    #and a version with an admin code as well
    city_name_admin_code_country_name=paste0(
      city_name_either_source_force_not_miss,", ",
      admin_code_simplemaps,", ",
      country_name_en),
    
    city_population_either_source=case_when(
      is.na(city_population_geonames)==F~city_population_geonames,
      is.na(city_population_geonames)==T~city_population_simplemaps
    ),
    
    #The first classification system is from Figure 2.4, Urban Public Health
    #and corresponds to specific city boundaries
    pop_cat_breaks_city_either_source=cut(city_population_either_source, 
                     breaks=c(0,300000,500000,1000000,5000000,10000000, 25000000)),
    
    #The second corresponds to the Atlas of Urban Expansion and we therefore use
    #the landscan population values. let's go with mean.
    #aue for atlas of urban expansion; ls for landscan
    pop_cat_breaks_gub_aue_ls=cut(
                pop_cat_mean_val_gub,
                breaks=c(0,100000,427000,1570000,5715000,
                                    300000000),#last category is 300,000,000 - max mean value
                include.lowest = TRUE,
                dig.lab = 9#avoid scientific notation
                           )
    )

setwd(here("data-processed"))
save(lookup_gub_several_vars,file="lookup_gub_several_vars.RData")
names(lookup_gub_several_vars)
#Check on pop var
lookup_gub_several_vars %>% 
  dplyr::select(ORIG_FID, starts_with("city_popul")) %>% 
  slice(1:1000) 

lookup_gub_several_vars %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n())

# lookup_gub_several_vars %>% 
#   filter(city_name_either_source=="Bazhou") %>% 
#   View()

names(lookup_gub_several_vars)
# lookup_gub_several_vars %>% 
#   distinct(country_name_en) %>% 
#   View()
table(lookup_gub_several_vars$country_name_en)

#how many GUBs are missing city names from both sources?

lookup_gub_several_vars %>% 
  group_by(city_name_miss_both_sources) %>% 
  summarise( n=n()) %>% 
  ungroup() %>% 
  mutate(
    n_total=sum(n),
    prop=n/n_total)

#Oct 9 2023
#it looks like I still need a lookup containing area and pop of GUB
#This is more pared down than the earlier version of this lookup - no pop. information
#from the city-level sources
lookup_gub_pop_area=lookup_gub_several_vars %>% 
  dplyr::select(ORIG_FID,
                starts_with("pop_cat_mean_val_gub"),
                starts_with("pop_cat_min_val_gub"),
                starts_with("pop_cat_max_val_gub")) %>% 
  left_join(lookup_gub_area_km2, by ="ORIG_FID")

lookup_gub_pop_area
setwd(here("data-processed"))
save(lookup_gub_pop_area, file = "lookup_gub_pop_area.RData")
### duplicate city names---------

city_name_country_name_dupe=lookup_gub_several_vars %>% 
  group_by(city_name_country_name) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>1) %>% #greater than 1! not gt 2
  mutate(
    city_name_country_name_dupe=1
  )
nrow(city_name_country_name_dupe)
city_name_country_name_dupe

#okay, for presentation purposes, add this back into the main gub lookup table

lookup_gub_several_vars_wrangle=lookup_gub_several_vars %>% 
  left_join(city_name_country_name_dupe,by="city_name_country_name") %>% 
  #a version that includes the admin only if it's a duplicate city-name-country name
  mutate(
    city_name_country_name_admin_if_dupe=case_when(
      #if duplicate city name
      city_name_country_name_dupe==1~paste0(
          city_name_either_source_force_not_miss,
                        ", ",
          admin_code_simplemaps,
                        ", ",
        country_name_en),
      
      #if not duplicate city name, just
      #use city_name_country_name
      TRUE ~city_name_country_name
      
    )
  )

setwd(here("data-processed"))
save(lookup_gub_several_vars_wrangle, file = "lookup_gub_several_vars_wrangle.RData")

#I need a look-up that excludes area_km2 and pop_cat_min_val, as these
#will be recreated when the hia_summarise() function is run
lookup_gub_city_info=lookup_gub_several_vars_wrangle %>% 
  dplyr::select(-starts_with("area_km2"),
                -starts_with("pop_cat_min_val_gub"),
                -starts_with("pop_cat_mean_val_gub"),
                -starts_with("pop_cat_max_val_gub"))

names(lookup_gub_city_info)
setwd(here("data-processed"))
save(lookup_gub_city_info, file = "lookup_gub_city_info.RData")

#check
lookup_gub_several_vars_wrangle %>% 
  dplyr::arrange(city_name_country_name_dupe,city_name_country_name) %>% 
  dplyr::select(ORIG_FID, starts_with("city_name")) %>% 
  slice(1:5000) 

#check some that still seem to be appearing as dupes in the figures
# lookup_gub_several_vars_wrangle %>% 
#   filter(city_name_either_source=="Bazhou") %>% 
#   View()







#Examine city population
lookup_gub_several_vars_wrangle %>% 
  arrange(desc(pop_cat_mean_val_gub))


#How does city_pop generally plot against landsat values
names(lookup_gub_several_vars_wrangle)
lookup_gub_several_vars_wrangle %>% 
  filter(is.na(city_population_either_source)==F) %>% 
  filter(pop_cat_mean_val_gub>100000) %>% 
  ggplot(aes(x=city_population_either_source,y= pop_cat_min_val_gub))+
  geom_point()

#the issue with strictly using city_population is that there are some GUBs that
#don't have a corresponding city
lookup_gub_several_vars_wrangle %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  left_join(lookup_gub_area_km2, by ="ORIG_FID") %>% 
  filter(area_km2_gub >5) %>% 
  group_by(pop_cat_breaks_city_either_source) %>% 
  summarise(n=n())

#This looks pretty good. We can go with this.

lookup_gub_several_vars_wrangle %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  left_join(lookup_gub_area_km2, by ="ORIG_FID") %>% 
  filter(area_km2_gub >5) %>% 
  group_by(pop_cat_breaks_gub_aue_ls) %>% 
  summarise(n=n())

#a lookup just for the population categories I just created
lookup_city_gub_pop_cat_breaks=lookup_gub_several_vars_wrangle %>% 
  dplyr::select(ORIG_FID,starts_with("pop_cat_breaks"))

setwd(here("data-processed"))
save(lookup_city_gub_pop_cat_breaks,file="lookup_city_gub_pop_cat_breaks.RData")
  

## lookup for city-biome-----------

### Explore cities with missing biomes------
#Feb 21, 2023:
#Generate a city-biome look-up table. Note there may be one to many in some
#cases, so count the number of pixels. Like this:
#This is in response to the concern of pixels with missing biome (below)
#changing this from lookup_city_biome to lookup_gub_biome to differentiate the two
lookup_gub_biome = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, BIOME_NAME) %>% 
  summarise(n_pixel_city_biome = n()) %>% 
  ungroup()

lookup_gub_biome %>% 
  print(n=100)

#cool. now get a 1-1 lookup corresponding to the most common biome within city
lookup_gub_biome_top  = lookup_gub_biome %>% 
  arrange(ORIG_FID, desc(n_pixel_city_biome)) %>% #sort descending by this
  group_by(ORIG_FID) %>% 
  slice(1) %>%   #and take top row per group
  ungroup() %>% 
  rename(biome_name_top = BIOME_NAME)

lookup_gub_biome_top   %>% 
  print(n=100)

#save in case I use in other code
setwd(here("data-processed"))
save(lookup_gub_biome_top, file = "lookup_gub_biome_top.RData")
#There are apparently some cities that don't have a biome under this method.
lookup_gub_biome_top  %>% 
  filter(is.na(biome_name_top)==TRUE) %>% 
  nrow()
#325 of 64694
325/64694
nrow(lookup_gub_biome_top)
load("lookup_gub_orig_fid_geo.RData")
lookup_gub_biome_top  %>% 
  filter(is.na(biome_name_top)==TRUE) %>% 
  left_join(lookup_gub_orig_fid_geo, by = "ORIG_FID") %>% 
  st_as_sf() %>% 
  mapview() #looks like mostly islands and coastal areas.

##lookup for Population category------
#I want to add a comma to the upper bound, especially, so
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_max_val)
table(pop_ndvi_gub_biome_tib_gub_not_miss$pop_cat_max_fac)
lookup_pop_cat_max_fac=pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  distinct(pop_cat_max_val,pop_cat_max_fac) %>% 
  mutate(
    pop_cat_max_fac_w_comma_int=as.factor(prettyNum(pop_cat_max_val, big.mark=",", trim=T)),
    #this step orders it by values of the double version of the pop_cat_max_val. nice
    #https://forcats.tidyverse.org/reference/fct_reorder.html
    pop_cat_max_fac_w_comma=fct_reorder(pop_cat_max_fac_w_comma_int,pop_cat_max_val)
  ) %>% 
  #Now I can drop pop_cat_max_val and the intermediate factor 
  dplyr::select(-pop_cat_max_val,-pop_cat_max_fac_w_comma_int) %>% 
  arrange(pop_cat_max_fac_w_comma)
    
lookup_pop_cat_max_fac
#Going to save this for use elsewhere
setwd(here("data-processed"))
save(lookup_pop_cat_max_fac, 
     file = "lookup_pop_cat_max_fac.RData")

lookup_pop_cat_max_fac

# Main analysis steps----
#July 18, 2024: Here trying to track down why many countries don't have
#baseline mortality data
#Check on Palestine, Taiwan, Hong Kong, and Puerto Rico.
#The reason they don't have data is because they're not in WHO 2019 mortality data.

names(countries_joined_with_un_pop_deaths_pared_nogeo)
names(lookup_gub_several_vars_wrangle)
names(pop_ndvi_gub_biome_tib_gub_not_miss)#note we don't have a measure of area
#can just assume it's one, though?
load("countries_joined_with_un_pop_deaths_pared_nogeo.RData")
names(countries_joined_with_un_pop_deaths_pared_nogeo)
table(countries_joined_with_un_pop_deaths_pared_nogeo$who_data_missing_country)
pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  group_by(country_name_en) %>% 
  summarise(n=n())
source(here("scripts", "rojas_green_space_drf.R")) #load dose-response info if needed
pop_ndvi_gub_biome_tib = pop_ndvi_gub_biome_tib_gub_not_miss %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  left_join(countries_joined_with_un_pop_deaths_pared_nogeo, by = "country_name_en") %>% 
  #July 18, 2024: limit to countries with non-missing WHO mortality data
  filter(who_data_missing_country==0) %>% 
  
  bind_cols(rr_ac) %>%   #add DRF to every row (regardless of country)
  #this adds the RR corresponding to non-accidental deaths
  #as well as the NDVI increment
  #add number of deaths to every row (would be a left_join for more countries)
  bind_cols(rr_na) %>% 
  
  mutate(pixel_id = row_number()) %>% #row number for a given pixel
  mutate(area_km2_pixel=1) %>% #assume it's one I suppose Oct 10, 2023
  

  #link in the city-biome lookup so that I can impute biomes
  #for pixels with missing biomes
  left_join(lookup_gub_biome_top, by = "ORIG_FID") %>% 
  #Feb 21 2023: now, if a given pixel is missing a biome, set it to the value
  #of the most common biome for that city
  
  #Nov 19, 2023: add this weight on the LandScan population
  left_join(
    lookup_country_name_en_ratio_pop_un_v_pop_cat_max_ls,
            by="country_name_en") %>% 
  mutate(
    #imp for imputed (if necessary)
    biome_name_imp=case_when(
      is.na(BIOME_NAME==TRUE) ~biome_name_top,
      TRUE ~BIOME_NAME),
    
    #recalculate the mean based on this weight and make
    #changes in the analysis function accordingly
    #Note from Jan 17, 2024: this uses UN population data. That's good.
    pop_cat_max_val_adj_int=pop_cat_max_val*ratio_pop_un_v_pop_cat_max_ls
  ) %>% 
  #July 22, 2024: there are a few instances where this takes the max
  #below the min. I need to change that so that downstream calculations make sense.
  rowwise() %>% 
  mutate(
    pop_cat_min_val_adj=min(pop_cat_min_val,pop_cat_max_val_adj_int),
    pop_cat_max_val_adj=max(pop_cat_min_val,pop_cat_max_val_adj_int),
  ) %>% 
  ungroup() %>% 
  mutate(
    #Now the mean can be adjusted based on the mean of those two values
    pop_cat_mean_val_adj=(pop_cat_max_val_adj+pop_cat_min_val_adj)/2
  ) %>% 
  #link area and population of the global urban boundary.
  #only include gubs above 5 square km and above 1,000 residents (min per landscan)
  left_join(lookup_gub_pop_area, by ="ORIG_FID") %>% 
  filter(pop_cat_min_val_gub>1000) %>% #using minimum to be conservative (err on including)
  filter(area_km2_gub >5) %>% #Oct 10 - updated with suffix
  #add a country group here first so I can summarize by country. this order is good.
  group_by(country_name_en, biome_name_imp, ORIG_FID, pop_cat_1_8) %>%
  mutate_steps_hia_ndvi_pop() %>% #see ~analysis-functions.R
  ungroup()

#Done! 
#Updated July 25, 2024
setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib, file = "pop_ndvi_gub_biome_tib.RData")
#load("pop_ndvi_gub_biome_tib.RData")
pop_ndvi_gub_biome_tib
n_distinct(pop_ndvi_gub_biome_tib$ORIG_FID)#15960
n_distinct(pop_ndvi_gub_biome_tib$country_name_en)
n_distinct(pop_ndvi_gub_biome_tib$country_name_en_original)
table(pop_ndvi_gub_biome_tib$who_data_missing_country)

#how many GUBs if we exclude top tertile, though?
#see hia-summary for final count. it should be 15917

### Checks-----
#how do the filters affect the number of rows?
#July 22, 2024: does my adjusted min work better?
# pop_ndvi_gub_biome_tib %>% 
#   dplyr::select(contains("pop_cat_min_val"),contains("pop_cat_max_val")) %>% 
#   View()
#     
# pop_ndvi_gub_biome_tib %>% 
#   group_by(country_name_en) %>% 
#   summarise(n=n()) %>% 
#   View()

## look-up for adjusted pop values at GUB level------
#I want a look-up that corresponds to the adjusted pop values at the GUB level
lookup_gub_pop_cat_val_adj=pop_ndvi_gub_biome_tib %>% 
  group_by(ORIG_FID) %>% 
  summarise(
    #have to name them something different so they can be joined
    #without duplicating column names
    pop_cat_mean_val_adj_gub = sum(pop_cat_mean_val_adj,na.rm=TRUE),
    pop_cat_min_val_adj_gub = sum(pop_cat_min_val_adj,na.rm=TRUE),
    pop_cat_max_val_adj_gub = sum(pop_cat_max_val_adj,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pop_cat_breaks_adj_gub_aue_ls=cut(
      pop_cat_mean_val_adj_gub,
      breaks=c(0,100000,427000,1570000,5715000,
               round(max(pop_cat_mean_val_adj_gub))),#the maximum pop GUB
      include.lowest = TRUE,
      dig.lab = 9#avoid scientific notation
    )
  )



max(lookup_gub_pop_cat_val_adj$pop_cat_mean_val_adj_gub)
setwd(here("data-processed"))
save(lookup_gub_pop_cat_val_adj, file = "lookup_gub_pop_cat_val_adj.RData")
lookup_gub_pop_cat_val_adj

lookup_gub_pop_cat_val_adj
table(lookup_gub_pop_cat_val_adj$pop_cat_breaks_adj_gub_aue_ls)

#July 18, 2024: after the filter excluding countries with missing WHO mortality data:
nrow(pop_ndvi_gub_biome_tib)
names(pop_ndvi_gub_biome_tib)
object.size(pop_ndvi_gub_biome_tib) 
table(pop_ndvi_gub_biome_tib$pop_cat_max_fac)
table(pop_ndvi_gub_biome_tib$country_name_en)
n_distinct(pop_ndvi_gub_biome_tib$country_name_en)
n_distinct(pop_ndvi_gub_biome_tib$ORIG_FID)
n_distinct(pop_ndvi_gub_biome_tib$biome_name_imp)

#what are the countries with data?
pop_ndvi_gub_biome_tib %>% 
  group_by(country_name_en) %>% 
  summarise(n=n()) %>% 
  print(n=200)


#How many cities have more than one biome?
n_biome_within_gub = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, BIOME_NAME) %>% 
  summarise(n_pixels_biome=n()) %>% 
  group_by(ORIG_FID) %>% 
  summarise(n_biome = n_distinct(BIOME_NAME)) %>% 
  ungroup()

n_biome_within_gub
summary(n_biome_within_gub$n_biome)

n_gub_w_2_or_more_biomes = n_biome_within_gub %>% 
  filter(n_biome>1) %>% 
  nrow()

n_gub = n_biome_within_gub %>% nrow()

n_gub_w_2_or_more_biomes/n_gub
1-(n_gub_w_2_or_more_biomes/n_gub)

#Describe variation of pop. density within GUB?
n_pop_cat_gub = pop_ndvi_gub_biome_tib %>% 
  filter(is.na(pop_cat_1_8)==FALSE) %>% #NAs throwing error 
  filter(ndvi_2019>0) %>% #exclude NDVI below 0 (water)
  group_by(ORIG_FID, pop_cat_1_8) %>% 
  summarise(n_pixels_pop_cat=n()) %>% 
  group_by(ORIG_FID) %>% 
  summarise(n_pop_cat = n_distinct(pop_cat_1_8))

n_pop_cat_gub
summary(n_pop_cat_gub$n_pop_cat)

# check on pt vs ll vs ul variables
names(pop_ndvi_gub_biome_tib)
# pop_ndvi_gub_biome_tib %>% 
#   dplyr::select(ORIG_FID, starts_with("paf"), starts_with("n_d_ac_prev_m")) %>% 
#   slice(1:500) %>% 
#   rowwise() %>% 
#   #what is the min and max of the 9 attrib deaths var?
#   mutate(
#     n_d_ac_prev_min_across_vars = min(
#       n_d_ac_prev_mean_pt, n_d_ac_prev_min_pt, n_d_ac_prev_max_pt,
#       n_d_ac_prev_mean_ll, n_d_ac_prev_min_ll, n_d_ac_prev_max_ll,
#       n_d_ac_prev_mean_ul, n_d_ac_prev_min_ul, n_d_ac_prev_max_ul, na.rm=TRUE)
#     ,
#   n_d_ac_prev_max_across_vars = max(
#     n_d_ac_prev_mean_pt, n_d_ac_prev_min_pt, n_d_ac_prev_max_pt,
#     n_d_ac_prev_mean_ll, n_d_ac_prev_min_ll, n_d_ac_prev_max_ll,
#     n_d_ac_prev_mean_ul, n_d_ac_prev_min_ul, n_d_ac_prev_max_ul, na.rm=TRUE)
#   ) %>% 
#   View()

      #okay, n_d_ac_prev_min_ul is the overall min.
      #That makes sense because _ul corresponds to the bound closer to 1 in the risk ratio
      #That means n_d_ac_prev_max_ll should correspond to the overall max. Yes.

#check the factor version of the scaled population


#End of code here
#For the summaries, now look here: ~summary-global.R

# For the final posted dataset, remove a few variables-----
names(pop_ndvi_gub_biome_tib)
#remove anything pertaining to urban and GBD
pop_ndvi_gub_biome_tib_public=pop_ndvi_gub_biome_tib %>% 
  dplyr::select(-contains("urban")) %>% 
  dplyr::select(-contains("gbd")) 

setwd(here("data-processed"))
save(pop_ndvi_gub_biome_tib_public, file = "pop_ndvi_gub_biome_tib_public.RData")


ncol(pop_ndvi_gub_biome_tib)
ncol(pop_ndvi_gub_biome_tib_public)
names(pop_ndvi_gub_biome_tib_public)


