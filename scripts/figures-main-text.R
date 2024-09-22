#Figures-final-text

#Only the figures in the final text of the main manuscript file
#Code largely copied from
#global-ndvi-pop/scripts/summary-global.R

#Sep 20, 2024

#Packages needed
library(here)
library(tidyverse)
library(scales)

# Data needed
#On my computer, the subfolder is called "data-processed".
#This code will need to be adapted to a particular computer's folder structure.
setwd(here("data-processed"))
load("pop_ndvi_gub_biome_tib_public.RData") #main pixel-level result dataset
load("lookup_pop_cat_max_fac.RData")#load this look-up table
nrow(pop_ndvi_gub_biome_tib_public)
ncol(pop_ndvi_gub_biome_tib_public)
names(pop_ndvi_gub_biome_tib_public)

#Figure 1: distribution of NDVI by density, biome, income cat--------


## NDVI x pop. density overall-----

### NDVI x pop density
lookup_pop_cat_max_fac
names(pop_ndvi_gub_biome_tib_public)
plot_ndvi_x_pop_pop_cat_max_fac=pop_ndvi_gub_biome_tib_public %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  ggplot(aes(x=pop_cat_max_fac_w_comma, y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab(
    "Population density\n (people per square kilometer,\nunscaled upper bound of LandScan category)"
  )+ 
  ylab("NDVI")+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=30, hjust=1))

plot_ndvi_x_pop_pop_cat_max_fac

setwd(here("plots"))
ggsave("plot_ndvi_x_pop_pop_cat_max_fac.png", height=2.5, width=8)


## NDVI x biome--------
#imp for imputed. see analysis-global.
table(pop_ndvi_gub_biome_tib_public$biome_name_imp)

plot_ndvi_x_biome_name_imp= pop_ndvi_gub_biome_tib_public %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  #Oct 5, 2023: Note I'm reordering biome by its NDVI level. Not sure how ggplot
  #figured out how to use the mean, but it did...
  #https://stackoverflow.com/questions/40199274/
  ggplot(aes(x=fct_reorder(biome_name_imp, ndvi_2019, .desc=TRUE), y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab(
    "Biome"
  )+ 
  ylab("NDVI")+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=35))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=40, hjust=1))

plot_ndvi_x_biome_name_imp

#save with uniform dimensions
#Note this is going into a panel, so keep height rather short
setwd(here("plots"))
ggsave("plot_ndvi_x_biome_name_imp.png", height=3.5, width=8)


## NDVI x income group-------
setwd(here("data-processed"))
load("lookup_income_grp.RData")
lookup_income_grp
plot_ndvi_x_income_grp=pop_ndvi_gub_biome_tib_public %>% 
  left_join(lookup_income_grp, by = c("country_name_en"="name_en")) %>% 
  filter(is.na(biome_name_imp)==FALSE) %>% 
  filter(is.na(income_grp)==F) %>% 
  #Oct 5, 2023: Note I'm reordering biome by its NDVI level. Not sure how ggplot
  #figured out how to use the mean, but it did...
  #https://stackoverflow.com/questions/40199274/
  ggplot(aes(x=income_grp_5_rename, y=ndvi_2019))+
  geom_boxplot(
    outlier.size =  .01,
    outlier.colour = "gray",
    varwidth=TRUE,
  )+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  xlab(
    "Income group"
  )+ 
  ylab("NDVI")+
  #This wraps the x-axis labels. Very useful. See
  #https://stackoverflow.com/questions/61782882/
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=10))+
  theme_bw(base_size=12)

plot_ndvi_x_income_grp
setwd(here("plots"))#note it takes the last plot.
ggsave("plot_ndvi_x_income_grp.png", height=2.5, width=8)

# Figures of results at the GUB level--------
#Note we cannot share all of the data needed to re-create these figures, so the code
#is not included here.
#Code to create these figures can be found here:
#global-ndvi-pop/scripts/summary-global.R

#Note we do not have permission to redistribute data corresponding to some city names,
#so these data cannot be reproduced.

names(pop_ndvi_gub_biome_tib_public)
#global-ndvi-pop/scripts/summary-global.R

#stratified by city size

# Figures of results at the country level------
#Try to make the figures a bit smaller so they can all fit on one page.
results_country = pop_ndvi_gub_biome_tib_public %>% 
  filter(ndvi_tertile<3) %>%   #exclude top NDVI tertile throughout  
  group_by(country_name_en) %>% 
  hia_summarise_who_only() %>% 
  ungroup() %>% 
  #make country a factor ranked byn_d_na_prev_std_who_per_100k_pop_pt
  mutate(
    country_name_ranked_by_n_d_na_prev_per_pop_int=as.factor(country_name_en),
    country_name_ranked_by_n_d_na_prev_per_pop=fct_reorder(
      country_name_ranked_by_n_d_na_prev_per_pop_int,
      n_d_na_prev_std_who_per_100k_pop_pt,
      .na_rm=TRUE)
  ) %>% 
  #I might want these to come along
  left_join(lookup_income_grp, by = c("country_name_en"="name_en"))  

#Separate figures by income level
income_grp_5_rename_levels =results_country %>% 
  group_by(income_grp_5_rename) %>% 
  summarise(n=n()) %>% 
  ungroup()

income_grp_5_rename_levels
income_grp_5_rename_levels$income_grp_5_rename[1]

results_country

# write a function to reduce copy/paste
#Function without labels
summary(results_country$n_d_na_prev_std_who_per_100k_pop_max_over_9)
ggplot_top_n_country_by_n_death_prev_per_100k_no_labs=function(df){
  df %>% 
    filter(is.na(country_name_en)==F) %>% 
    filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
    ggplot(aes(x=n_d_na_prev_std_who_per_100k_pop_pt,
               y=country_name_ranked_by_n_d_na_prev_per_pop))+
    #Having trouble getting point size to reduce. This works...Sep 21, 2024
    geom_point(stroke=0,size=.5,color="transparent")+
    geom_pointrange(aes(xmin=n_d_na_prev_std_who_per_100k_pop_min_over_9,
                        xmax=n_d_na_prev_std_who_per_100k_pop_max_over_9))+
    scale_x_continuous(limits=c(0,175))+
#    scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=10))+
    labs(
      x="Age-standardized\nnon-accidental death rate\n prevented\nper 100,000 population\n(adults 30+)",
      y="")+
    theme_bw(base_size = 12)
}
table(results_country$income_grp_5_rename)
plot_n_d_na_prev_std_per_100k_x_country_inc_grp_1=results_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[1]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k_no_labs()

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_1

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_1
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_inc_grp_1.png", height=10, width=5)

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_2=results_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[2]) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k_no_labs()

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_2
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_inc_grp_2.png", height=10, width=5)

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_3=results_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[3]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k_no_labs()

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_3
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_inc_grp_3.png", height=10, width=5)

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_4=results_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[4]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k_no_labs()

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_4
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_inc_grp_4.png", height=10, width=5)

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_5=results_country %>% 
  filter(income_grp_5_rename==income_grp_5_rename_levels$income_grp_5_rename[5]) %>% 
  filter(is.na(country_name_en)==F) %>% 
  filter(is.na(n_d_na_prev_std_who_per_100k_pop_pt)==F) %>% 
  arrange(desc(n_d_na_prev_std_who_per_100k_pop_pt)) %>% 
  ggplot_top_n_country_by_n_death_prev_per_100k_no_labs()

plot_n_d_na_prev_std_per_100k_x_country_inc_grp_5
setwd(here("plots"))
ggsave("plot_n_d_na_prev_std_per_100k_x_country_inc_grp_5.png", height=10, width=5)