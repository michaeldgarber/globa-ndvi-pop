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

# Figure 2:

