#supplement

#July 24, 2024
#So it's easier to find, I'm making a separate script for supplemental material.
#This should be run first if not already done
library(here)
source(here("scripts", "summary-global.R")) #HIA

# Map of biomes------
#That one is done here
source(here("scripts", "summary-global-maps.R")) #HIA


# Histogram of number of distinct population categories in urban areas-----
#also in the summary-global script
hia_summary_gub %>% 
  ggplot(aes(n_distinct_pop_cat_not_scaled))+
  geom_histogram(bins=9)+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks=seq(1,9,1))+
  ylab("Number of\n urban areas")+
  xlab("Number of distinct\npopulation categories\n among constituent pixels")+
  theme_bw(base_size = 12)

setwd(here("plots"))
ggsave("plot_pop_cat_urban_areas_hist.png", 
       height=5, width=5)


#NDVI distribution within pop dens and biome------
14/3

#a function without an x-axis label
pop_ndvi_boxplot_fun_no_varwidth_no_xlab = function(df){
  df %>% 
    filter(is.na(biome_name_imp)==FALSE) %>% 
    ggplot(aes(x=pop_cat_max_fac_w_comma, y=ndvi_2019))+
    geom_boxplot(
      outlier.size =  .01,
      outlier.colour = "gray"
    )+
    #add mean - see Evernote for notes
    stat_summary(fun=mean, geom="point", shape=23, size=2)+
    ylab("NDVI")+
    xlab("")
}

#Let's do  5, 5, 5
## First 5------
#July 24, 2024: moving this to supplement

biome_number = pop_ndvi_gub_biome_tib %>% 
  group_by(biome_name_imp) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(biome_number=row_number()) %>% 
  dplyr::select(-n)

biome_first_7 = biome_number %>% 
  filter(biome_number<=7)
biome_last_7=biome_number %>% 
  filter(biome_number>7)


pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(biome_number<=5) %>% 
  pop_ndvi_boxplot_fun_no_varwidth_no_xlab()+
  facet_grid(
    #use labeler to wrap text. brilliant.
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 10)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90))


#save with uniform dimensions
#Note this is going into a panel, so keep height rather short
setwd(here("plots"))
ggsave("plot_ndvi_x_biome_x pop_dens_first_5.png", 
       height=4, width=8)

## Second 5----
pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(biome_number>5) %>% 
  filter(biome_number<=10) %>% 
  pop_ndvi_boxplot_fun_no_varwidth_no_xlab()+
  facet_grid(
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 10)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90))

setwd(here("plots"))
ggsave("plot_ndvi_x_biome_x pop_dens_second_5.png", 
       height=4, width=8)

## Last 4------
pop_ndvi_gub_biome_tib %>% 
  left_join(biome_number, by = "biome_name_imp") %>% 
  left_join(lookup_pop_cat_max_fac, by = "pop_cat_max_fac") %>% 
  filter(biome_number>10) %>% 
  pop_ndvi_boxplot_fun_no_varwidth()+
  facet_grid(
    labeller = labeller(biome_name_imp = label_wrap_gen(width = 10)),
    cols = vars(biome_name_imp))+
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90))

setwd(here("plots"))
ggsave("plot_ndvi_x_biome_x pop_dens_last_4.png", 
       height=5, width=6)


# Imputation-related tables and figures---------
#The tables and figures are here:
source(here("scripts", "read-united-nations-gbd-data.R")) #HIA

# On the missingness of the GUBs-------


