#Analysis functions
# Created as its own script Feb 21, 2023

# Mutate steps for HIA-------
#after the data has already been grouped
mutate_steps_hia_ndvi_pop = function(df){
  df %>% 
    mutate( #use group_by() %>% mutate() rather than group_by() %>% summarise()
      #I'm getting strange things with negative values. Maybe create categories manually
      #rather than using cut_interval.
      ndvi_33=quantile(ndvi_2019, probs=0.333333, na.rm=TRUE),
      ndvi_66=quantile(ndvi_2019, probs=0.666666, na.rm=TRUE),
      #The median of the top tertile is by definition 83.3333, right? 5/6. Yes.
      ndvi_83=quantile(ndvi_2019, probs=0.833333, na.rm=TRUE),
      ndvi_tertile = case_when(
        ndvi_2019 <= ndvi_33 ~1,
        ndvi_2019 > ndvi_66 ~3,
        ndvi_2019 > ndvi_33 & ndvi_2019 <=ndvi_66 ~2),
      #Now calculate the difference in NDVI, but only for the bottom two tertiles:
      #scenario: raise all pixels to the median NDVI value of the top tertile
      ndvi_diff = case_when(
        ndvi_tertile <3 ~       ndvi_83-ndvi_2019,
        ndvi_tertile==3 ~ NA_real_),
      
      #Now I can simply do the HIA. For now just use the mean of
      #the population category as a population estimate.
      #Modified from the HIA code in green-space-denver
      #Note we should scale the population first, as I think the
      #land-scan data includes everyone, not just 20+, so we could get a proportion 20+
      #Feb 15 2023 adding min/max as well
      pop_cat_mean_val_scaled = pop_cat_mean_val*pop_ratio_20_plus,
      pop_cat_min_val_scaled = pop_cat_min_val*pop_ratio_20_plus,
      pop_cat_max_val_scaled = pop_cat_max_val*pop_ratio_20_plus,
      
      rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      paf =(rr_alt -1)/rr_alt , #pop_est attrib fraction
      #estimate number of deaths first for summary purposes
      deaths_baseline_mean = death_rate_20_plus*pop_cat_mean_val_scaled,
      deaths_baseline_min = death_rate_20_plus*pop_cat_min_val_scaled,
      deaths_baseline_max = death_rate_20_plus*pop_cat_max_val_scaled,
      
      attrib_d_mean = paf*deaths_baseline_mean, 
      attrib_d_min = paf*deaths_baseline_min,
      attrib_d_max = paf*deaths_baseline_max,
      
      deaths_prevented_mean = attrib_d_mean*-1,
      deaths_prevented_min = attrib_d_min*-1,
      deaths_prevented_max = attrib_d_max*-1,
      
      deaths_prevented_per_pop_mean = deaths_prevented_mean/pop_cat_mean_val_scaled,
      deaths_prevented_per_pop_min = deaths_prevented_min/pop_cat_mean_val_scaled,
      deaths_prevented_per_pop_max = deaths_prevented_max/pop_cat_mean_val_scaled
    )
}


#Summarize the HIA results-----
hia_summarise = function(df){
  df %>% 
    summarise(
      pop_cat_mean_val_scaled = sum(pop_cat_mean_val_scaled,na.rm=TRUE),
      pop_cat_min_val_scaled = sum(pop_cat_min_val_scaled,na.rm=TRUE),
      pop_cat_max_val_scaled = sum(pop_cat_max_val_scaled,na.rm=TRUE),
      
      deaths_baseline_mean = sum(deaths_baseline_mean, na.rm=TRUE),
      deaths_baseline_min = sum(deaths_baseline_min, na.rm=TRUE),
      deaths_baseline_max = sum(deaths_baseline_max, na.rm=TRUE),
      
      deaths_prevented_mean = sum(deaths_prevented_mean, na.rm=TRUE),
      deaths_prevented_min = sum(deaths_prevented_min, na.rm=TRUE),
      deaths_prevented_max = sum(deaths_prevented_max, na.rm=TRUE),
      
      ndvi_2019_mean = mean(ndvi_2019, na.rm=TRUE),
      ndvi_2019_sd = sd(ndvi_2019, na.rm=TRUE),
      ndvi_diff_mean = mean(ndvi_diff, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      deaths_prevented_per_1k_pop_mean = (
        deaths_prevented_mean/pop_cat_mean_val_scaled)* 1000 ,
  
      deaths_prevented_per_1k_pop_min = (
        deaths_prevented_min/pop_cat_min_val_scaled)* 1000 ,

      deaths_prevented_per_1k_pop_max = (
        deaths_prevented_max/pop_cat_max_val_scaled)* 1000
    )
}
