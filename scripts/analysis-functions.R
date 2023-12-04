#Analysis functions
# Created as its own script Feb 21, 2023

# Wrangle landscan population data-------
#January 25th, 2022: Since I'm using the same steps for the USA,
#I'm going to make a function
#Feb 9, 2023: out of curiosity, will this switch between tidyterra and dplyr 
#without me writing tidyterra::rename? The answer is yes. Very cool.
#This will allow me to use the same function for the global analysis


landscan_pop_wrangle = function(df){
  df %>% 
    #rename the layers.
    rename(
      rgb_1 = `landscan-global-2019-colorized_1` ,
      rgb_2 = `landscan-global-2019-colorized_2`,
      rgb_3 = `landscan-global-2019-colorized_3`,
      rgb_4 = `landscan-global-2019-colorized_4`) %>% 
    #now try to create the categories following the above scheme
    mutate(
      #   1 - 5: light yellow         rgb(255,255,190)
      # 6 - 25: medium yellow         rgb(255,255,115)
      # 26 - 50: yellow               rgb(255,255,0)
      # 51 - 100: orange              rgb(255,170,0)
      # 101 - 500: orange-red         rgb(255,102,0)
      # 501 - 2500: red               rgb(255,0,0)
      # 2501 - 5000: dark red         rgb(204,0,0)
      # 5001 - 185000: maroon         rgb(115,0,0)
      #The actual value
      pop_cat_max_val = case_when(
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 190 ~5,
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 115 ~25,
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 0 ~50,
        rgb_1==255 & rgb_2 == 170 & rgb_3 == 0 ~100,
        rgb_1==255 & rgb_2 == 102 & rgb_3 == 0 ~500,
        rgb_1==255 & rgb_2 == 0 & rgb_3 == 0 ~2500,
        rgb_1==204 & rgb_2 == 0 & rgb_3 == 0 ~5000,
        rgb_1==115 & rgb_2 == 0 & rgb_3 == 0 ~185000,
        TRUE ~ 0
      ),
      pop_cat_min_val = case_when(
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 190 ~1,
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 115 ~6,
        rgb_1==255 & rgb_2 == 255 & rgb_3 == 0 ~26,
        rgb_1==255 & rgb_2 == 170 & rgb_3 == 0 ~51,
        rgb_1==255 & rgb_2 == 102 & rgb_3 == 0 ~101,
        rgb_1==255 & rgb_2 == 0 & rgb_3 == 0 ~501,
        rgb_1==204 & rgb_2 == 0 & rgb_3 == 0 ~2501,
        rgb_1==115 & rgb_2 == 0 & rgb_3 == 0 ~5001,
        TRUE ~ 0
      ),
      
      #The simple mean of the two ((min+max)/2)for point estimates
      pop_cat_mean_val = case_when(
        is.na(pop_cat_min_val)==TRUE ~ NA_real_,
        TRUE ~ (pop_cat_max_val+pop_cat_min_val)/2),
      
      #factor categories to facilitate some of the visualizations
      pop_cat_max_fac = as.factor(pop_cat_max_val),
      pop_cat_mean_fac = as.factor(pop_cat_mean_val),

      #a simple numeric for the 8 categories: - 1-8
      pop_cat_1_8 = case_when(
        pop_cat_max_val == 5 ~ 1,
        pop_cat_max_val == 25 ~ 2,
        pop_cat_max_val == 50 ~ 3,
        pop_cat_max_val == 100 ~ 4,
        pop_cat_max_val == 500 ~ 5,
        pop_cat_max_val == 2500 ~ 6,
        pop_cat_max_val == 5000 ~ 7,
        pop_cat_max_val == 185000 ~ 8,
        TRUE ~ 0
      )
      
    ) %>% 
    #drop the rgb values using the select helpers?
    select(-starts_with("rgb"))
}


# Mutate steps for HIA-------
#after the data has already been grouped
#March 10, 2023 update:
#I'm thinking about how to incorporate the bounds of the dose-response function.
#I could make the data long form, but I think that may overcomplicate.
#Just create additional variables wide-form.
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
      #Nov 19, 2023: updating this to be scaled down based on how much
      #Landscan overestimates the country's pop. See analysis-global code.
      #Note the minimum is not adjusted but the max and mean are.
      pop_cat_mean_val_scaled = pop_cat_mean_val_adj*pop_ratio_20_plus,
      pop_cat_min_val_scaled = pop_cat_min_val*pop_ratio_20_plus,#unchanged min
      pop_cat_max_val_scaled = pop_cat_max_val_adj*pop_ratio_20_plus,
      
      #a factor version of this
      pop_cat_mean_val_scaled_fac = as.factor(pop_cat_mean_val_scaled),
      
#      using pt to have a suffix - Mar 10, 2023
      rr_alt_pt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      rr_alt_ll = drf_ll**(ndvi_diff/drf_increment),#adding bounds Mar 10, 2023
      rr_alt_ul = drf_ul**(ndvi_diff/drf_increment),#adding bounds Mar 10, 2023

      paf_pt =(rr_alt_pt -1)/rr_alt_pt , #pop_est attrib fraction. 
      paf_ll =(rr_alt_ll -1)/rr_alt_ll, #pop_est attrib fraction
      paf_ul =(rr_alt_ul -1)/rr_alt_ul, #pop_est attrib fraction
      
      #estimate number of deaths first for summary purposes
      #use n_d_0 instead of deaths, baseline
      n_d_0_mean = death_rate_20_plus*pop_cat_mean_val_scaled,
      n_d_0_min = death_rate_20_plus*pop_cat_min_val_scaled,
      n_d_0_max = death_rate_20_plus*pop_cat_max_val_scaled,

      #paf rate - curious Oct 10  
      #doesn't matter if rate and PAF are not independent,
      #just that pop. is
      paf_rate_pt=paf_pt*death_rate_20_plus,

      #Oct 10, 2023
      #I have a feeling I know why they're not adding up, because the
      #denom. includes data from the high-NDVI tertiles...maybe
      pop_cat_mean_val_scaled_bottom_tertiles=case_when(
        ndvi_tertile <3 ~       pop_cat_mean_val_scaled,
        ndvi_tertile==3 ~ 0),
        

      #Attributable deaths
      #The "mean" (or min/max) here corresponds to the pop category and the "pt" (or ll, ul)
      #corresponds to the RR
      attrib_d_mean_pt = paf_pt*n_d_0_mean, 
      attrib_d_min_pt = paf_pt*n_d_0_min,
      attrib_d_max_pt = paf_pt*n_d_0_max,
      
      attrib_d_mean_ll = paf_ll*n_d_0_mean, 
      attrib_d_min_ll = paf_ll*n_d_0_min,
      attrib_d_max_ll = paf_ll*n_d_0_max,
      
      attrib_d_mean_ul = paf_ul*n_d_0_mean, 
      attrib_d_min_ul = paf_ul*n_d_0_min,
      attrib_d_max_ul = paf_ul*n_d_0_max,
      
      #Deaths prevented. 3/10/23 - These names are too long. use n_d_prev for deaths prevented
      n_d_prev_mean_pt = attrib_d_mean_pt*-1,
      n_d_prev_min_pt = attrib_d_min_pt*-1,
      n_d_prev_max_pt = attrib_d_max_pt*-1,
      
      n_d_prev_mean_ll = attrib_d_mean_ll*-1,
      n_d_prev_min_ll = attrib_d_min_ll*-1,
      n_d_prev_max_ll = attrib_d_max_ll*-1,
      
      n_d_prev_mean_ul = attrib_d_mean_ul*-1,
      n_d_prev_min_ul = attrib_d_min_ul*-1,
      n_d_prev_max_ul = attrib_d_max_ul*-1,

      #Pick out the one that will be the true min/max so I don't have to use all 9 every time
      #I verified that this is true (think about flip of sign, etc)
      #over_9 to remember considering bboth sources for min over both sources of variation
      n_d_prev_min_over_9=n_d_prev_min_ul,
      n_d_prev_max_over_9=n_d_prev_max_ll,
      
      #Deaths prevented per pop
      n_d_prev_per_pop_mean_pt = n_d_prev_mean_pt/pop_cat_mean_val_scaled,
      n_d_prev_per_pop_min_pt = n_d_prev_min_pt/pop_cat_min_val_scaled,
      n_d_prev_per_pop_max_pt = n_d_prev_max_pt/pop_cat_max_val_scaled,

      n_d_prev_per_pop_mean_ll = n_d_prev_mean_ll/pop_cat_mean_val_scaled,
      n_d_prev_per_pop_min_ll = n_d_prev_min_ll/pop_cat_min_val_scaled,
      n_d_prev_per_pop_max_ll = n_d_prev_max_ll/pop_cat_max_val_scaled,

      n_d_prev_per_pop_mean_ul = n_d_prev_mean_ul/pop_cat_mean_val_scaled,
      n_d_prev_per_pop_min_ul = n_d_prev_min_ul/pop_cat_min_val_scaled,
      n_d_prev_per_pop_max_ul = n_d_prev_max_ul/pop_cat_max_val_scaled
    )
}


#Summarize the HIA results-----
hia_summarise = function(df){
  df %>% 
    summarise(
      
      #number of distinct values of pop. density. might not always be useful, but sometimes
      #interestingly, some GUBs overlap country, which gives more than 9 unique values,
      #so report both:
      n_distinct_pop_cat_scaled = n_distinct(pop_cat_mean_val_scaled_fac),
      n_distinct_pop_cat_not_scaled = n_distinct(pop_cat_mean_val),

      #scaled population - pop of adults 20+
      pop_cat_mean_val_scaled = sum(pop_cat_mean_val_scaled,na.rm=TRUE),
      pop_cat_min_val_scaled = sum(pop_cat_min_val_scaled,na.rm=TRUE),
      pop_cat_max_val_scaled = sum(pop_cat_max_val_scaled,na.rm=TRUE),

      #for completeness, keep the unscaled population as well
      pop_cat_mean_val = sum(pop_cat_mean_val,na.rm=TRUE),
      pop_cat_min_val = sum(pop_cat_min_val,na.rm=TRUE),
      pop_cat_max_val = sum(pop_cat_max_val,na.rm=TRUE),
    
      #baseline n, deaths
      n_d_0_mean = sum(n_d_0_mean, na.rm=TRUE),
      n_d_0_min = sum(n_d_0_min, na.rm=TRUE),
      n_d_0_max = sum(n_d_0_max, na.rm=TRUE),
      
      #Summarize the min/max over both sources (pop. bounds and RR bounds)
      n_d_prev_mean_pt = sum(n_d_prev_mean_pt, na.rm=TRUE),
      n_d_prev_min_over_9 = sum(n_d_prev_min_over_9, na.rm=TRUE),
      n_d_prev_max_over_9 = sum(n_d_prev_max_over_9, na.rm=TRUE),
      
      #October 10, 2023: I also want to include explicit tracking
      #of the uncertainty due to both sources so that I can verify
      #my suspicion that the uncertainty of the landscan doesn't affect
      #the per-population estimates. The rest follows below
      n_d_prev_min_pt=sum(n_d_prev_min_pt,na.rm=TRUE),
      n_d_prev_max_pt=sum(n_d_prev_max_pt,na.rm=TRUE),
      
      #Oct 10, 2023: not necessary, but I'm curious,
      #is the average PAF multiplied by the average baselien rate get us the same result?
      #If so, then that would justify why the population can cancel (one way)
      #Should work if they're independent (which we assume they are)
      paf_pt_mean=mean(paf_pt,na.rm=TRUE),
      death_rate_20_plus_mean=mean(death_rate_20_plus,na.rm=TRUE),
      #the answer is no, they're different, but they don't need to be independent.
      #try this
      paf_rate_pt_mean=mean(paf_rate_pt,na.rm=TRUE),
      
    
      #don't need the 2019
      ndvi_mean = mean(ndvi_2019, na.rm=TRUE),
      ndvi_sd = sd(ndvi_2019, na.rm=TRUE),
      ndvi_med = median(ndvi_2019, na.rm=TRUE),
      ndvi_25th = quantile(ndvi_2019, probs=c(0.25), na.rm=TRUE),
      ndvi_75th = quantile(ndvi_2019, probs=c(0.75), na.rm=TRUE),
      ndvi_diff_mean = mean(ndvi_diff, na.rm=TRUE),
      ndvi_diff_sd = sd(ndvi_diff, na.rm=TRUE),
      ndvi_diff_med = median(ndvi_diff, na.rm=TRUE),
      ndvi_diff_25th = quantile(ndvi_diff, probs=c(0.25), na.rm=TRUE),
      ndvi_diff_75th = quantile(ndvi_diff, probs=c(0.75), na.rm=TRUE),
      
      #adding some other summary values
      #Good catch - this is the sum of pixels
      #Oct 10, 2023
      area_km2= sum(area_km2_pixel,na.rm=TRUE),
      
      #Oct 10, 2023: I have a hunch this is why my various ways of
      #summarizing number of deaths prev. per 100k are not adding up
      pop_cat_mean_val_scaled_bottom_tertiles=sum(pop_cat_mean_val_scaled_bottom_tertiles,na.rm=T)
      

    ) %>% 
    ungroup() %>% 
    mutate(
      #Deaths prevented per pop - note the order
      n_d_prev_per_1k_pop_pt = (
        n_d_prev_mean_pt/pop_cat_mean_val_scaled)* 1000 ,
  
      #Keeping the _min_over_9 to remember that this is over both sources of uncertainty
      #i.e., 9 iterations - 3 RR values and 3 pop. values
      n_d_prev_per_1k_pop_min_over_9 = (
        n_d_prev_min_over_9/pop_cat_min_val_scaled)* 1000 ,#yes, over the min value of population

      n_d_prev_per_1k_pop_max_over_9 = (
        n_d_prev_max_over_9/pop_cat_max_val_scaled)* 1000,
      
      #Oct 10, 2023: curious re. my claim that population doesn't affect n_d_prev...
      #That is, is the n deaths prevented per pop the same within category of population?
      #I think it will be, but let's check
      n_d_prev_per_pop_mean_pt = n_d_prev_mean_pt/pop_cat_mean_val_scaled,
      n_d_prev_per_pop_min_pt = n_d_prev_min_pt/pop_cat_min_val_scaled,
      n_d_prev_per_pop_max_pt = n_d_prev_max_pt/pop_cat_max_val_scaled,
      
      #checking alt. way to calculate attributable rate
      n_d_prev_per_pop_mean_pt_alt_calc=paf_pt_mean*death_rate_20_plus_mean*-1,
      
      
      #Per 100k as well, per DRR's suggestion, as we did for native plants paper
      #note 5 deaths per 1k is 500 deaths per 100k
      n_d_prev_per_100k_pop_pt=n_d_prev_per_1k_pop_pt*100,
      n_d_prev_per_100k_pop_min_over_9=n_d_prev_per_1k_pop_min_over_9*100,
      n_d_prev_per_100k_pop_max_over_9=n_d_prev_per_1k_pop_max_over_9*100,
      
      #For the table, it'd be easier to have in millions
      #scaled population - pop of adults 20+
      pop_cat_mean_val_scaled_millions = pop_cat_mean_val_scaled/1000000,
      pop_cat_min_val_scaled_millions = pop_cat_min_val_scaled/1000000,
      pop_cat_max_val_scaled_millions = pop_cat_max_val_scaled/1000000,
      
      #for completeness, keep the unscaled population as well
      pop_cat_mean_val_millions = pop_cat_mean_val/1000000,
      pop_cat_min_val_millions = pop_cat_min_val/1000000,
      pop_cat_max_val_millions = pop_cat_max_val/1000000,
      
      #Oct 10, 2023
      #exploring possible reason for various expressions of PAF rate
      #not adding up...
      pop_cat_mean_val_scaled_bottom_tertiles_ratio=
        pop_cat_mean_val_scaled_bottom_tertiles/pop_cat_mean_val,
      
      paf_rate_pt_mean_bottom_tertiles=pop_cat_mean_val_scaled_bottom_tertiles_ratio*paf_rate_pt_mean

    )
}
