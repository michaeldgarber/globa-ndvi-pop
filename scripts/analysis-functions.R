#Analysis functions
# Created as its own script Feb 21, 2023
#January 25th, 2022: Since I'm using the same steps for the USA,
#I'm going to make a function
#without me writing tidyterra::rename? The answer is yes. Very cool.
#This will allow me to use the same function for the global analysis

#Dec 5 2023: updates to add age-adjusted numbers
#Jan 17, 2024: updating to include both WHO and GBD data and restricting to pop30+ rather than 20+

# Wrangle landscan population data-------
#Feb 9, 2023: out of curiosity, will this switch between tidyterra and dplyr 


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

setwd(here("data-processed"))
load("countries_joined_with_un_pop_deaths_pared_nogeo.RData")
names(countries_joined_with_un_pop_deaths_pared_nogeo)

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
      
      #Jan 17, 2024: updating to include WHO and GBD
      #Also replacing pop_ratio_20_plus with pop_prop_30_plus
      #Note that pop_prop_30_plus_who_pt is non-missing in 184 countries,
      #while pop_prop_30_plus_gbd_pt is non-missing in 204 countries.
      
      #WHO
      pop_cat_mean_val_scaled_who = pop_cat_mean_val_adj*pop_prop_30_plus_who_pt,
      pop_cat_min_val_scaled_who = pop_cat_min_val*pop_prop_30_plus_who_pt,#unchanged min
      pop_cat_max_val_scaled_who = pop_cat_max_val_adj*pop_prop_30_plus_who_pt,
      
      #GBD version. use point estimate (don't worry about lower, upper limits of pop.)
      pop_cat_mean_val_scaled_gbd = pop_cat_mean_val_adj*pop_prop_30_plus_gbd_pt,
      pop_cat_min_val_scaled_gbd = pop_cat_min_val*pop_prop_30_plus_gbd_pt,#unchanged min
      pop_cat_max_val_scaled_gbd = pop_cat_max_val_adj*pop_prop_30_plus_gbd_pt,
      
      #a factor version of these
      pop_cat_mean_val_scaled_who_fac = as.factor(pop_cat_mean_val_scaled_who),
      pop_cat_mean_val_scaled_gbd_fac = as.factor(pop_cat_mean_val_scaled_gbd),
      
#      using pt to have a suffix - Mar 10, 2023
      #Revising Dec 7, 2023 to say strong (sl) or weak limit (wl) so it's clearer if it needs to be flipped
      #Jan 19, 2024: changing notation a bit to include both all-cause and non-accidental
      #alt means corresponding to the alternative scenario
      rr_ac_alt_pt = rr_ac_pt**(ndvi_diff/ndvi_ac_increment), #calc. risk ratios per dose-response funct
      rr_ac_alt_sl = rr_ac_sl**(ndvi_diff/ndvi_ac_increment),#adding bounds Mar 10, 2023
      rr_ac_alt_wl = rr_ac_wl**(ndvi_diff/ndvi_ac_increment),#adding bounds Mar 10, 2023

      #strong limit vs weak limit
      paf_ac_pt =(rr_ac_alt_pt -1)/rr_ac_alt_pt , #pop_est attrib fraction. 
      paf_ac_sl =(rr_ac_alt_sl -1)/rr_ac_alt_sl, #pop_est attrib fraction
      paf_ac_wl =(rr_ac_alt_wl -1)/rr_ac_alt_wl, #pop_est attrib fraction

      #adding non-accidental stuff as well
      rr_na_alt_pt = rr_na_pt**(ndvi_diff/ndvi_na_increment),
      rr_na_alt_sl = rr_na_sl**(ndvi_diff/ndvi_na_increment), 
      rr_na_alt_wl = rr_na_wl**(ndvi_diff/ndvi_na_increment),
      
      paf_na_pt =(rr_na_alt_pt -1)/rr_na_alt_pt , 
      paf_na_sl =(rr_na_alt_sl -1)/rr_na_alt_sl, 
      paf_na_wl =(rr_na_alt_wl -1)/rr_na_alt_wl, 


      # Baseline number of deaths-------
      ## All-cause deaths------
      #estimate number of deaths first for summary purposes
      #use n_d_ac_0 instead of deaths, baseline
      #Dec 5, 2023: new calculations here to calculate age-adjusted
      #mortality prevented
      #Jan 17, 2024: update here to include both WHO data and GBD data
      #and to restrict to adults 30 plus
      #Use _age_adj vs age_crude
      #[19] "death_rate_ac_gbd_ul_age_all"                  "death_rate_ac_gbd_ll_age_all"                 
      # [21] "death_rate_ac_gbd_pt_age_std"                  "death_rate_ac_gbd_ul_age_std"                 
      # [23] "death_rate_ac_gbd_ll_age_std"                  "death_rate_ac_gbd_pt_age_20_plus"             
      # [25] "death_rate_ac_gbd_ul_age_20_plus"              "death_rate_ac_gbd_ll_age_20_plus"             
      # [27] "death_rate_ac_gbd_pt_age_std_20_plus"          "death_rate_ac_gbd_ul_age_std_20_plus" 
      #use _std for standardized and _crude otherwise. Note all of this should be
      #restricted to adults 20+
      #death_rate_ac_gbd_pt_age_std_20_plus: this is age-adjusted among 20 plus
      #Dec 6, 2023: consider the lower and upper bound of the mortality rates
      #here as well

      #WHO
      #note the WHO data doesn't have intervals, just pt estimates
      #Jan 18, 2024: updating with the _ac notation to differentiate from non-accidental
      n_d_ac_0_std_who_mean=death_rate_ac_who_30_plus_std_pt*pop_cat_mean_val_scaled_who,
      n_d_ac_0_std_who_min=death_rate_ac_who_30_plus_std_pt*pop_cat_min_val_scaled_who, #lower limit here
      n_d_ac_0_std_who_max=death_rate_ac_who_30_plus_std_pt*pop_cat_max_val_scaled_who,#upper limit on rate

      #and now the crude (not age-adjusted) estimate
      n_d_ac_0_crude_who_mean=death_rate_ac_who_30_plus_crude_pt*pop_cat_mean_val_scaled_who,
      n_d_ac_0_crude_who_min=death_rate_ac_who_30_plus_crude_pt*pop_cat_min_val_scaled_who,
      n_d_ac_0_crude_who_max=death_rate_ac_who_30_plus_crude_pt*pop_cat_max_val_scaled_who,

      #GBD
      #GBD rates have confidence intervals
      n_d_ac_0_std_gbd_mean=death_rate_ac_gbd_30_plus_std_pt*pop_cat_mean_val_scaled_gbd,
      n_d_ac_0_std_gbd_min=death_rate_ac_gbd_30_plus_std_ll*pop_cat_min_val_scaled_gbd, #lower limit here
      n_d_ac_0_std_gbd_max=death_rate_ac_gbd_30_plus_std_ul*pop_cat_max_val_scaled_gbd,#upper limit on rate
      
      #and now the crude (not age-adjusted) estimate
      n_d_ac_0_crude_gbd_mean=death_rate_ac_gbd_30_plus_crude_pt*pop_cat_mean_val_scaled_gbd,
      n_d_ac_0_crude_gbd_min=death_rate_ac_gbd_30_plus_crude_ll*pop_cat_min_val_scaled_gbd,
      n_d_ac_0_crude_gbd_max=death_rate_ac_gbd_30_plus_crude_ul*pop_cat_max_val_scaled_gbd,

      ## Non-accidental deaths------
      #Note just WHO data here, no GBD data (not necessary)
      n_d_na_0_std_who_mean=death_rate_na_who_30_plus_std_pt*pop_cat_mean_val_scaled_who,
      n_d_na_0_std_who_min=death_rate_na_who_30_plus_std_pt*pop_cat_min_val_scaled_who, #lower limit here
      n_d_na_0_std_who_max=death_rate_na_who_30_plus_std_pt*pop_cat_max_val_scaled_who,#upper limit on rate
      
      #and now the crude (not age-adjusted) estimate
      n_d_na_0_crude_who_mean=death_rate_na_who_30_plus_crude_pt*pop_cat_mean_val_scaled_who,
      n_d_na_0_crude_who_min=death_rate_na_who_30_plus_crude_pt*pop_cat_min_val_scaled_who,
      n_d_na_0_crude_who_max=death_rate_na_who_30_plus_crude_pt*pop_cat_max_val_scaled_who,

      #paf rate - curious Oct 10  
      #doesn't matter if rate and PAF are not independent,
      #just that pop. is
      #Jan 18, 2024: updating the paf now as well, as I'm going to use
      #a different one for the non-accidental mortality
      paf_ac_rate_pt=paf_ac_pt*death_rate_ac_gbd_30_plus_crude_pt,

      #Oct 10, 2023
      #I have a feeling I know why they're not adding up, because the
      #denom. includes data from the high-NDVI tertiles...maybe
      pop_cat_mean_val_scaled_bottom_tertiles=case_when(
        ndvi_tertile <3 ~       pop_cat_mean_val_scaled_gbd,
        ndvi_tertile==3 ~ 0),
        

      # Attributable deaths----
      ## All-cause deaths prevented--------
      #Attributable deaths
      #The "mean" (or min/max) here corresponds to the pop category and the "pt" (or ll, ul)
      #corresponds to the RR

      #Dec 7, 2023: simplify this. Don't need so many mins and maxes. Just pick one of each.
      #Note I now have "weak limit" and "strong limit" on PAF as it's more intuitive

      #age-adjusted
      #here the mean corresponds to the n_d_ac_0_ estimate and the pt, sl, wl corresponds to the PAF
      n_d_ac_prev_std_gbd_mean_pt = paf_ac_pt*n_d_ac_0_std_gbd_mean*-1, #multiply by -1 so it's prevented deaths
      n_d_ac_prev_std_gbd_mean_ul = paf_ac_sl*n_d_ac_0_std_gbd_mean*-1,  
      n_d_ac_prev_std_gbd_mean_ll = paf_ac_wl*n_d_ac_0_std_gbd_mean*-1,  
      
      n_d_ac_prev_std_gbd_min_pt = paf_ac_pt*n_d_ac_0_std_gbd_min*-1,
      n_d_ac_prev_std_gbd_min_ul = paf_ac_sl*n_d_ac_0_std_gbd_min*-1,  
      n_d_ac_prev_std_gbd_min_ll = paf_ac_wl*n_d_ac_0_std_gbd_min*-1,  
      
      n_d_ac_prev_std_gbd_max_pt = paf_ac_pt*n_d_ac_0_std_gbd_max*-1,
      n_d_ac_prev_std_gbd_max_ul = paf_ac_sl*n_d_ac_0_std_gbd_max*-1,  
      n_d_ac_prev_std_gbd_max_ll = paf_ac_wl*n_d_ac_0_std_gbd_max*-1,  

      #WHO version
      n_d_ac_prev_std_who_mean_pt = paf_ac_pt*n_d_ac_0_std_who_mean*-1, #multiply by -1 so it's prevented deaths
      n_d_ac_prev_std_who_mean_ul = paf_ac_sl*n_d_ac_0_std_who_mean*-1,  
      n_d_ac_prev_std_who_mean_ll = paf_ac_wl*n_d_ac_0_std_who_mean*-1,  
      
      n_d_ac_prev_std_who_min_pt = paf_ac_pt*n_d_ac_0_std_who_min*-1,
      n_d_ac_prev_std_who_min_ul = paf_ac_sl*n_d_ac_0_std_who_min*-1,  
      n_d_ac_prev_std_who_min_ll = paf_ac_wl*n_d_ac_0_std_who_min*-1,  
      
      n_d_ac_prev_std_who_max_pt = paf_ac_pt*n_d_ac_0_std_who_max*-1,
      n_d_ac_prev_std_who_max_ul = paf_ac_sl*n_d_ac_0_std_who_max*-1,  
      n_d_ac_prev_std_who_max_ll = paf_ac_wl*n_d_ac_0_std_who_max*-1,  

      #crude (not age-adjusted)
      n_d_ac_prev_crude_gbd_mean_pt = paf_ac_pt*n_d_ac_0_crude_gbd_mean*-1, #multiply by -1 so it's prevented deaths
      n_d_ac_prev_crude_gbd_mean_ul = paf_ac_sl*n_d_ac_0_crude_gbd_mean*-1,  
      n_d_ac_prev_crude_gbd_mean_ll = paf_ac_wl*n_d_ac_0_crude_gbd_mean*-1,  
      
      n_d_ac_prev_crude_gbd_min_pt = paf_ac_pt*n_d_ac_0_crude_gbd_min*-1,
      n_d_ac_prev_crude_gbd_min_ul = paf_ac_sl*n_d_ac_0_crude_gbd_min*-1,  
      n_d_ac_prev_crude_gbd_min_ll = paf_ac_wl*n_d_ac_0_crude_gbd_min*-1,  
      
      n_d_ac_prev_crude_gbd_max_pt = paf_ac_pt*n_d_ac_0_crude_gbd_max*-1,
      n_d_ac_prev_crude_gbd_max_ul = paf_ac_sl*n_d_ac_0_crude_gbd_max*-1,
      n_d_ac_prev_crude_gbd_max_ll = paf_ac_wl*n_d_ac_0_crude_gbd_max*-1,  

      n_d_ac_prev_crude_who_mean_pt = paf_ac_pt*n_d_ac_0_crude_who_mean*-1, #multiply by -1 so it's prevented deaths
      n_d_ac_prev_crude_who_mean_ul = paf_ac_sl*n_d_ac_0_crude_who_mean*-1,  
      n_d_ac_prev_crude_who_mean_ll = paf_ac_wl*n_d_ac_0_crude_who_mean*-1,  
      
      n_d_ac_prev_crude_who_min_pt = paf_ac_pt*n_d_ac_0_crude_who_min*-1,
      n_d_ac_prev_crude_who_min_ul = paf_ac_sl*n_d_ac_0_crude_who_min*-1,  
      n_d_ac_prev_crude_who_min_ll = paf_ac_wl*n_d_ac_0_crude_who_min*-1,  
      
      n_d_ac_prev_crude_who_max_pt = paf_ac_pt*n_d_ac_0_crude_who_max*-1,
      n_d_ac_prev_crude_who_max_ul = paf_ac_sl*n_d_ac_0_crude_who_max*-1,
      n_d_ac_prev_crude_who_max_ll = paf_ac_wl*n_d_ac_0_crude_who_max*-1,  


      #Pick out the one that will be the true min/max so I don't have to use all 9 every time
      n_d_ac_prev_std_gbd_min_over_9=n_d_ac_prev_std_gbd_min_ll,
      n_d_ac_prev_std_gbd_max_over_9=n_d_ac_prev_std_gbd_max_ul,
      
      n_d_ac_prev_crude_gbd_min_over_9=n_d_ac_prev_crude_gbd_min_ll,
      n_d_ac_prev_crude_gbd_max_over_9=n_d_ac_prev_crude_gbd_max_ul,

      n_d_ac_prev_std_who_min_over_9=n_d_ac_prev_std_who_min_ll,
      n_d_ac_prev_std_who_max_over_9=n_d_ac_prev_std_who_max_ul,
      
      n_d_ac_prev_crude_who_min_over_9=n_d_ac_prev_crude_who_min_ll,
      n_d_ac_prev_crude_who_max_over_9=n_d_ac_prev_crude_who_max_ul,
    
      
      #Deaths prevented per pop
      #age-adjusted
      n_d_ac_prev_std_gbd_per_pop_mean_pt = n_d_ac_prev_std_gbd_mean_pt/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_min_pt = n_d_ac_prev_std_gbd_min_pt/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_max_pt = n_d_ac_prev_std_gbd_max_pt/pop_cat_max_val_scaled_gbd,

      n_d_ac_prev_std_gbd_per_pop_mean_ll = n_d_ac_prev_std_gbd_mean_ll/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_min_ll = n_d_ac_prev_std_gbd_min_ll/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_max_ll = n_d_ac_prev_std_gbd_max_ll/pop_cat_max_val_scaled_gbd,

      n_d_ac_prev_std_gbd_per_pop_mean_ul = n_d_ac_prev_std_gbd_mean_ul/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_min_ul = n_d_ac_prev_std_gbd_min_ul/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_std_gbd_per_pop_max_ul = n_d_ac_prev_std_gbd_max_ul/pop_cat_max_val_scaled_gbd,

      n_d_ac_prev_std_who_per_pop_mean_pt = n_d_ac_prev_std_who_mean_pt/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_min_pt = n_d_ac_prev_std_who_min_pt/pop_cat_min_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_max_pt = n_d_ac_prev_std_who_max_pt/pop_cat_max_val_scaled_who,
      
      n_d_ac_prev_std_who_per_pop_mean_ll = n_d_ac_prev_std_who_mean_ll/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_min_ll = n_d_ac_prev_std_who_min_ll/pop_cat_min_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_max_ll = n_d_ac_prev_std_who_max_ll/pop_cat_max_val_scaled_who,
      
      n_d_ac_prev_std_who_per_pop_mean_ul = n_d_ac_prev_std_who_mean_ul/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_min_ul = n_d_ac_prev_std_who_min_ul/pop_cat_min_val_scaled_who,
      n_d_ac_prev_std_who_per_pop_max_ul = n_d_ac_prev_std_who_max_ul/pop_cat_max_val_scaled_who,


      #crude
      n_d_ac_prev_crude_gbd_per_pop_mean_pt = n_d_ac_prev_crude_gbd_mean_pt/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_min_pt = n_d_ac_prev_crude_gbd_min_pt/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_max_pt = n_d_ac_prev_crude_gbd_max_pt/pop_cat_max_val_scaled_gbd,
      
      n_d_ac_prev_crude_gbd_per_pop_mean_ll = n_d_ac_prev_crude_gbd_mean_ll/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_min_ll = n_d_ac_prev_crude_gbd_min_ll/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_max_ll = n_d_ac_prev_crude_gbd_max_ll/pop_cat_max_val_scaled_gbd,
      
      n_d_ac_prev_crude_gbd_per_pop_mean_ul = n_d_ac_prev_crude_gbd_mean_ul/pop_cat_mean_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_min_ul = n_d_ac_prev_crude_gbd_min_ul/pop_cat_min_val_scaled_gbd,
      n_d_ac_prev_crude_gbd_per_pop_max_ul = n_d_ac_prev_crude_gbd_max_ul/pop_cat_max_val_scaled_gbd,

      n_d_ac_prev_crude_who_per_pop_mean_pt = n_d_ac_prev_crude_who_mean_pt/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_min_pt = n_d_ac_prev_crude_who_min_pt/pop_cat_min_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_max_pt = n_d_ac_prev_crude_who_max_pt/pop_cat_max_val_scaled_who,
      
      n_d_ac_prev_crude_who_per_pop_mean_ll = n_d_ac_prev_crude_who_mean_ll/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_min_ll = n_d_ac_prev_crude_who_min_ll/pop_cat_min_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_max_ll = n_d_ac_prev_crude_who_max_ll/pop_cat_max_val_scaled_who,
      
      n_d_ac_prev_crude_who_per_pop_mean_ul = n_d_ac_prev_crude_who_mean_ul/pop_cat_mean_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_min_ul = n_d_ac_prev_crude_who_min_ul/pop_cat_min_val_scaled_who,
      n_d_ac_prev_crude_who_per_pop_max_ul = n_d_ac_prev_crude_who_max_ul/pop_cat_max_val_scaled_who,

      
      ## Non-accidental deaths prevented--------
      #na for non-accidental
      # WHO only
      #age-standardized
      n_d_na_prev_std_who_mean_pt = paf_na_pt*n_d_na_0_std_who_mean*-1, #multiply by -1 so it's prevented deaths
      n_d_na_prev_std_who_mean_ul = paf_na_sl*n_d_na_0_std_who_mean*-1,  
      n_d_na_prev_std_who_mean_ll = paf_na_wl*n_d_na_0_std_who_mean*-1,  
      
      n_d_na_prev_std_who_min_pt = paf_na_pt*n_d_na_0_std_who_min*-1,
      n_d_na_prev_std_who_min_ul = paf_na_sl*n_d_na_0_std_who_min*-1,  
      n_d_na_prev_std_who_min_ll = paf_na_wl*n_d_na_0_std_who_min*-1,  
      
      n_d_na_prev_std_who_max_pt = paf_na_pt*n_d_na_0_std_who_max*-1,
      n_d_na_prev_std_who_max_ul = paf_na_sl*n_d_na_0_std_who_max*-1,  
      n_d_na_prev_std_who_max_ll = paf_na_wl*n_d_na_0_std_who_max*-1,  

      #crude
      n_d_na_prev_crude_who_mean_pt = paf_na_pt*n_d_na_0_crude_who_mean*-1,
      n_d_na_prev_crude_who_mean_ul = paf_na_sl*n_d_na_0_crude_who_mean*-1,  
      n_d_na_prev_crude_who_mean_ll = paf_na_wl*n_d_na_0_crude_who_mean*-1,  
      
      n_d_na_prev_crude_who_min_pt = paf_na_pt*n_d_na_0_crude_who_min*-1,
      n_d_na_prev_crude_who_min_ul = paf_na_sl*n_d_na_0_crude_who_min*-1,  
      n_d_na_prev_crude_who_min_ll = paf_na_wl*n_d_na_0_crude_who_min*-1,  
      
      n_d_na_prev_crude_who_max_pt = paf_na_pt*n_d_na_0_crude_who_max*-1,
      n_d_na_prev_crude_who_max_ul = paf_na_sl*n_d_na_0_crude_who_max*-1,
      n_d_na_prev_crude_who_max_ll = paf_na_wl*n_d_na_0_crude_who_max*-1,  

      #select true min/max
      n_d_na_prev_std_who_min_over_9=n_d_na_prev_std_who_min_ll,
      n_d_na_prev_std_who_max_over_9=n_d_na_prev_std_who_max_ul,
      
      n_d_na_prev_crude_who_min_over_9=n_d_na_prev_crude_who_min_ll,
      n_d_na_prev_crude_who_max_over_9=n_d_na_prev_crude_who_max_ul,

      #Deaths prevented per pop
      #age-adjusted
      n_d_na_prev_std_who_per_pop_mean_pt = n_d_na_prev_std_who_mean_pt/pop_cat_mean_val_scaled_who,
      n_d_na_prev_std_who_per_pop_min_pt = n_d_na_prev_std_who_min_pt/pop_cat_min_val_scaled_who,
      n_d_na_prev_std_who_per_pop_max_pt = n_d_na_prev_std_who_max_pt/pop_cat_max_val_scaled_who,
      
      n_d_na_prev_std_who_per_pop_mean_ll = n_d_na_prev_std_who_mean_ll/pop_cat_mean_val_scaled_who,
      n_d_na_prev_std_who_per_pop_min_ll = n_d_na_prev_std_who_min_ll/pop_cat_min_val_scaled_who,
      n_d_na_prev_std_who_per_pop_max_ll = n_d_na_prev_std_who_max_ll/pop_cat_max_val_scaled_who,
      
      n_d_na_prev_std_who_per_pop_mean_ul = n_d_na_prev_std_who_mean_ul/pop_cat_mean_val_scaled_who,
      n_d_na_prev_std_who_per_pop_min_ul = n_d_na_prev_std_who_min_ul/pop_cat_min_val_scaled_who,
      n_d_na_prev_std_who_per_pop_max_ul = n_d_na_prev_std_who_max_ul/pop_cat_max_val_scaled_who,

      #crude
      n_d_na_prev_crude_who_per_pop_mean_pt = n_d_na_prev_crude_who_mean_pt/pop_cat_mean_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_min_pt = n_d_na_prev_crude_who_min_pt/pop_cat_min_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_max_pt = n_d_na_prev_crude_who_max_pt/pop_cat_max_val_scaled_who,
      
      n_d_na_prev_crude_who_per_pop_mean_ll = n_d_na_prev_crude_who_mean_ll/pop_cat_mean_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_min_ll = n_d_na_prev_crude_who_min_ll/pop_cat_min_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_max_ll = n_d_na_prev_crude_who_max_ll/pop_cat_max_val_scaled_who,
      
      n_d_na_prev_crude_who_per_pop_mean_ul = n_d_na_prev_crude_who_mean_ul/pop_cat_mean_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_min_ul = n_d_na_prev_crude_who_min_ul/pop_cat_min_val_scaled_who,
      n_d_na_prev_crude_who_per_pop_max_ul = n_d_na_prev_crude_who_max_ul/pop_cat_max_val_scaled_who
    ) 
}


#Summarize the HIA results-----
#Revising Jan 17, 2024
hia_summarise = function(df){
  df %>% 
    summarise(
      
      #number of distinct values of pop. density. might not always be useful, but sometimes
      #interestingly, some GUBs overlap country, which gives more than 9 unique values,
      #so report both:
      
      n_distinct_pop_cat_scaled_gbd = n_distinct(pop_cat_mean_val_scaled_who),
      n_distinct_pop_cat_scaled_who = n_distinct(pop_cat_mean_val_scaled_who),
      
      n_distinct_pop_cat_not_scaled = n_distinct(pop_cat_mean_val),

      #scaled population - pop of adults 30+
      #Jan 17, 2024: we now have two versions of this - GBD and WHO
      pop_cat_mean_val_scaled_gbd = sum(pop_cat_mean_val_scaled_gbd,na.rm=TRUE),
      pop_cat_min_val_scaled_gbd = sum(pop_cat_min_val_scaled_gbd,na.rm=TRUE),
      pop_cat_max_val_scaled_gbd = sum(pop_cat_max_val_scaled_gbd,na.rm=TRUE),
      
      pop_cat_mean_val_scaled_who = sum(pop_cat_mean_val_scaled_who,na.rm=TRUE),
      pop_cat_min_val_scaled_who = sum(pop_cat_min_val_scaled_who,na.rm=TRUE),
      pop_cat_max_val_scaled_who = sum(pop_cat_max_val_scaled_who,na.rm=TRUE),

      #for completeness, keep the unscaled population as well
      pop_cat_mean_val = sum(pop_cat_mean_val,na.rm=TRUE),
      pop_cat_min_val = sum(pop_cat_min_val,na.rm=TRUE),
      pop_cat_max_val = sum(pop_cat_max_val,na.rm=TRUE),
    
      #baseline n, deaths
      #all cause
      n_d_ac_0_std_gbd_mean = sum(n_d_ac_0_std_gbd_mean, na.rm=TRUE),
      n_d_ac_0_std_gbd_min = sum(n_d_ac_0_std_gbd_min, na.rm=TRUE),
      n_d_ac_0_std_gbd_max = sum(n_d_ac_0_std_gbd_max, na.rm=TRUE),
      
      n_d_ac_0_crude_gbd_mean = sum(n_d_ac_0_crude_gbd_mean, na.rm=TRUE),
      n_d_ac_0_crude_gbd_min = sum(n_d_ac_0_crude_gbd_min, na.rm=TRUE),
      n_d_ac_0_crude_gbd_max = sum(n_d_ac_0_crude_gbd_max, na.rm=TRUE),
      
      n_d_ac_0_std_who_mean = sum(n_d_ac_0_std_who_mean, na.rm=TRUE),
      n_d_ac_0_std_who_min = sum(n_d_ac_0_std_who_min, na.rm=TRUE),
      n_d_ac_0_std_who_max = sum(n_d_ac_0_std_who_max, na.rm=TRUE),
      
      n_d_ac_0_crude_who_mean = sum(n_d_ac_0_crude_who_mean, na.rm=TRUE),
      n_d_ac_0_crude_who_min = sum(n_d_ac_0_crude_who_min, na.rm=TRUE),
      n_d_ac_0_crude_who_max = sum(n_d_ac_0_crude_who_max, na.rm=TRUE),
      
      #non-accidental
      n_d_na_0_std_who_mean = sum(n_d_na_0_std_who_mean, na.rm=TRUE),
      n_d_na_0_std_who_min = sum(n_d_na_0_std_who_min, na.rm=TRUE),
      n_d_na_0_std_who_max = sum(n_d_na_0_std_who_max, na.rm=TRUE),
      
      n_d_na_0_crude_who_mean = sum(n_d_na_0_crude_who_mean, na.rm=TRUE),
      n_d_na_0_crude_who_min = sum(n_d_na_0_crude_who_min, na.rm=TRUE),
      n_d_na_0_crude_who_max = sum(n_d_na_0_crude_who_max, na.rm=TRUE),
      

      #Summarize the min/max over both sources (pop. bounds and RR bounds)
      #all-cause
      n_d_ac_prev_std_gbd_mean_pt = sum(n_d_ac_prev_std_gbd_mean_pt, na.rm=TRUE),
      n_d_ac_prev_std_gbd_min_over_9 = sum(n_d_ac_prev_std_gbd_min_over_9, na.rm=TRUE),
      n_d_ac_prev_std_gbd_max_over_9 = sum(n_d_ac_prev_std_gbd_max_over_9, na.rm=TRUE),
      
      n_d_ac_prev_std_gbd_mean_pt = sum(n_d_ac_prev_std_gbd_mean_pt, na.rm=TRUE),
      n_d_ac_prev_std_gbd_min_over_9 = sum(n_d_ac_prev_std_gbd_min_over_9, na.rm=TRUE),
      n_d_ac_prev_std_gbd_max_over_9 = sum(n_d_ac_prev_std_gbd_max_over_9, na.rm=TRUE),
      
      n_d_ac_prev_std_who_mean_pt = sum(n_d_ac_prev_std_who_mean_pt, na.rm=TRUE),
      n_d_ac_prev_std_who_min_over_9 = sum(n_d_ac_prev_std_who_min_over_9, na.rm=TRUE),
      n_d_ac_prev_std_who_max_over_9 = sum(n_d_ac_prev_std_who_max_over_9, na.rm=TRUE),
      
      n_d_ac_prev_std_who_mean_pt = sum(n_d_ac_prev_std_who_mean_pt, na.rm=TRUE),
      n_d_ac_prev_std_who_min_over_9 = sum(n_d_ac_prev_std_who_min_over_9, na.rm=TRUE),
      n_d_ac_prev_std_who_max_over_9 = sum(n_d_ac_prev_std_who_max_over_9, na.rm=TRUE),
      
      #non-accidental
      n_d_na_prev_std_who_mean_pt = sum(n_d_na_prev_std_who_mean_pt, na.rm=TRUE),
      n_d_na_prev_std_who_min_over_9 = sum(n_d_na_prev_std_who_min_over_9, na.rm=TRUE),
      n_d_na_prev_std_who_max_over_9 = sum(n_d_na_prev_std_who_max_over_9, na.rm=TRUE),
      
      n_d_na_prev_std_who_mean_pt = sum(n_d_na_prev_std_who_mean_pt, na.rm=TRUE),
      n_d_na_prev_std_who_min_over_9 = sum(n_d_na_prev_std_who_min_over_9, na.rm=TRUE),
      n_d_na_prev_std_who_max_over_9 = sum(n_d_na_prev_std_who_max_over_9, na.rm=TRUE),
      

      #October 10, 2023: I also want to include explicit tracking
      #of the uncertainty due to both sources so that I can verify
      #my suspicion that the uncertainty of the landscan doesn't affect
      #the per-population estimates. The rest follows below
      n_d_ac_prev_std_gbd_min_pt=sum(n_d_ac_prev_std_gbd_min_pt,na.rm=TRUE),
      n_d_ac_prev_std_gbd_max_pt=sum(n_d_ac_prev_std_gbd_max_pt,na.rm=TRUE),
      
      n_d_ac_prev_std_who_min_pt=sum(n_d_ac_prev_std_who_min_pt,na.rm=TRUE),
      n_d_ac_prev_std_who_max_pt=sum(n_d_ac_prev_std_who_max_pt,na.rm=TRUE),
      
      n_d_na_prev_std_who_min_pt=sum(n_d_na_prev_std_who_min_pt,na.rm=TRUE),
      n_d_na_prev_std_who_max_pt=sum(n_d_na_prev_std_who_max_pt,na.rm=TRUE),
      
      
      #crude versions
      #all-cause
      n_d_ac_prev_crude_gbd_mean_pt = sum(n_d_ac_prev_crude_gbd_mean_pt, na.rm=TRUE),
      n_d_ac_prev_crude_gbd_min_over_9 = sum(n_d_ac_prev_crude_gbd_min_over_9, na.rm=TRUE),
      n_d_ac_prev_crude_gbd_max_over_9 = sum(n_d_ac_prev_crude_gbd_max_over_9, na.rm=TRUE),
      
      n_d_ac_prev_crude_gbd_min_pt=sum(n_d_ac_prev_crude_gbd_min_pt,na.rm=TRUE),
      n_d_ac_prev_crude_gbd_max_pt=sum(n_d_ac_prev_crude_gbd_max_pt,na.rm=TRUE),
      
      n_d_ac_prev_crude_who_mean_pt = sum(n_d_ac_prev_crude_who_mean_pt, na.rm=TRUE),
      n_d_ac_prev_crude_who_min_over_9 = sum(n_d_ac_prev_crude_who_min_over_9, na.rm=TRUE),
      n_d_ac_prev_crude_who_max_over_9 = sum(n_d_ac_prev_crude_who_max_over_9, na.rm=TRUE),
      
      n_d_ac_prev_crude_who_min_pt=sum(n_d_ac_prev_crude_who_min_pt,na.rm=TRUE),
      n_d_ac_prev_crude_who_max_pt=sum(n_d_ac_prev_crude_who_max_pt,na.rm=TRUE),
      
      #non-accidental
      n_d_na_prev_crude_who_mean_pt = sum(n_d_na_prev_crude_who_mean_pt, na.rm=TRUE),
      n_d_na_prev_crude_who_min_over_9 = sum(n_d_na_prev_crude_who_min_over_9, na.rm=TRUE),
      n_d_na_prev_crude_who_max_over_9 = sum(n_d_na_prev_crude_who_max_over_9, na.rm=TRUE),
      
      n_d_na_prev_crude_who_min_pt=sum(n_d_na_prev_crude_who_min_pt,na.rm=TRUE),
      n_d_na_prev_crude_who_max_pt=sum(n_d_na_prev_crude_who_max_pt,na.rm=TRUE),
      
      
      
      #Oct 10, 2023: not necessary, but I'm curious,
      #is the average PAF multiplied by the average baseline rate get us the same result?
      #If so, then that would justify why the population can cancel (one way)
      #Should work if they're independent (which we assume they are)
      paf_ac_pt_mean=mean(paf_ac_pt,na.rm=TRUE),
      # death_rate_ac_gbd_pt_age_std_20_plus_mean=mean(death_rate_ac_gbd_pt_age_std_20_plus,na.rm=TRUE),
      # death_rate_ac_gbd_pt_age_20_plus_mean=mean(death_rate_ac_gbd_pt_age_20_plus,na.rm=TRUE),
      # #the answer is no, they're different, but they don't need to be independent.
      # #try this
      paf_ac_rate_pt_mean=mean(paf_ac_rate_pt,na.rm=TRUE),
  
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
      

      #Oct 10, 2023: I have a hunch this is why my various ways of
      #summarizing number of deaths prev. per 100k are not adding up
      pop_cat_mean_val_scaled_bottom_tertiles=sum(pop_cat_mean_val_scaled_bottom_tertiles,na.rm=T),
      
      #adding some other summary values
      #Good catch - this is the sum of pixels
      #Oct 10, 2023
      area_km2= sum(area_km2_pixel,na.rm=TRUE),
 
    ) %>% 
    ungroup() %>% 
    mutate(
      #Deaths prevented per pop------
      #Deaths prevented per pop - note the order
      #Jan 17, 2024:
      #Note sure why I began with 1k. Perhaps we can omit this and go straight to 100k?
      ## all-cause-------------
      n_d_ac_prev_std_gbd_per_100k_pop_pt = (
        n_d_ac_prev_std_gbd_mean_pt/pop_cat_mean_val_scaled_gbd)*100000 ,
  
      #Keeping the _min_over_9 to remember that this is over both sources of uncertainty
      #i.e., 9 iterations - 3 RR values and 3 pop. values
      n_d_ac_prev_std_gbd_per_100k_pop_min_over_9 = (
        #yes, over the min value of population
        n_d_ac_prev_std_gbd_min_over_9/pop_cat_min_val_scaled_gbd)*100000 ,

      n_d_ac_prev_std_gbd_per_100k_pop_max_over_9 = (
        n_d_ac_prev_std_gbd_max_over_9/pop_cat_max_val_scaled_gbd)*100000,
      
      #same for WHO
      n_d_ac_prev_std_who_per_100k_pop_pt = (
        n_d_ac_prev_std_who_mean_pt/pop_cat_mean_val_scaled_who)*100000 ,
      n_d_ac_prev_std_who_per_100k_pop_min_over_9 = (
        n_d_ac_prev_std_who_min_over_9/pop_cat_min_val_scaled_who)*100000 ,
      n_d_ac_prev_std_who_per_100k_pop_max_over_9 = (
        n_d_ac_prev_std_who_max_over_9/pop_cat_max_val_scaled_who)*100000,
      

      #Oct 10, 2023: curious re. my claim that population doesn't affect n_d_ac_prev...
      #That is, is the n deaths prevented per pop the same within category of population?
      #I think it will be, but let's check
      # n_d_ac_prev_std_per_pop_mean_pt = n_d_ac_prev_std_mean_pt/pop_cat_mean_val_scaled,
      # n_d_ac_prev_std_per_pop_min_pt = n_d_ac_prev_std_min_pt/pop_cat_min_val_scaled,
      # n_d_ac_prev_std_per_pop_max_pt = n_d_ac_prev_std_max_pt/pop_cat_max_val_scaled,
      
      #checking alt. way to calculate attributable rate
#      n_d_ac_prev_std_per_pop_mean_pt_alt_calc=paf_ac_pt_mean*death_rate_ac_gbd_pt_age_std_20_plus_mean*-1,

#      n_d_ac_prev_std_per_100k_pop_max_over_9=n_d_ac_prev_std_per_1k_pop_max_over_9*100,
      
      #all of the above for the crude (not standardized) estimate
      #gbd
      n_d_ac_prev_crude_gbd_per_100k_pop_pt = (
        n_d_ac_prev_crude_gbd_mean_pt/pop_cat_mean_val_scaled_gbd)* 100000 ,
    
      n_d_ac_prev_crude_gbd_per_100k_pop_min_over_9 = (
        n_d_ac_prev_crude_gbd_min_over_9/pop_cat_min_val_scaled_gbd)*100000 ,
      
      n_d_ac_prev_crude_gbd_per_100k_pop_max_over_9 = (
        n_d_ac_prev_crude_gbd_max_over_9/pop_cat_max_val_scaled_gbd)* 100000,
      
      #same for WHO
      n_d_ac_prev_crude_who_per_100k_pop_pt = (
        n_d_ac_prev_crude_who_mean_pt/pop_cat_mean_val_scaled_who)* 100000 ,
      n_d_ac_prev_crude_who_per_100k_pop_min_over_9 = (
        n_d_ac_prev_crude_who_min_over_9/pop_cat_min_val_scaled_who)* 100000 ,
      n_d_ac_prev_crude_who_per_100k_pop_max_over_9 = (
        n_d_ac_prev_crude_who_max_over_9/pop_cat_max_val_scaled_who)* 100000,

      #checking alt. way to calculate attributable rate
#      n_d_ac_prev_crude_per_pop_mean_pt_alt_calc=paf_ac_pt_mean*death_rate_ac_gbd_pt_age_20_plus_mean*-1,

      ## non-accidental---------
      n_d_na_prev_std_who_per_100k_pop_pt = (
        n_d_na_prev_std_who_mean_pt/pop_cat_mean_val_scaled_who)*100000 ,
      n_d_na_prev_std_who_per_100k_pop_min_over_9 = (
        n_d_na_prev_std_who_min_over_9/pop_cat_min_val_scaled_who)*100000 ,
      n_d_na_prev_std_who_per_100k_pop_max_over_9 = (
        n_d_na_prev_std_who_max_over_9/pop_cat_max_val_scaled_who)*100000,

      n_d_na_prev_crude_who_per_100k_pop_pt = (
        n_d_na_prev_crude_who_mean_pt/pop_cat_mean_val_scaled_who)* 100000 ,
      n_d_na_prev_crude_who_per_100k_pop_min_over_9 = (
        n_d_na_prev_crude_who_min_over_9/pop_cat_min_val_scaled_who)* 100000 ,
      n_d_na_prev_crude_who_per_100k_pop_max_over_9 = (
        n_d_na_prev_crude_who_max_over_9/pop_cat_max_val_scaled_who)* 100000,

      #For the table, it'd be easier to have in millions
      #scaled population - pop of adults 30+
      pop_cat_mean_val_scaled_gbd_millions = pop_cat_mean_val_scaled_gbd/1000000,
      pop_cat_min_val_scaled_gbd_millions = pop_cat_min_val_scaled_gbd/1000000,
      pop_cat_max_val_scaled_gbd_millions = pop_cat_max_val_scaled_gbd/1000000,

      pop_cat_mean_val_scaled_who_millions = pop_cat_mean_val_scaled_who/1000000,
      pop_cat_min_val_scaled_who_millions = pop_cat_min_val_scaled_who/1000000,
      pop_cat_max_val_scaled_who_millions = pop_cat_max_val_scaled_who/1000000,

      #for completeness, keep the unscaled population as well
      pop_cat_mean_val_millions = pop_cat_mean_val/1000000,
      pop_cat_min_val_millions = pop_cat_min_val/1000000,
      pop_cat_max_val_millions = pop_cat_max_val/1000000,
      
      #Oct 10, 2023
      #exploring possible reason for various expressions of PAF rate
      #not adding up...
      pop_cat_mean_val_scaled_bottom_tertiles_ratio=
        pop_cat_mean_val_scaled_bottom_tertiles/pop_cat_mean_val,
      
      paf_ac_rate_pt_mean_bottom_tertiles=pop_cat_mean_val_scaled_bottom_tertiles_ratio*paf_ac_rate_pt_mean

    )
}
