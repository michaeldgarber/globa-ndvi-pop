#January 24, 2023
#Easier to have this in its own script.
#Copied this code from
#green-space-denver-project/green-space-denver/scripts/3_HIA_for_each_scenario.R
#Revised Mar 13 2023 to make a function out of part of it as I need those steps
#elsewhere

#Dec 7, 2023: revising this to say "stronger limit" and "weaker limit" so that it's clearer
#if it needs to be flipped. Stronger meaning farther from zero
#Jan 19, 2024: changed this object name to match non-accidental bleow
rr_ac_init_tibble = 1 %>% 
  as_tibble()

rr_ac_wrangle = function(df){
  df %>% 
    mutate(
      #changing this notation a bit Jan 19, 2024 to indicate all-cause mortality rather than
      #the non-accidental below. I don't love the phrase "drf" so I'm changing
      #it to be simply "rr" for risk ratio, as it's more specific
      rr_ac_pt = 0.96,
      rr_ac_sl = 0.94,#strong limit
      rr_ac_wl = 0.97,#weak limit
  #the amount of change in NDVI corresponding to the change in risk
  #measured by the risk ratio from the meta-analysis
  #Jan 19, 2024
  #let's say ndvi increment rather than ndvi_ac_increment
  
  #This is the increment stated in the meta-analysis
  ndvi_ac_increment = 0.1,
  
  rr_ac_pt_log = log(rr_ac_pt),
  rr_ac_sl_log = log(rr_ac_sl),
  rr_ac_wl_log = log(rr_ac_wl),
  measure = "deaths", #to link with the IHME data
  cause_short = "all" ,#to link with the IHME data
  
  #assume symmetrical around lower bound on the log scale.
  #assume lower bound is estimate - 1.96*SD
  rr_sd_log_scale = abs(rr_ac_pt_log-rr_ac_sl_log)/1.96
    )
}
rr_ac = rr_ac_init_tibble %>% 
  rr_ac_wrangle()
  

#The final dataset needs to be rr_ac
rr_ac

# calcs of cohort age and RRs--------
## input data------
#misc calcs of minimum age
min_age_cohort_studies=c(
  25,
  35,
  30,
  21,
  30,
  80,
  18,
  65,
  30) %>% 
  as_tibble()  %>% 
  rename(min_age=value)
#add column the dplyr way
#https://tibble.tidyverse.org/reference/add_column.html
pop_cohort_studies= min_age_cohort_studies %>% 
  add_column(
    pop=c(
      1265515,
      574840,
      108630,
      1645,
      4284680,
      23754,
      792649,
      9218,
      1263721)
    ) %>% 
  add_column(
    rr_pt=c(
      0.92, 0.95, 0.88, 0.92, 0.94, 0.95, 0.92, .97,.99
    )
    )%>% 
  add_column(
    rr_sl=c(0.91, 0.94, 0.82, 0.81, 0.93, 0.94, 0.89, 0.89, 0.98
            )
  ) %>%
  add_column(
    rr_wl=c(0.93, 0.97, 0.94, 1.05, 0.95, 0.95, 0.97, 1.05, 0.99
    )
    ) %>% 
  add_column(
    outcome=c(
      "All non-accidental",
      "All non-accidental",
      "All non-accidental",
      "All-cause after stroke",
      "Natural cause mortality", 
      "All-cause mortality",
      "All-cause mortality",
      "All-cause mortality",
      "All non-accidental"
    )
  ) %>% 
  mutate(
    #collapse natural cause with non-accidental
    outcome_collapsed=
      case_when(
        outcome=="Natural cause mortality"~"All non-accidental",
        TRUE ~outcome
      ),
    #logged versions of the rr
    rr_pt_log=log(rr_pt),
    rr_sl_log=log(rr_sl),
    rr_wl_log=log(rr_wl),
  ) %>% 
  #what's the NDVI increment?
  add_column(#inc for increment
    ndvi_inc=c(0.15, 0.24, 0.1, .22, .14, .1, .1, NA, .1)
  ) %>% 
  #three studies were considered to have a high risk of bias
  #90,92,94
  add_column(
    bias_risk=c("low","low","high","low","high","low","high","low","low")
  ) %>% 
  add_column(#buffer size
    buffer_m=c(250,500,250,250,500,250,300,300,300)  
  ) %>% 
  add_column(
    ref_number=c(88,89,90,91,92,93,94,95,96)
  )

pop_cohort_studies
#another thing I migth do is use a different NDVI increment
#mean(0.15,0.24,0.1) instead of 0.1
## calculations-----
### minimum age------
summary_min_age_pop=pop_cohort_studies  %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    min_age_mean=mean(min_age,na.rm=t),
    min_age_mean_wt=weighted.mean(
      x=min_age,
      w=pop,
      na.rm=T
    ),
    min_age_med=median(min_age,na.rm=T)
  )

### average RR restricted to all-cause mortality
#make a function as I use this 2+ times
summarise_rr=function(df){
    df %>% 
      summarise(
        n_studies=n(), #keep track of number of studies
        #age calcs
        min_age_mean=mean(min_age,na.rm=t),
        min_age_mean_wt=weighted.mean(
          x=min_age,
          w=pop,
          na.rm=T
        ),
        min_age_med=median(min_age,na.rm=T),
        
        rr_pt_log_mean=weighted.mean(
          x=rr_pt_log,
          w=pop,
          na.rm=T
        ),
        rr_sl_log_mean=weighted.mean(
          x=rr_sl_log,
          w=pop,
          na.rm=T
        ),
        rr_wl_log_mean=weighted.mean(
          x=rr_wl_log,
          w=pop,
          na.rm=T
        ),
        
        #ndvi increment mean
        ndvi_inc_mean=weighted.mean(
          x=ndvi_inc,
          w=pop,
          na.rm=T
        ),
        
        #buffer size
        buffer_m_mean=weighted.mean(
          x=buffer_m,
          w=pop,
          na.rm=T
        )
      ) %>% 
      ungroup() %>% 
      mutate(
        #now exponentiate the logged average
        rr_pt_mean=exp(rr_pt_log_mean),
        rr_sl_mean=exp(rr_sl_log_mean),
        rr_wl_mean=exp(rr_wl_log_mean),
      )
}

#which assess non-accidental with a low risk of bias?
pop_cohort_studies %>% 
  filter(outcome_collapsed=="All non-accidental") %>% 
  filter(bias_risk=="low")

rr_by_outcome=pop_cohort_studies %>% 
  group_by(outcome_collapsed) %>% 
  summarise_rr() %>% 
  dplyr::select(-contains("log"))

rr_by_outcome


rr_pooled_overall=  pop_cohort_studies %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>%
  summarise_rr()

rr_pooled_overall

#excluding the study with a high risk of bias
pop_cohort_studies
rr_by_outcome_low_bias=pop_cohort_studies %>% 
  filter(bias_risk=="low") %>% 
  group_by(outcome) %>% 
  summarise_rr() %>% 
  dplyr::select(-contains("log"))

#mean is 0.96 for all-cause
rr_by_outcome_low_bias

# Grab RRs used for non-accidental mortality outcome-------
#we're using this for the non-accidental mortality rate global HIA is
#na for non-accidental
rr_na=rr_by_outcome_low_bias %>% 
  filter(outcome=="All non-accidental") %>% 
  dplyr::select(contains("rr"),contains("ndvi")) %>% 
  #Here I have to change some names to differentiate from all-cause
  #for use in the functions
  rename(
    rr_na_pt=rr_pt_mean,
    rr_na_wl=rr_wl_mean,
    rr_na_sl=rr_sl_mean,
    ndvi_na_increment=ndvi_inc_mean
    )

rr_na
