#January 24, 2023
#Easier to have this in its own script.
#Copied this code from
#green-space-denver-project/green-space-denver/scripts/3_HIA_for_each_scenario.R

drf_deaths =  0.96 %>% 
  as_tibble() %>% 
  rename(drf_est=value) %>% 
  mutate(
    drf_ll = 0.94,
    drf_ul = 0.97,
    #the amount of change in NDVI corresponding to the change in risk
    #measured by the risk ratio from the meta-analysis
    drf_increment = 0.1,
    
    drf_est_log = log(drf_est),
    drf_ll_log = log(drf_ll),
    drf_ul_log = log(drf_ul),
    measure = "deaths", #to link with the IHME data
    cause_short = "all" ,#to link with the IHME data
    
    #assume symmetrical around lower bound on the log scale.
    #assume lower bound is estimate - 1.96*SD
    drf_sd_log_scale = abs(drf_est_log-drf_ll_log)/1.96
  )

drf_deaths