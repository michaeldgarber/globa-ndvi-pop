#January 24, 2023
#Easier to have this in its own script.
#Copied this code from
#green-space-denver-project/green-space-denver/scripts/3_HIA_for_each_scenario.R
#Revised Mar 13 2023 to make a function out of part of it as I need those steps
#elsewhere

drf_deaths_init_tibble = 1 %>% 
  as_tibble()

drf_deaths_wrangle = function(df){
  df %>% 
    mutate(
      drf_est = 0.96,
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
}
drf_deaths = drf_deaths_init_tibble %>% 
  drf_deaths_wrangle()
  

#The final dataset needs to be drf_deaths
drf_deaths