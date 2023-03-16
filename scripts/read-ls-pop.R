#And this script for reading the population data

#THe data appear to be a .adf file, which I guess is ESRI's raster file format?
#https://stackoverflow.com/questions/71695609/how-to-properly-load-these-adf-files
library(raster)
library(terra)
library(here)
library(tidyverse)
library(mapview)
#https://cran.r-project.org/web/packages/tidyterra/vignettes/tidyterra.html
library(tidyterra)
library(sf)

# Feb 8 2023
#Adding global at the bottom. Not the most efficient code to begin with
#smaller and work up to global, but it's okay..


# Read data-------
#Note I was having an issue opening the data in this folder
# setwd(here("data-input", "LandScan Global 2019"))
# lspop2019 = terra::rast("lspop2019")
#lspop2019 %>% plot() #plot isn't working. what about mapview?

#So I tried to download it myself, located here:
setwd(here("data-input", "ls-global-2019-alt-dl"))
ls_2019_global = terra::rast("landscan-global-2019-colorized.tif")
ls_2019_global_raster = raster::raster("landscan-global-2019-colorized.tif")
ls_2019_global_raster %>% plot()
#Okay, there are four distinct files, and each file
#visualizes distinct colors, per this readme file:

#Prior to 2022-11-17, colorized TIFs had zero values as symbolized 
#as a gray color. In the current version, zero values have been 
#made transparent. Other colors have not been changed.

# The color map for LandScan Global is:
#   1 - 5: light yellow           rgb(255,255,190)
#   6 - 25: medium yellow         rgb(255,255,115)
#   26 - 50: yellow               rgb(255,255,0)
#   51 - 100: orange              rgb(255,170,0)
#   101 - 500: orange-red         rgb(255,102,0)
#   501 - 2500: red               rgb(255,0,0)
#   2501 - 5000: dark red         rgb(204,0,0)
#   5001 - 185000: maroon         rgb(115,0,0)

# Explore data-------
ls_2019_global %>% plot() #works!
ls_2019_global$`landscan-global-2019-colorized_1` %>% plot()
ls_2019_global$`landscan-global-2019-colorized_2` %>% plot()
ls_2019_global$`landscan-global-2019-colorized_3` %>% plot()
ls_2019_global$`landscan-global-2019-colorized_4` %>% values()



# Limit to Colorado-----------
source(here("scripts", "generate-boundaries-states-countries.R")) 

#  #I'm getting projection errors with mapview. Try:
#terra::project("epsg:4326") #apparently they insist on this format.
#https://rdrr.io/github/rspatial/terra/man/project.html
#what if I try Central Colorado? EPSG:2232
#I tried the above, and it didn't help so omitting that step.

ls_2019_colorado = ls_2019_global %>% 
  terra::crop(colorado_boundary) 

dim(ls_2019_colorado)
res(ls_2019_colorado)
ls_2019_colorado
ls_2019_colorado %>% terra::values() %>% table()
ls_2019_colorado$`landscan-global-2019-colorized_1` %>% 
  as_tibble() %>% table()
#Interesting the layers each contain the actual RGB values.

#Update Jan 9 5:28 pm - works with updates of packages...yay!
ls_2019_colorado$`landscan-global-2019-colorized_1` %>% 
  raster::raster() %>% mapview(layer.name = "landscan") 
#okay, qtm works.
  raster::raster() %>% tmap::qtm()  

#So I need to convert those RGB values to the appropriate categories.
ls_2019_colorado %>% terra::values() %>% summary()
ls_2019_colorado$`landscan-global-2019-colorized_1` %>% plot()
ls_2019_colorado$`landscan-global-2019-colorized_2` %>% plot()
ls_2019_colorado$`landscan-global-2019-colorized_3` %>% plot()
ls_2019_colorado$`landscan-global-2019-colorized_4` %>% plot()

#Do all of the layers have the same number of rows?
ls_2019_colorado$`landscan-global-2019-colorized_1` %>% 
  as_tibble() %>% nrow()
ls_2019_colorado$`landscan-global-2019-colorized_2` %>% 
  as_tibble() %>% nrow()
ls_2019_colorado$`landscan-global-2019-colorized_3` %>% 
  as_tibble() %>% nrow()
ls_2019_colorado$`landscan-global-2019-colorized_4` %>% 
  as_tibble() %>% nrow()
#Yes.
# Try making new vars using tidyterra------
#This seems like an appealing use case for tidyterra. I need to create a new variable
#conditional on values of these other RGB variables corresponding to the population categories.

#Again, these are the categories - pasting for convenience:
#   1 - 5: light yellow         rgb(255,255,190)
# 6 - 25: medium yellow         rgb(255,255,115)
# 26 - 50: yellow               rgb(255,255,0)
# 51 - 100: orange              rgb(255,170,0)
# 101 - 500: orange-red         rgb(255,102,0)
# 501 - 2500: red               rgb(255,0,0)
# 2501 - 5000: dark red         rgb(204,0,0)
# 5001 - 185000: maroon         rgb(115,0,0)

#These are the values contained in colorized -1
ls_2019_co_tidy = ls_2019_colorado %>% 
  tidyterra::as_tibble()

ls_2019_co_tidy %>% 
  print(n=1000)
# Summarize each layer's distinct values
ls_2019_co_tidy_1 = ls_2019_colorado$`landscan-global-2019-colorized_1` %>% 
  tidyterra::as_tibble() %>% 
  mutate(id = row_number())
ls_2019_co_tidy_2 = ls_2019_colorado$`landscan-global-2019-colorized_2` %>% 
  tidyterra::as_tibble() %>% 
  mutate(id = row_number())
ls_2019_co_tidy_3 = ls_2019_colorado$`landscan-global-2019-colorized_3` %>% 
  tidyterra::as_tibble() %>%
  mutate(id = row_number())
ls_2019_co_tidy_4 = ls_2019_colorado$`landscan-global-2019-colorized_4` %>% 
  tidyterra::as_tibble() %>% 
  mutate(id = row_number())

## Summarize values for each layer-------
ls_2019_co_tidy_1 %>% 
  group_by(`landscan-global-2019-colorized_1`) %>% 
  summarise(n=n())
ls_2019_co_tidy_2 %>% 
  group_by(`landscan-global-2019-colorized_2`) %>% 
  summarise(n=n())
ls_2019_co_tidy_3 %>% 
  group_by(`landscan-global-2019-colorized_3`) %>% 
  summarise(n=n()) #corresponds to above in the third position
ls_2019_co_tidy_4 %>% 
  group_by(`landscan-global-2019-colorized_4`) %>% 
  summarise(n=n())

#Can I wrangle the entire thing?
ls_2019_colorado %>% 
  mutate( hi = 5) #yes, looks like it

#okay, go ahead.
#Feb 22, 2023: I've moved this function to this script,
#as it's used elsewhere.
source(here("scripts", "analysis-functions.R")) 

ls_2019_co_wrangle = ls_2019_colorado %>% 
  landscan_pop_wrangle() 

names(ls_2019_co_wrangle)
class(ls_2019_co_wrangle)
ls_2019_co_wrangle %>% 
  as_tibble() %>% 
  group_by(pop_cat_mean_val) %>% 
  summarise(n=n())
ls_2019_co_wrangle %>% 
  as_tibble() %>% 
  group_by(pop_cat_max_val) %>% 
  summarise(n=n())

## visuals-------
summary(ls_2019_co_wrangle$pop_cat_mean_val)
ls_2019_co_wrangle$pop_cat_max_fac %>%
  raster::raster() %>% 
  mapview(zcol = "pop_cat_max_fac")
#hey - this works!
ls_2019_co_wrangle %>% 
  as_tibble() %>% 
  group_by(pop_cat_max_fac) %>% 
  summarise(n=n())
ls_2019_co_wrangle$pop_cat_max_fac %>%
  raster::raster() %>% 
  mapview(zcol = "pop_cat_1_8")

#filter to various pop. categories
ls_2019_co_wrangle %>% 
  tidyterra::filter(pop_cat_1_8 == 2) %>% 
  .$pop_cat_1_8 %>%  #strange but works
  raster::raster()  %>% 
  mapview(zcol = "pop_cat_1_8", layer.name = "pop_cat_1_8")

ls_2019_co_wrangle %>% 
  tidyterra::filter(pop_cat_1_8 == 6) %>% 
  .$pop_cat_1_8 %>%  #strange but works
  raster::raster()  %>% 
  mapview(zcol = "pop_cat_1_8", layer.name = "pop_cat_1_8")

  
## Save------
#note unusual syntax because it's a raster.
setwd(here("data-processed"))
terra::writeRaster(
  ls_2019_co_wrangle,
  overwrite=TRUE,
  #  datatype = "INT1U", #see Robin's discussion of what this means. default OK
  filename = "ls_2019_co_wrangle.tif" 
)

# All of USA (continental 48)-------
ls_2019_usa_48 = ls_2019_global %>% 
  terra::crop(usa_boundaries_cont_48)

time_start = Sys.time()
ls_2019_usa_48_wrangle = ls_2019_usa_48 %>% 
  landscan_pop_wrangle() #this takes a few minutes.
time_stop = Sys.time()
time_to_make_ls_2019_usa_wrangle = time_stop-time_start
setwd(here("data-processed"))
terra::writeRaster(
  ls_2019_usa_48_wrangle,
  overwrite=TRUE,
  filename = "ls_2019_usa_48_wrangle.tif" 
)

ls_2019_usa_48_wrangle$pop_cat_1_8 %>% plot()

#test to be sure it works
ls_2019_michigan = ls_2019_usa_48 %>% 
  terra::crop(michigan_boundary)
ls_2019_michigan$`landscan-global-2019-colorized_1` %>% 
  raster::raster() %>% mapview(layer.name = "landscan") 
ls_2019_usa_48$`landscan-global-2019-colorized_1` %>% 
  raster::raster() %>% mapview(layer.name = "landscan")

# Global--------
#This takes several minutes to run.
#Feb 9 2023: this didn't work. I need to convert it to a tibble first
#and then do the data wrangling steps
#So that's all for this script..
#Continued
#~scripts/analysis-global.R

