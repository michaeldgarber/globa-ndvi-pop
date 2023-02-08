#I'm putting together this to make sure I know what order to run my scripts

setwd(here("data-processed"))
source(here("scripts", "rojas_green_space_drf.R")) #load dose-response info. doesn't depend on anything
source(here("scripts", "generate-boundaries-states-countries.R")) 
source(here("scripts", "read-united-nations-deaths.R.R")) 
source(here("scripts", "read-gub.R"))
source(here("scripts", "read-ecoregions-biomes.R"))
source(here("scripts", "read-ls-pop.R")) 
source(here("scripts", "read-ndvi-global.R")) 
source(here("scripts", "rasterize-vectors.R")) #those that aren't already rasters
source(here("scripts", "merge-rasters.R")) #merge all rasters
source(here("scripts", "analysis-test.R")) #HIA
