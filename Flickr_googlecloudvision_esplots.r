# This script follows on from Flickr_googlecloudvision_esmapping.r
# It makes plots and data summaries of the ecosystem service provision and use across the different Arctic regions


### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/es_mapping"
#wd <- "/data/Claire/rasters/es_mapping"
setwd(wd)
wd2 <- "D:/Box Sync/Arctic/Data"
#wd2 <- "/data/Claire/rasters"

### Preliminary processing ----

#calculate summary statistics by region
#make raster stack of all es rasters, then summarise by shp of each region

#make nice plots of the rasters



