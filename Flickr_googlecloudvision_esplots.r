# This script follows on from Flickr_googlecloudvision_esmapping.r
# It makes plots and data summaries of the ecosystem service provision and use across the different Arctic regions


### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/es_mapping"
#wd <- "/data/Claire/rasters/es_mapping"
setwd(wd)


### Preliminary processing ----

#convert to proportion of photos containing each es
#load raster all photos across time
rast_total <- raster("static_PUD_allphotos_alles/Flickr_allphotos_PUDper10000cell.tif") #PUD of all photos
#load es rasters
absfiles <- list.files("static_PUD_absolute", full.names=TRUE)
rast_abs <- stack(absfiles)

#normalise es rasters to get proportion of photos containing that es
rast_prop <- rast_abs/rast_total
for(i in 1:length(absfiles)){
  outname <- paste0("static_PUD_prop/", strsplit(basename(absfiles[[i]]), "_PUDper10000cell.tif")[[1]], "_propphotoscontaininges_per10000cell.tif")
  writeRaster(rast_prop[[i]], outname, overwrite=TRUE)
}


#make nice plots of the rasters


#calculate summary statistics by region
#make raster stack of all es rasters, then summarise by shp of each region

