#This script plots density maps of 
# a) flickr photos across the Arctic 
# b) words used to tag flickr photos across the Arctic
# Follows on from Flickr_tidy_flickrtags.R & Flickr_googlecloudvision_label.r


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(sp)
library(rgdal)
library(spatstat)

options(stringsAsFactors = FALSE)
#options(tibble.width = Inf) #print all columns

#load country borders shp
#worldmap <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/", "CountryBorders_60degreesN_lambert")
#load bounding box shp
boundary60N <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN", "60degreesN")
#load flickr points as .shp
flickrshp <- readOGR("D:/Box Sync/Arctic/Data/Flickr", "Flickr_Artic_60N_byregion_laea_icelandupdate")

### Preliminary processing ----
#coerce study area to owin object
studywindow <- as.owin(boundary60N)
#coerce photo points to ppp object and pass to study area window
flickrppp <- as.ppp(flickrshp, W=studywindow, marks=Year1@data$MyVar)
#with data
#photos <- ppp(coordinates(flickrshp)[,1], coordinates(flickrshp)[,2] W=studywindow, marks=Year1@data$MyVar)

#alternately with bounding box
bb <- bounding.box(flickrppp)
flickrppp <- ppp(flickrppp, W=bb)

### Main processing ----

sigmas <- c(0.25, 0.5, 0.75, 1)

densgrids <- lapply(sigmas, function(x){
  dens <- density(flickrppp, sigma=x, xy=crds)
  spatgrid <- as(dens, "SpatialGridDataFrame")
  return(spatgrid)
})
names(densgrids) <- paste0("density_s", sigmas)

plot(densgrids)