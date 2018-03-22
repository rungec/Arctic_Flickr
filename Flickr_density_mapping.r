#This script plots density maps of 
# a) flickr photos across the Arctic 
# b) words used to tag flickr photos across the Arctic
# Follows on from Flickr_tidy_flickrtags.R & Flickr_googlecloudvision_label.r


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
#library(sf)
library(raster)
library(rasterVis)
library(rgdal)
library(ggplot2)

options(stringsAsFactors = TRUE) #otherwise stat_density_2d throws an error
#options(tibble.width = Inf) #print all columns

#load country borders shp
#worldmap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBordersESRI_60degreesN_lambert.shp")
#load bounding box shp
boundary60N <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN", "60degreesN")
#load flickr points as .shp
flickrshp <- readOGR("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate.shp")


##########################
### Preliminary processing ----
#turn boundary 60N into a raster
rcrs <- crs(boundary60N)


##########################
### Main processing ----
#rasterize points
rastFun <- function(curres, currphotos, currfile){
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs)
  rast60N <- rasterize(boundary60N, rasttemplate, filename=sprintf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/60degreesN_%sres.tif", currfile))
  rast60N[rast60N==1] <- 0
  densRast <- rasterize(flickrshp, rast60N, fun='count', field="id", update=TRUE, filename=sprintf("density_mapping/Flickr_%s_per%scell.tif", currphotos, currfile), overwrite=TRUE)
}

rast5km <- rastFun(5000, "allphotos", "5km")
rast10km <- rastFun(10000, "allphotos", "10km")


##########################
### Plot rasters ----
levelplot(densRast, contour=FALSE)








##########
#Testing out stat_density_2d
# it seems to be plotting the desnity in different projection to the points
testdat <- flickrshp[sample(1:nrow(flickrshp), 1000),]
coords <- as.data.frame(st_coordinates(testdat))

testplot <- ggplot(coords, aes(x=X, y=Y)) +
            geom_point() +
            #stat_density_2d(aes(x=coords[,1], y=coords[,2], fill = ..level..), geom = "polygon") +
            stat_density_2d(aes(fill = ..level..), geom = "polygon", bins=5, n=c(200, 100), h=c) +
            theme_bw()

testplot <- ggplot(testdat) +
            geom_sf() +
            #stat_density_2d(aes(x=coords[,1], y=coords[,2], fill = ..level..), geom = "polygon") +
            stat_density_2d(aes(x=coords[,2], y=coords[,1], fill = ..level..), geom = "polygon") +
            theme_bw()
  
 coords <- st_coordinates(testdat)
  
  
