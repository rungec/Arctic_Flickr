#This script plots density maps of 
# a) flickr photos across the Arctic 
# b) words used to tag flickr photos across the Arctic
# Follows on from Flickr_tidy_flickrtags.R & Flickr_googlecloudvision_label.r


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(sf)
library(ggplot2)

options(stringsAsFactors = TRUE) #otherwise stat_density_2d throws an error
#options(tibble.width = Inf) #print all columns

#load country borders shp
#worldmap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBorders_60degreesN_lambert.shp")
#load bounding box shp
boundary60N <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/60degreesN.shp")
#load flickr points as .shp
flickrshp <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate.shp")

### Preliminary processing ----


### Main processing ----
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
  
  
