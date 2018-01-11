#ARCTIC CONNECT Project
#Flickr data
#This script reads a .csv listing the flickr photos for a given location (1,782,987 rows), transforms to spatial data and makes plots.


#  https://www.r-bloggers.com/mapping-france-at-night-with-the-new-sf-package/
wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis"
setwd(wd)

#library(rgdal)
library(sf)
library(zoo) #as.yearmon
library(maps)
library(animation)
#library(leaflet)
library(devtools)
library(plyr)
#devtools::install_github("tidyverse/ggplot2")
library(tidyverse)
#library(showtext) #for other fonts 
#library(Cairo) # for embedding fonts in PDF; may not need to be loaded here
library(wesanderson) #colors

#font_add_google("Hind", "Hind") #google fonts
#font_add_google("Roboto", "Roboto") 

options(stringsAsFactors = FALSE)
options(tibble.width = Inf) #print all columns

#load data
dat <- read_csv("D:/Box Sync/Arctic/Data/Flickr/FlickrPhotosIceland.csv")
#drop rows that are outside 60N, drop 904 rows, leaving 1782083
dat <- dat[dat$latitude >= 60, ] 
#problems(dat) #check if load issues
#spec(dat) #check column formats
#add time rows
dat$month <- format(dat$datetaken,"%m")
dat$year <- format(dat$datetaken,"%Y")
dat$yearmon <- format(dat$datetaken,"%Y%m")

#add latitude column
dat$photo_lat <- dat$latitude

#load country borders shapefile
worldmap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBorders_60degreesN_lambert.shp")


############################
#convert to spatial points
############################
# WGS84 = EPSG: 4326
#North pole azimithul lambert equal area ESRI:102017 +proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 


#full dataset
all.sf <- st_as_sf(dat, coords=c('longitude', 'latitude'))
st_crs(all.sf) <- 4326 #WGS84
#transform to north pole lambert
all.sf <- st_transform(all.sf, crs=102017)

############################
#Plot
############################
#check latitude 
p <- ggplot(all.sf, aes(photo_lat)) + 
  geom_histogram() + 
  #facet_grid(region ~ . , scales = 'free_y') +
  #geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr by latitude",
       subtitle="By region",
       x="Latitude",
       y="Number of photos") +
    theme_minimal(base_size=10)
ggsave("Flickr_60N_histogram_latitide_iceland.png", p, height = 4, width = 4)

  

