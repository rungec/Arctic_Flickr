#ARCTIC CONNECT Project
#Flickr data
#This script reads a .csv listing the flickr photos for a given location (1,782,987 rows), transforms to spatial data and makes plots.

setwd("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis")

library(readr)
library(rgdal)
library(sf)
library(purrr)
#library(plyr)
#library(maps)
library(leaflet)
#library(tidyverse)
library(htmlwidgets)

options(stringsAsFactors = FALSE)
options(tibble.width = Inf) #print all columns

############################
#load data
############################
dat <- read_csv("D:/Box Sync/Arctic/Data/Flickr/FlickrPhotosNorthOf60.csv")
#drop rows that are outside 60N, drop 904 rows, leaving 1782083
dat <- dat[dat$latitude >= 60, ] 

# WGS84 = EPSG: 4326
#North pole azimithul lambert equal area ESRI:102017 +proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 

#test dataset
subdat <- dat[1:10000,]
all.sf <- st_as_sf(subdat, coords=c('longitude', 'latitude'))
st_crs(all.sf) <- 4326 #WGS84


#full dataset
#all.sf <- st_as_sf(dat, coords=c('longitude', 'latitude'))
#st_crs(all.sf) <- 4326 #WGS84

############################
#set up polar projection
############################

extent <- 11000000 + 9036842.762 + 667
origin = c(-extent, extent)
maxResolution <- ((extent - -extent) / 256)
defZoom <- 4
bounds <- list(c(-extent, extent),c(extent, -extent))
minZoom <- 0
maxZoom <- 18
resolutions <- purrr::map_dbl(minZoom:maxZoom,function(x) maxResolution/(2^x))

# 6 Projection EPSG Codes
projections <- c('3571', '3572', '3573', '3574', '3575', '3576')
# Corresponding proj4defs codes for each projection
proj4defs <- list(
  '3571' = '+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',
  '3572' = '+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',
  '3573' = '+proj=laea +lat_0=90 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',
  '3574' = '+proj=laea +lat_0=90 +lon_0=-40 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',
  '3575' = '+proj=laea +lat_0=90 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',
  '3576' = '+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
)

# create a CRS instance for each projection
crses <- purrr::map(projections, function(code) {
  leafletCRS(
    crsClass = 'L.Proj.CRS',
    code = sprintf("EPSG:%s",code),
    proj4def = proj4defs[[code]],
    origin = origin,
    resolutions = resolutions,
    bounds = bounds
  )
})

# Tile URL Template for each projection
tileURLtemplates <- purrr::map(projections, function(code) {
  sprintf('http://{s}.tiles.arcticconnect.org/osm_%s/{z}/{x}/{y}.png',
          code)
})

# We can't add all 6 tiles to our leaflet map,
# because each one is in a different projection,
# and you can have only one projection per map in Leaflet.
# So we create 6 maps.
polarmaps <- purrr::map2(crses, tileURLtemplates,
                         function(crs, tileURLTemplate) {
                           leaflet(options= leafletOptions(
                             crs=crs, minZoom = minZoom, maxZoom = maxZoom)) %>%
                             setView(0, 90, defZoom) %>%
                             addTiles(urlTemplate = tileURLTemplate,
                                      attribution = "Map © ArcticConnect. Data © OpenStreetMap contributors",
                                      options = tileOptions(subdomains = "abc", noWrap = TRUE,
                                                            continuousWorld = FALSE))
                         })

############################
# Build the map components
############################
#popup content
content <- paste0("<b><a href=",
                  all.sf$url_m,
                  ">",
                  all.sf$title,
                  "</a></b>"
)


m <- polarmaps[[4]] %>%
  addGraticule(interval = 20) %>%
  addCircleMarkers(data=all.sf,  color = "red", popup = content,
                   stroke = FALSE, fillOpacity = 0.5)
  

#Display the map
m

#### Export the html widget ####
saveWidget(m, file="flickr_map.html",selfcontained = FALSE)





