### Set up libraries ----
require(sf)
require(tidyverse)
require(rnaturalearth)
wd <- paste0(getwd(), "/Documents")
setwd(wd)

load("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

bb <- st_read("flickr/flickr_AMAP60N_dissolve.shp")

roads <- st_read("Arctic_roads_GRIP.shp") %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)


distancefromroad <- st_distance(flickramap, roads)

d2 <- apply(distancefromroad, 1, min)

quantile(d2)
mean(d2)
median(d2)
length(d2[d2<1001])
