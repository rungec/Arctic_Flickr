
require(sf)
require(raster)
require(sp)
require(tidyverse)
require(lubridate)
require(mgcv)
require(rnaturalearth)
require(lwgeom)

######################
##Set up data
######################

#wd <- "D:/Box Sync/Arctic/Data"
#setwd(wd)
# flickr data
load("Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
#load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

# protected areas
PA <- st_read("Arctic_Protected_areas_2017/WDPA_plusCAFF_PAs_Amap60N_dissolve.shp")
#PA <- st_read("D:/Box Sync/Arctic/Data/Ecological/Arctic_Protected_areas_2017/CAFF_Protected_Areas_20_01_2017_nplaea.shp")
# because PA has some invalid geometries (i.e. st_is_valid()) we need to buffer PA to fix these
PAbuf <- st_buffer(PA,0)

# bounding box for entire Arctic
# bb <- st_sfc(st_multipoint(matrix(c(-180:180,rep(60,361)),ncol=2)),
#              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") %>%
#   st_sf() %>% 
#   st_transform(st_crs(flickramap)) %>% 
#   st_convex_hull()

bb <- st_read("flickr_AMAP60N_dissolve.shp")
#bb <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp")

#ggplot(bb)+
  #geom_sf()

# country

# countries <- rbind(raster::getData('GADM', country='CAN', level=0),
#                    raster::getData('GADM', country='USA', level=0),
#                    raster::getData('GADM', country='ISL', level=0),
#                    raster::getData('GADM', country='NOR', level=0),
#                    raster::getData('GADM', country='FRO', level=0),
#                    raster::getData('GADM', country='FIN', level=0),
#                    raster::getData('GADM', country='GRL', level=0),
#                    raster::getData('GADM', country='SJM', level=0),
#                    raster::getData('GADM', country='SWE', level=0),
#                    raster::getData('GADM', country='RUS', level=0)) %>% 
#   st_as_sf() %>% 
#   st_transform(st_crs(flickramap)) %>% 
#   st_simplify(preserveTopology = TRUE,10000) #TODO:when doing for real, comment out this line!

# for natural earth, SJM seems to be lumped in woth NOR
countries <- ne_states(returnclass = "sf")  %>% 
  filter(adm0_a3 %in% c("CAN","USA","ISL","NOR","FRO","FIN","GRL","SWE","RUS")) %>% 
  st_transform(st_crs(flickramap)) %>% 
  lwgeom::st_make_valid() %>%
  st_intersection(bb) %>% 
  mutate(adm0_a3 = replace(adm0_a3, name=="Svalbard", "SJM")) %>% 
  group_by(adm0_a3) %>% summarise()
 
ggplot(countries)+
  geom_sf(aes(fill=adm0_a3))

# flickr
#AllFlickr <- read.csv("D:/Box Sync/Arctic/Data/Flickr/global_flickr/Flickr_global_nphotostaken_byhr_2000to2018.csv") %>% 
AllFlickr <- read.csv("Flickr_global_nphotostaken_byhr_2000to2018.csv") %>% 
  mutate(year=year(datetime)) %>% 
  filter(year>=min(flickramap$year)) %>% 
  group_by(year) %>% 
  summarize("Total Flickr Photos" = sum(total))

# Global Roads Inventory Project (GRIP) dataset 
roads <- st_read("Arctic_roads_GRIP.shp") %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)

#ggplot(roads)+
 # geom_sf()
# naturalearth layers, change ne_load() with ne_download() if the data is not downloaded yet
airports <- ne_load(type="airports",
                     scale="large",
                     returnclass = "sf",
                     destdir = getwd())  %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)

#ggplot(airports)+
#  geom_sf()

ports <- ne_load(type="ports",
                     scale="large",
                     returnclass = "sf",
                     destdir = getwd())  %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)

#ggplot(ports)+
 # geom_sf()

populated_places <- ne_load(type="populated_places",
                     scale="large",
                     returnclass = "sf",
                     destdir = getwd())  %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)

#ggplot(populated_places)+
 # geom_sf()

urban_areas <- ne_load(type="urban_areas",
                     scale="large",
                     returnclass = "sf",
                     destdir = getwd())  %>% 
  st_transform(st_crs(flickramap)) %>% 
  st_intersection(bb)

#ggplot(urban_areas)+
 # geom_sf()


######################
##Make grid templates
######################
#function to make hexagonal grids
makegridfun <- function(currdiameter){
    # switch to sp for the spsample function
    ext <- as(st_buffer(bb, currdiameter), "Spatial")
    projection(ext) <- sf::st_crs(flickramap)$proj4string
    
    # generate hexagon grid for footprints
    currgrid <- spsample(ext,
                     type = "hexagonal",
                     cellsize = currdiameter,
                     offset = c(0.5, 0.5)) %>% 
      HexPoints2SpatialPolygons(dx = currdiameter) %>% 
      st_as_sf() %>% 
      st_intersection(bb) %>% 
      rowwise() %>% 
      mutate(Latitude = st_coordinates(st_centroid(geometry))[2],
             Longitude = st_coordinates(st_centroid(geometry))[1]) %>% 
      st_as_sf(crs=st_crs(bb))
  return(currgrid)
}

cell_diameter = 10000
cell_diameter_footprint = 5000

grid_models <- makegridfun(cell_diameter) #we use a bigger resolution for the models
save(grid_models, file = paste0("ArcticAMAP_templatehexgrid_",cell_diameter,"_m.Rdata"))
grid_footprint <- makegridfun(cell_diameter_footprint) #we use a bigger resolution for the models
save(grid_footprint, file = paste0("ArcticAMAP_templatehexgrid_",cell_diameter_footprint,"_m.Rdata"))
rm(grid_footprint) # we no longer need it so let's free up memory

######################
##Set up overlap functions
######################

# create a function to measure proportion of elements of sf1 are covered by sf2
prop_overlap <- function(sf1,sf2){
  area <- as.numeric(st_area(sf1))
  dist <- map(st_geometry(sf1),
              function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                st_intersection(sf2) %>% 
                st_area() %>% 
                as.numeric()) %>% 
    lapply(function(x) ifelse(is.null(x), NA, x)) %>% 
    lapply(function(x) ifelse(is.na(x), 0, x)) %>%
    unlist()
  return(dist/area)
}

# create a function to measure which elements of sf1 are most covered by elements of sf2
most_overlap <- function(sf1,sf2,column,missing){
  most <- suppressWarnings(map(st_geometry(sf1),
                               function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                                 st_sf() %>% 
                                 st_intersection(.,sf2) %>% 
                                 mutate(area=st_area(.)) %>%
                                 filter(area==max(area)) %>% 
                                 data.frame() %>% 
                                 select(column))) %>% 
    lapply(function(x) ifelse(nrow(x)==0, missing, x)) %>% 
    unlist()
  return(most)
}

# create a function to measure the length of sf2 contained in elements of sf1
length_overlap <- function(sf1,sf2){
  lengths <- map(st_geometry(sf1),
                 function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                   st_intersection(.,sf2) %>% 
                   st_combine() %>% 
                   st_sf() %>% 
                   mutate(length=as.numeric(sum(st_length(.)))) %>%
                   data.frame() %>% 
                   select(length)) %>% 
    lapply(function(x) ifelse(nrow(x)==0, 0, x)) %>% 
    unlist()
}
######################
##Spatial overlaps
######################
#Then, for the models, let's see which countries overlap which grid cell and if there is overlap with protected areas. If there is >20% overlap with PA then we consider that cell as a PA, we justify this because of the halo effect of PAs, people stay just outside PAs where there are accomodations.

# 2 country
grid_models$country <- most_overlap(grid_models,countries,"adm0_a3","Ocean")

# 3 roads
grid_models$roadlength <- length_overlap(grid_models,roads)
grid_models$dist2road <- st_distance(grid_models,st_combine(roads))

# 4 airports
grid_models$airports <- lengths(st_intersects(grid_models,airports))
grid_models$dist2airports <- st_distance(grid_models,st_combine(airports))
save(grid_models, file = paste0("ArcticAMAP_griddedaccessibilitydata_",cell_diameter,"_m.Rdata")) #another just in case

# 5 ports
grid_models$ports <- lengths(st_intersects(grid_models,ports))
grid_models$dist2ports <- st_distance(grid_models,st_combine(ports))

# 6 populated places
grid_models$populated_places <- lengths(st_intersects(grid_models,populated_places))
grid_models$dist2populated_places <- st_distance(grid_models,st_combine(populated_places))

# 7 urban area
grid_models$urban_areas <- prop_overlap(grid_models,urban_areas)
grid_models$dist2urban_areas <- st_distance(grid_models,st_combine(urban_areas))
save(grid_models, file = paste0("ArcticAMAP_griddedaccessibilitydata_",cell_diameter,"_m.Rdata")) #the next step takes a lot of time, so lets save it just in case

# 1 protected areas
grid_models$propPA <- prop_overlap(grid_models,PAbuf)
grid_models$PA <- grid_models$propPA>0.2

#check
head(grid_models)
save(grid_models, file = paste0("ArcticAMAP_griddedaccessibilitydata_",cell_diameter,"_m.Rdata"))

###END
