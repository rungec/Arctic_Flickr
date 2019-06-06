
require(sf)
require(tidyverse)
require(lubridate)
require(mgcv)


#wd <- "D:/Box Sync/Arctic/Data"
#setwd(wd)
wd <- paste0(getwd(), "/Documents")
setwd(wd)
# flickr data
#load("Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
#load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
load("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
flickraccess <- flickramap

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

#########################
##SET UP FLICKR DATA
#########################

#add yearseason to flickramap
flickraccess$season <- "summer"
flickraccess$season[flickraccess$month %in% c("01", "02", "03", "04", "11", "12")] <- "winter"
flickraccess$year_old <- as.numeric(flickraccess$year)
flickraccess <- flickraccess %>% 
  mutate(year=(if_else(month %in% c("11", "12"), year_old + 1, as.numeric(year)))) %>%
  mutate(yearseason = paste(year, season, sep="_"))

#Which users take photos of nature?
#index rows as TRUE\FALSE where 'biotic_wildlife' or 'biotic_bird' is in any of the escode columns
indx <- flickraccess %>% st_set_geometry(NULL) %>%
  select(starts_with("escode")) %>%  #select escode columns
  mutate_all(funs(grepl("^biotic_wildlife", .)))  #return df of TRUE FALSE
indx2 <- flickraccess %>% st_set_geometry(NULL) %>%
  select(starts_with("escode")) %>%  #select escode columns
  mutate_all(funs(grepl("^biotic_bird", .)))  #return df of TRUE FALSE
wildlife_photos <- bind_cols(indx, indx2) %>%
  rowSums > 0 
flickraccess$wildlifebird_photos <- wildlife_photos

#Calculate the proportion of a given user's photos that are of wildlife
wildlife_photo_prop <- flickraccess %>% st_set_geometry(NULL) %>%
  group_by(owner) %>%
  summarise(wildlife_photo_prop = sum(wildlifebird_photos)/n())
flickraccess <- flickraccess %>% 
  left_join(wildlifebird_photo_prop, by="owner")

#########################
##SET UP ACCESSIBILITY DATA
#########################

# protected areas
PA <- st_read("Arctic_Protected_areas_2017/WDPA_plusCAFF_PAs_Amap60N_dissolve.shp")
#PA <- st_read("D:/Box Sync/Arctic/Data/Ecological/Arctic_Protected_areas_2017/CAFF_Protected_Areas_20_01_2017_nplaea.shp")
# because PA has some invalid geometries (i.e. st_is_valid()) we need to buffer PA to fix these
PAbuf <- st_buffer(PA,0)

bb <- st_read("flickr_AMAP60N_dissolve.shp")
#bb <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp")

# country
# for natural earth, SJM seems to be lumped in woth NOR
countries <- ne_states(returnclass = "sf")  %>% 
  filter(adm0_a3 %in% c("CAN","USA","ISL","NOR","FRO","FIN","GRL","SWE","RUS")) %>% 
  st_transform(st_crs(flickraccess)) %>% 
  lwgeom::st_make_valid() %>%
  st_intersection(bb) %>% 
  mutate(adm0_a3 = replace(adm0_a3, name=="Svalbard", "SJM")) %>% 
  group_by(adm0_a3) %>% summarise()

# Global Roads Inventory Project (GRIP) dataset 
roads <- st_read("Arctic_roads_GRIP.shp") %>% 
  st_transform(st_crs(flickraccess)) %>% 
  st_intersection(bb)

# naturalearth layers, change ne_load() with ne_download() if the data is not downloaded yet
airports <- ne_load(type="airports",
                    scale="large",
                    returnclass = "sf",
                    destdir = getwd())  %>% 
  st_transform(st_crs(flickraccess)) %>% 
  st_intersection(bb)

ports <- ne_load(type="ports",
                 scale="large",
                 returnclass = "sf",
                 destdir = getwd())  %>% 
  st_transform(st_crs(flickraccess)) %>% 
  st_intersection(bb)

populated_places <- ne_load(type="populated_places",
                            scale="large",
                            returnclass = "sf",
                            destdir = getwd())  %>% 
  st_transform(st_crs(flickraccess)) %>% 
  st_intersection(bb)

urban_areas <- ne_load(type="urban_areas",
                       scale="large",
                       returnclass = "sf",
                       destdir = getwd())  %>% 
  st_transform(st_crs(flickraccess)) %>% 
  st_intersection(bb)

######################
##Spatial overlaps
######################
#link each photo to accessibility data

# 2 country
flickraccess$country <- most_overlap(flickraccess,countries,"adm0_a3","Ocean")

# 3 roads
flickraccess$roadlength <- length_overlap(flickraccess,roads)
flickraccess$dist2road <- st_distance(flickraccess,st_combine(roads))

# 4 airports
flickraccess$airports <- lengths(st_intersects(flickraccess,airports))
flickraccess$dist2airports <- st_distance(flickraccess,st_combine(airports))
save(flickraccess, file = paste0("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")) #just in case

# 5 ports
flickraccess$ports <- lengths(st_intersects(flickraccess,ports))
flickraccess$dist2ports <- st_distance(flickraccess,st_combine(ports))

# 6 populated places
flickraccess$populated_places <- lengths(st_intersects(flickraccess,populated_places))
flickraccess$dist2populated_places <- st_distance(flickraccess,st_combine(populated_places))

# 7 urban area
flickraccess$urban_areas <- prop_overlap(flickraccess,urban_areas)
flickraccess$dist2urban_areas <- st_distance(flickraccess,st_combine(urban_areas))
save(flickraccess, file = paste0("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")) #the next step takes a lot of time, so lets save it just in case

# 1 protected areas
#Overlap with protected areas: If there is >20% overlap with PA then we consider that cell as a PA, we justify this because of the halo effect of PAs, people stay just outside PAs where there are accomodations.
flickraccess$propPA <- prop_overlap(flickraccess,PAbuf)
flickraccess$PA <- flickraccess$propPA>0.2

#check
head(flickraccess)
save(flickraccess, file = paste0("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata"))

###END
