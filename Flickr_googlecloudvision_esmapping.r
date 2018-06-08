#this script performs spatial analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#I categorise the google labels by ecosystem feature or activity (escode) that they represent
#then map the distribution of those ecosystem features or activities
#follows on from Flickr_googlecloudvision_datasummary.r

### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)
require(rgdal)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/es_mapping"
#wd <- "/data/Claire/rasters/es_mapping"
setwd(wd)
wd2 <- "D:/Box Sync/Arctic/Data"
#wd2 <- "/data/Claire/rasters"


##########################
### Preliminary processing ----

load(paste0(wd2, "/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap.Rdata"))
#photos from amap region, 
#regular and super users only, 
#2004 to 2017
#dropped rows with no google vision labels
#includes photos in towns/cities

#load country borders shp
#worldmap <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBordersESRI_60degreesN_lambert.shp")
#load AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf(paste0(wd2, "/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp"))
rcrs <- st_crs(amap)

#######################
#Function to rasterise es where the value in each cell = PUD
#PUD = photo unit days
#number of days in a cell when a photo was taken that represents that es

rastfun <- function(data, es, curres, currfile) {
  #extract photos containing that es
  escols <- grep("escode", names(data), value=TRUE)
  data_es <- filter_at(data, escols, any_vars(. %in% es))
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs$proj4string)
    if(file.exists(sprintf("%s/Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", wd2, curres))==FALSE){ 
      rastamap <- rasterize(amap, rasttemplate, filename=sprintf("%s/Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", wd2, curres))
    } else {
      rastamap <- raster(sprintf("%s/Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", wd2, curres))
    }
    rastamap[rastamap==1] <- 0
       #fill with pud
    esrast <- rasterize(data_es, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("Flickr_%s_PUDper%scell.tif", currfile, curres), overwrite=TRUE)
  }

#######################
# Apply rastfun over es
escats <- read.csv(paste0(dirname(wd), "/tag_analysis/googlevision/regional_word_frequency/NumPhotos_byescode_anduser_amap.csv"), header=TRUE, stringsAsFactors = FALSE)
escats <- escats$escode[1:(nrow(escats)-2)]

lapply(escats, function(x) {
  rastfun(flickramap, x, 10000, x)
} ) 
  
#drop rows for biotic fauna with pet?

#######################
# Combined es rasters 
# note that becuase these words can occur in a single photo, individual rasters for es cannot simply be added together.

#Biotic
bioes <- c("biotic_wildlife", "biotic_bird", "biotic_reptile", "biotic_invertebrate", "biotic_plant", "biotic_fungus", "biotic_ecosystem")
rast <- rastfun(flickramap, bioes, 10000, "biotic_all")

bionature <- c("biotic_plant", "biotic_fungus", "biotic_ecosystem")
rast <- rastfun(flickramap, bionature, 10000, "biotic_nature")

biowildlife <- c("biotic_wildlife", "biotic_reptile", "biotic_invertebrate", "biotic_bird")
rast <- rastfun(flickramap, bioes, 10000, "biotic_fauna_all")

#abiotic
abiotic_all <- c("abiotic_aurora", "abiotic_coastal", "abiotic_geology", "abiotic_ice", 
"abiotic_volcanic", "abiotic_water") 
rast <- rastfun(flickramap, abiotic_all, 10000, "abiotic_all")

#harvesting and hunting
harvest_es <- c("biotic_harvesting_animal", "biotic_livestock", "biotic_harvesting_berry", 
                "biotic_harvesting_fish", "biotic_harvesting_hay",     
                "biotic_harvesting_mushroom",  "biotic_harvesting_produce", 
                "biotic_harvesting_seafood", "biotic_harvesting_wood")
rast <- rastfun(flickramap, harvest_es, 10000, "harvest_all")

harvestorhunt_es <- c(harvest_es, "recreation_hunting", "recreation_fishing")
rast <- rastfun(flickramap, harvest_es, 10000, "harvesthuntingorfishing_all")

fishing_es <- c("recreation_fishing", "biotic_harvesting_fish", "biotic_harvesting_seafood")
rast <- rastfun(flickramap, fishing_es, 10000, "fishing_all")

farming_es <- c("biotic_harvesting_animal", "biotic_livestock", "biotic_managed")
rast <- rastfun(flickramap, farming_es, 10000, "livestockfarming_all")

#recreation
recreation <- c("recreation_air", "recreation_bonfire", "recreation_camping", 
                "recreation_cycling", "recreation_diving", "recreation_dogsled", 
                "recreation_fishing", "recreation_general", "recreation_hiking", 
                "recreation_horseriding", "recreation_hunting", "recreation_mountainsports", 
                "recreation_offroad", "recreation_photography", "recreation_snowsport", 
                "recreation_tourism", "recreation_watersports")
rast <- rastfun(flickramap, recreation, 10000, "recreation_all")


#Make a raster of all es for normalising
alles <- c("abiotic_aurora", "abiotic_coastal", "abiotic_geology", "abiotic_ice", 
           "abiotic_volcanic", "abiotic_water", "biotic_bird", "biotic_ecosystem", 
           "biotic_fungus", "biotic_harvesting_animal", "biotic_harvesting_berry", 
           "biotic_harvesting_fish", "biotic_harvesting_hay", "biotic_harvesting_mushroom", 
           "biotic_harvesting_produce", "biotic_harvesting_seafood", "biotic_harvesting_wood", 
           "biotic_invertebrate", "biotic_livestock", "biotic_managed", 
           "biotic_marine", "biotic_plant", "biotic_reptile", "biotic_traces", 
           "biotic_wildlife", "no", "pet", "recreation_air", "recreation_bonfire", 
           "recreation_camping", "recreation_cycling", "recreation_diving", 
           "recreation_dogsled", "recreation_fishing", "recreation_general", 
           "recreation_hiking", "recreation_horseriding", "recreation_hunting", 
           "recreation_mountainsports", "recreation_offroad", "recreation_photography", 
           "recreation_snowsport", "recreation_tourism", "recreation_watersports")
rast <- rastfun(flickramap, alles, 10000, "allphotos")

#END######################
