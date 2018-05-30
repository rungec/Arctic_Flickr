#this script performs spatial analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#I categorise the google labels by ecosystem feature or activity (escode) that they represent
#then map the distribution of those ecosystem features or activities
#follows on from Flickr_googlecloudvision_datasummary.r

### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/es_mapping"
#wd <- "/data/Claire/rasters/es_mapping"
setwd(wd)
wd2 <- "D:/Box Sync/Arctic/Data"
#wd2 <- "/data/Claire/rasters"


##########################
### Preliminary processing ----

load("Flickr_Artic_60N_plus_flickrandgooglelabels_escodes_amap.Rdata")
#photos from amap region, 
#regular and super users only, 
#dropped rows with no google vision labels
#includes photos in towns/cities

#add a new column, owner_date
flickramap$owner_date <- paste(flickramap$owner, flickramap$datetkn, sep="_")

#load country borders shp
#worldmap <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBordersESRI_60degreesN_lambert.shp")
#load AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf(paste0(wd2, "/Boundaries/Arctic_circle/AMAP/AMAP_updatedRussia_clipto60N.shp"))
rcrs <- crs(amap)

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
    esrast <- rasterize(data_es, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("Flickr_%s_PUDper%scell.tif", currfile, currres), overwrite=TRUE)
  }

#######################
# Apply rastfun over es
escats <- unique(flickramap[, names(flickramap) %in% grep("escode", names(data), value=TRUE)])
  
lapply(escats, function(x) {
  rastfun(flickramap, x, 10000, x)
} ) 
  
#drop rows for biotic fauna with pet

#######################
# Combined es rasters 
# note that becuase these words can occur in a single photo, individual rasters for es cannot simply be added together.

#Biotic
bioes <- c("biotic_fauna", "biotic_reptile", "biotic_insect", "biotic_amphibian", "biotic_plant", "biotic_fungus", "biotic_ecosystem")
rast <- rastfun(flickramap, bioes, 10000, "biotic_all")

bionature <- c("biotic_plant", "biotic_fungus", "biotic_ecosystem")
rast <- rastfun(flickramap, bionature, 10000, "biotic_nature")

biowildlife <- c("biotic_fauna", "biotic_reptile", "biotic_insect", "biotic_amphibian")
rast <- rastfun(flickramap, bioes, 10000, "biotic_wildlife")

#abiotic
dput(escats)

#harvesting and hunting
harvest_es <- c()
rast <- rastfun(flickramap, harvest_es, 10000, "harvest_all")
harvestorhunt_es <- c(harvest_es, "recreation_hunting", "recreation_fishing")
rast <- rastfun(flickramap, harvest_es, 10000, "harvesthuntingfishing")

#recreation
recreation <- c()
rast <- rastfun(flickramap, recreation, 10000, "recreation_all")

#END######################
