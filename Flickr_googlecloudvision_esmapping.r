#this script performs spatial analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#I categorise the google labels by ecosystem feature or activity (escode) that they represent
#then map the distribution of those ecosystem features or activities
#follows on from Flickr_googlecloudvision_datasummary.r

### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/es_mapping"
setwd(wd)


##########################
### Preliminary processing ----

load("Flickr_Artic_60N_plus_flickrandgooglelabels_amap_escodes.Rdata")
#photos from amap region, 
#regular and super users only, 
#dropped rows with no google vision labels
#includes photos in towns/cities
#add a new column, owner_date
flickramap$owner_date <- paste(flickramap$owner, flickramap$datetkn, sep="_")

#load country borders shp
#worldmap <- readOGR("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBordersESRI_60degreesN_lambert.shp")
#load AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/AMAP_updatedRussia_clipto60N.shp")
rcrs <- crs(amap)

#######################
#Function to rasterise es where the value in each cell = PUD
#PUD = photo unit days
#number of days in a cell when a photo was taken that represents that es

rastfun <- function(es, curres, currfile) {
  #extract photos containing that es
  escols <- grep("escode", names(flickramap), value=TRUE)
  data_es <- filter_at(flickramap, escols, any_vars(. %in% es))
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs)
    if(file.exists(sprintf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/60degreesN_%sres.tif", currfile))==FALSE){ 
      rastamap <- rasterize(amap, rasttemplate, filename=sprintf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/AMAP_%sres.tif", currfile))
    } else {
      rastamap <- raster(sprintf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/AMAP_%sres.tif", currfile))
    }
    amap[amap==1] <- 0
    #fill with pud
    esrast <- rasterize(data_es, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("Flickr_%s_per%scell.tif", currfile, currres), overwrite=TRUE)
  }

#######################
# Apply rastfun over es

#drop rows for biotic fauna with pet

#Biotic
bioes <- c("biotic_fauna", "biotic_plant")
biorast <- rastfun(bioes, 500, "biotic_all")

