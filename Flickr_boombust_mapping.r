#This script calculates and creates rasters of the annual growth rate in Flickr, adjusted for overall Flickr trends
#we want to do this analysis in PUD but we only have flickr trends in terms of number of photos and have no idea about how number of users has changed. 
#So we multiply by a correction factor - nphotos arctic/nphotos global
#Intrinsic growth rate (annual)
#(log(X2)-log(X1))/(t2-t1)


### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)
require(rgdal)
require(ggplot2)
require(lubridate)

wd <- "/data/Claire/rasters"
setwd(wd)

curres <- 10000

##########################
### Setup ----

load("Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")
#photos from amap region, 
#regular and super users only, 
#2004 to 2017
#dropped rows with no google vision labels
#includes photos in towns/cities

#load AMAP boundaries
amap <- read_sf("Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp")
rcrs <- st_crs(amap)

#load global Flickr trends
AllFlickr <- read.csv("Flickr_global_nphotostaken_byhr_2000to2018.csv") %>% 
  mutate(year=year(datetime)) %>% 
  filter(year>=min(flickramap$year)) %>% 
  group_by(year) %>% 
  summarize("Total_Flickr_Photos" = sum(total))
#Calculate annual correction factor
ArcticFlickr <- flickramap %>% 
                  st_set_geometry(NULL) %>%
                  group_by(year) %>% 
		              summarise("Arctic_Flickr_Photos" = length(unique(id))) 
ArcticFlickr$year <- as.numeric(ArcticFlickr$year)
AllFlickr <- cbind(AllFlickr[1:14,], ArcticFlickr)
AllFlickr$correction_factor <- AllFlickr$Arctic_Flickr_Photos/AllFlickr$Total_Flickr_Photos

#######################
#Function to rasterise annually where the value in each cell = corrected PUD
#PUD = photo unit days
#res is resolution of the grid cell length (NOT area) in metres
rastfun <- function(curryear, curres) {
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs$proj4string)
    if(file.exists(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))==FALSE){ 
      rastamap <- rasterize(amap, rasttemplate, filename=sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif",curres))
    } else {
      rastamap <- raster(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))
    }
    rastamap[rastamap==1] <- 0
	#correction factor for that year
	corrfac <- AllFlickr$correction_factor[AllFlickr$year==curryear]
    #fill with pud
	subdat <- flickramap[flickramap$year==curryear, ]
    currrast <- rasterize(subdat, rastamap, fun=function(x, ...){ length(unique(x))*corrfac }, field="owner_date", update=TRUE, filename=sprintf("density_mapping/annual_rasters_pud_corrected/Flickr_correctedPUDper%scell_%s.tif", curres, curryear), overwrite=TRUE)
}
#######################
#Function to calculate annual growth rate
growthfun <- function(t2, curres) {
	t1 <- t2-1
	X1 <- raster(sprintf("density_mapping/annual_rasters_pud_corrected/Flickr_correctedPUDper%scell_%s.tif", curres, t1))
	X2 <- raster(sprintf("density_mapping/annual_rasters_pud_corrected/Flickr_correctedPUDper%scell_%s.tif", curres, t2))
	growthrast <- overlay(X1, X2, fun=function(x,y) {return((log(x)-log(y)))}, 
							filename=sprintf("density_mapping/annual_growth_rasters/Flickr_intrinsicgrowthper%scell_%sto%s.tif", curres, t1,t2))
}

#######################
#Create rasters of corrected annual pud
lapply(2004:2017, function(x) rastfun(x, curres))

#######################
#Create rasters of annual growth rate
lapply(2005:2017, function(x) growthfun(x, curres))

##END
