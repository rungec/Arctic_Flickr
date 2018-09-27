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
write.csv(AllFlickr, "ArcticandGlobalFlickrTrends.csv")

#Seasonal statistics on flickr use in the Arctic
a_all_pud <- flickramap %>% st_set_geometry(NULL) %>% 
  group_by(Country) %>% 
  summarise("Arctic_allseasons_PUDs" = length(unique(owner_date)),
            "Arctic_allseasons_Photos" = length(unique(id)))
a_win_pud <- flickramap %>% st_set_geometry(NULL) %>% 
  group_by(Country) %>% 
  filter(month %in% c("01", "02", "03", "04", "11", "12")) %>% 
  summarise("Arctic_winter_PUDs" = length(unique(owner_date)),
            "Arctic_winter_Photos" = length(unique(id)))
a_sum_pud <- flickramap %>% st_set_geometry(NULL) %>% 
  group_by(Country) %>% 
  filter(month %in% c("05", "06", "07", "08", "09", "10")) %>% 
  summarise("Arctic_summer_PUDs" = length(unique(owner_date)),
            "Arctic_summer_Photos" = length(unique(id)))
a_allseasons <- cbind(a_all_pud, a_win_pud, a_sum_pud)
write.csv(a_allseasons, "Arctic_Flickr_photos_puds_byseasonandcountry.csv", row.names=FALSE)


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

#Function to rasterise annually where the value in each cell = uncorrected PUD
rastfun2 <- function(curryear, curres) {
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs$proj4string)
  if(file.exists(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))==FALSE){ 
    rastamap <- rasterize(amap, rasttemplate, filename=sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif",curres))
  } else {
    rastamap <- raster(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))
  }
  rastamap[rastamap==1] <- 0
  #fill with pud
  subdat <- flickramap[flickramap$year==curryear, ]
  currrast <- rasterize(subdat, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("density_mapping/annual_rasters_pud_amap/Flickr_PUDper%scell_%s.tif", curres, curryear), overwrite=TRUE)
}

rastfun3 <- function(season, curres) {
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs$proj4string)
  if(file.exists(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))==FALSE){ 
    rastamap <- rasterize(amap, rasttemplate, filename=sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif",curres))
  } else {
    rastamap <- raster(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))
  }
  rastamap[rastamap==1] <- 0
  #fill with pud
  if(season=="summer") {
        subdat <- flickramap[flickramap$month %in% c("05", "06", "07", "08", "09", "10"),]
  } else if (season=="winter") {
          subdat <- flickramap[flickramap$month %in% c("11", "12", "01", "02", "03", "04"), ]
  }  
  currrast <- rasterize(subdat, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("density_mapping/static_rasters_pud_amap/Flickr_PUDper%scell_%s.tif", curres, season), overwrite=TRUE)
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

#Function to calculate annual growth rate - uncorrected
growthfun2 <- function(t2, curres) {
  t1 <- t2-1
  X1 <- raster(sprintf("density_mapping/annual_rasters_pud_amap/Flickr_PUDper%scell_%s.tif", curres, t1))
  X2 <- raster(sprintf("density_mapping/annual_rasters_pud_amap/Flickr_PUDper%scell_%s.tif", curres, t2))
  growthrast <- overlay(X1, X2, fun=function(x,y) {return((log(x)-log(y)))}, 
                        filename=sprintf("density_mapping/annual_growth_rasters_uncorrected/Flickr_intrinsicgrowthper%scell_%sto%s.tif", curres, t1,t2))
}

#######################
#Function to calculate lm of slope of growth over years
slopemodfun <- function(t1, t2, curres){
  years <- seq(t1, t2, 1)
  filelist <- lapply(years, function(x) sprintf("density_mapping/annual_rasters_pud_amap/Flickr_PUDper%scell_%s.tif", curres, x))
  rstack <- stack(filelist)
  rslope <- calc(rstack, fun=function(x) {
    if(sum(is.na(x))==0 & (sum(x!=0)>2)==TRUE) {
      df <- data.frame(years, x)
      m1 <- lm(df)
      currslope <- coefficients(m1)[2]
    } else {
      currslope <- NA
    }
    return(currslope)
    }, filename=sprintf("density_mapping/boom_bust_model/Flickr_slope%sto%sper%scell.tif", t1, t2, curres), overwrite=TRUE)
  rpval <- calc(rstack, fun=function(x) {
    if(sum(is.na(x))==0 & (sum(x!=0)>2)==TRUE) { 
      df <- data.frame(years, x)
      m1 <- lm(df)
      try(pval <- coef(summary(m1))["x","Pr(>|t|)"])
    } else {
      pval <- NA
    } 
    return(pval)
  }, filename=sprintf("density_mapping/boom_bust_model/Flickr_pval%sto%sper%scell.tif", t1, t2, curres), overwrite=TRUE)
}
  

#######################
#Create rasters of corrected annual pud
lapply(2004:2017, function(x) rastfun(x, curres))

#######################
#Create rasters of corrected annual growth rate
lapply(2005:2017, function(x) growthfun(x, curres))

#######################
#Create rasters of uncorrected annual pud
lapply(2004:2017, function(x) rastfun2(x, curres))

#######################
#Create rasters of uncorrected annual growth rate
lapply(2005:2017, function(x) growthfun2(x, curres))

#######################
#Create rasters of slope and pval
slopemodfun(2012, 2017, curres)

#######################
#Create rasters of seasonal pud
rastfun3("summer", 10000)
rastfun3("winter", 10000)

##END
