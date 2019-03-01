
#########################
##LIBRARIES and FILES
#########################

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

#load data on accessibility and PA that has been gridded to 10000m (script Flickr_spatialoverlaps_for_gams.r).
#load("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model/input/ArcticAMAP_griddedaccessibilitydata_10000_m.Rdata")
load("flickr/ArcticAMAP_griddedaccessibilitydata_10000_m.Rdata")


#########################
##SET UP DATA
#########################
## We need to calculate the number of Flickr photos per year and correct it based on total Flickr usage over the years

#add yearseason to flickramap
flickramap$season <- "summer"
flickramap$season[flickramap$month %in% c("01", "02", "03", "04", "11", "12")] <- "winter"
flickramap$year_old <- as.numeric(flickramap$year)
flickramap <- flickramap %>% 
  mutate(year=(if_else(month %in% c("11", "12"), year_old + 1, as.numeric(year)))) %>%
  mutate(yearseason = paste(year, season, sep="_"))
# 
#photo-unit-day function
PUD_grid <- function(flickrrecords, time, grid){
  df <- flickrrecords %>% split(time) %>% 
    map( function(x){
      x$col.id <- as.integer(1:nrow(x)) #each col.id represents a separate photo
      grid$row.id <- as.integer(row.names(grid)) #each row.id represents a grid cell
      PUDdf <- as.data.frame(st_intersects(grid, x)) %>% 
        left_join(x, by="col.id") %>% 
        select(row.id, owner_date) %>% 
        unique() %>% 
        group_by(row.id) %>%
        summarize(PUD=n()) %>% 
        right_join(grid, by="row.id") %>% 
        mutate(PUD=if_else(is.na(PUD), 0, as.numeric(PUD))) %>%
        as.tibble() 
      return(PUDdf)
    }) %>% bind_rows(.id="yearseason") %>%       
    separate(yearseason, c("year", "season"), sep="_", remove=FALSE)
  return (df)
}


#how many pud in each grid cell in each year-season, for the models
gridYearPUD_models <- PUD_grid(flickramap, flickramap$yearseason, grid_models) 
saveRDS(gridYearPUD_models,file = paste0("gridYearPUD_models",10000,"_m.Rdata"))
rm(gridYearPUD_models)
