#this script performs descriptive and statistic analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r and Flickr_googlecloudvision_postprocessing.r


### Set up libraries ----
require(sf)
require(tidyverse)
require(ExPosition)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
setwd(wd)

options(stringsAsFactors = FALSE)

#load data
load("output/Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_urban.Rdata")
#flickrshp - flickr data for each photo plus user classifications, what region and whether urban

#Contingency table

subdat <- flickrshp[1:10, grep("googletag", names(flickrshp))] %>% st_set_geometry(NULL) 
subdat <- subdat[rowSums(is.na(subdat))<length(subdat),] #drop rows with all NAs

wordL <- lapply(1:nrow(subdat), function(i) {
  currow <- unname(unlist(subdat[i, !is.na(subdat[i,])])) %>% droplevels()
  d <- expand.grid(currow, currow)
  return(d)
})

wordDF <- do.call(rbind, wordL)
ctbl <- table(wordDF)




#CATA analysis


#network diagram
#arc diagram