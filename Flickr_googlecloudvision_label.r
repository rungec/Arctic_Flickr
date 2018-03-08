# This script takes an text file listing the url for each flickr photo
# then uses Google Cloud Vision https://cloud.google.com/vision/ to tag each image
# It returns the tag, score, and MID code that can be used in Knowledge graph https://www.google.com/intl/bn/insidesearch/features/search/knowledge.html
# https://www.r-bloggers.com/google-vision-api-in-r-rooglevision/

### Set up libraries ----
library(sf)
library(tidyverse)
 devtools::install_github("flovv/RoogleVision")
library(RoogleVision) #for getGoogleVision
library(jsonlite) # to import credentials

 wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
 setwd(wd)
 
### Set Google API authentication and vision API client ----
creds = fromJSON(paste0(dirname(wd), '/login_cloud/client_oauth_flickr_labels.json'))
# options("googleAuthR.client_id" = "xxx.apps.googleusercontent.com")
# options("googleAuthR.client_secret" = "")
options("googleAuthR.client_id" = creds$installed$client_id)
options("googleAuthR.client_secret" = creds$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

### Load data ----
flickrshp <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate.shp")

### Preliminary processing ---
#drop photos pre 2000 and from 2018 or later
flickrshp <- flickrshp[flickrshp$year<2018 & flickrshp$year>2000, ]

### Main processing ----

#set up splits 
d <- 1:nrow(flickrshp)
splits <- split(d, ceiling(seq_along(d)/3000))
rm(d)

#loop over splits
for(currsplit in 1:length(splits)){
  countertext = formatC(currsplit, width = 4, format = "d", flag = "0") #Pad with zero
  print(paste("Starting", countertext, Sys.time(), sep=" "))
  
  #pull out the urls for this split
  n1 <- splits[[currsplit]][1] #from
  n2 <- tail(splits[[currsplit]], 1) #to
  flickr_urls <- flickrshp[n1:n2, c("id", "url_m")] %>% st_set_geometry(NULL) %>% data.frame() #pull out urls, drop the georeferencing, change tibble to data.frame
 
  #set an empty list
  google <- rep(NA, nrow(flickr_urls)) 
  
  #if file is already there load it
  if(file.exists(sprintf("intermediate/google_%s.Rdata", countertext))) load(sprintf("intermediate/google_%s.Rdata", countertext))
  
  #extract googleVision tags for each photo in subset
  for (i in 1:nrow(flickr_urls)){
    if(is.na(google[i])){
      google[[1]][i] <- try(list(getGoogleVisionResponse(imagePath=gsub("https","http",flickr_urls[i,"url_m"]), feature = 'LABEL_DETECTION', numResults=20)))
    }
    #save the file every 1000 photos
    if(i%%1000==0) save(google,file=sprintf("intermediate/google_%s.Rdata", countertext))
  }
  names(google) <- flickr_urls[, "id"]
  save(google,file=sprintf("intermediate/google_%s.Rdata", countertext)) #save the subset
}

### Post processing ---

filelist <- list.files(paste0(wd, "/intermediate"))
