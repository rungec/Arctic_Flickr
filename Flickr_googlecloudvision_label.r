# This script takes an text file listing the url for each flickr photo
# then uses Google Cloud Vision https://cloud.google.com/vision/ to tag each image
# It returns the tag, score, and MID code that can be used in Knowledge graph https://www.google.com/intl/bn/insidesearch/features/search/knowledge.html
# https://www.r-bloggers.com/google-vision-api-in-r-rooglevision/
# It follows on from Flickr_tidy_flickrtags.r

### Set up libraries ----
library(sf)
library(tidyverse)
 devtools::install_github("flovv/RoogleVision")
library(RoogleVision) #for getGoogleVision
library(jsonlite) # to import credentials

#wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
wd <- "/home/cru016@ad.uit.no/Documents/tag_analysis"
setwd(wd)
wd2 <- "/data/Claire"
 
#set list of regions to process
regionlist <- list(IcelandGreenland=c("Iceland", "Greenland"),
                    NorthAmerica = c("Alaska", "Canada"), 
                    Scandinavia=c("Norway", "Sweden"), 
                    Finland=c("Finland", "Aland"), 
                    Russia=c("Russia"), 
                    Marine=c("Marine"), 
                    OtherIslands =c("Faroe.Islands", "United.Kingdom"))
#try(lapply(names(regionlist), function(i) dir.create(paste0(getwd(), "/intermediate/", i))))

#create dirs
lapply(names(regionlist), function(i) {
newdir <- paste0("intermediate/", i)
try(dir.create(newdir))
})

### Set Google API authentication and vision API client ----
creds = fromJSON(paste0(dirname(wd), '/login_cloud/client_oauth_flickr_labels.json'))
creds = fromJSON(paste0(wd, '/login_cloud/client_oauth_flickr_labels.json'))
# options("googleAuthR.client_id" = "xxx.apps.googleusercontent.com")
# options("googleAuthR.client_secret" = "")
options("googleAuthR.client_id" = creds$installed$client_id)
options("googleAuthR.client_secret" = creds$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

### Load data ----
flickrshp <- read_sf("input/Flickr_Artic_60N_byregion_laea_icelandupdate_urban.shp")
#load(file="input/Flickr_Artic_60N_plus_flickr_labels.Rdata") #includes the flickr tags
#flickrshp <- flickrshp_tags
#rm(flickrshp_tags)

### Preliminary processing ---
#drop photos pre 2000 and from 2018 or later
flickrshp <- flickrshp[flickrshp$year<2018 & flickrshp$year>2000, ]

setwd(wd2)


### Main processing ----
for(i in seq_along(regionlist)){
 curregion <- regionlist[i] 
curregname <- names(curregion)
 flickrshp_sub <- flickrshp[flickrshp$region %in% curregion[[1]], ]
  #set up splits 
  d <- 1:nrow(flickrshp_sub)
  splits <- split(d, ceiling(seq_along(d)/3000))
  rm(d)
  #check if any files exist
  filelist <- list.files(paste0("intermediate/", names(regionlist[i])), ".Rdata")
  
  #loop over splits
  for(currsplit in 1:length(splits)){
  	
	#check if any files exist and if they do then start from the last one
	if(length(filelist)> currsplit) next 
	
    countertext = paste(curregname, formatC(currsplit, width = 4, format = "d", flag = "0"), sep="_") #Pad with zero
    print(paste("Starting", countertext, Sys.time(), sep=" "))
    
    #pull out the urls for this split
    n1 <- splits[[currsplit]][1] #from
    n2 <- tail(splits[[currsplit]], 1) #to
    flickr_urls <- flickrshp_sub[n1:n2, c("id", "url_m")] %>% st_set_geometry(NULL) %>% data.frame() #pull out urls, drop the georeferencing, change tibble to data.frame
   
    #set an empty list
    google <- rep(NA, nrow(flickr_urls)) 
  
    #if file is already there load it
    if(file.exists(sprintf("intermediate/%s/google_%s.Rdata", curregname, countertext))) load(sprintf("intermediate/%s/google_%s.Rdata", curregname, countertext))
  
    #extract googleVision tags for each photo in subset
    for (i in 1:nrow(flickr_urls)){
     if(is.na(google[i])){
        google[[i]][1] <- try(list(getGoogleVisionResponse(imagePath=gsub("https","http",flickr_urls[i,"url_m"]), feature = 'LABEL_DETECTION', numResults=20)))
     }
      #save the file every 1000 photos
      if(i%%1000==0) save(google,file=sprintf("intermediate/%s/google_%s.Rdata", curregname, countertext))
    }
    names(google) <- flickr_urls[, "id"]
    save(google,file=sprintf("intermediate/%s/google_%s.Rdata", curregname, countertext)) #save the subset
  }
  rm(splits)
  print(paste0("FINISHED GOOGLE ", names(curregion), Sys.time()))
}

