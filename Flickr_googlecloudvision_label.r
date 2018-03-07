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

### Main processing ----

pb <- txtProgressBar()


counter = "000"
flickr_sub <- flickrshp[1:100, ]
starttime <- Sys.time()
google <- rep(NA,nrow(flickr_sub))
if(file.exists(sprintf("intermediate/google_%s.Rdata", counter)) load(sprintf("intermediate/google_%s.Rdata", counter)))

for(i in 1:nrow(flickr_sub)){
  if(is.na(google[i])){
    google[i] <- try(list(getGoogleVisionResponse(imagePath=gsub("https","http",flickr_sub$url_m[i]), feature = 'LABEL_DETECTION')))
  }
  setTxtProgressBar(pb, i/nrow(flickr_sub))
  if(i%%100==0) save(google,file=sprintf("intermediate/google_%s.Rdata", counter))
}
save(google,file=sprintf("intermediate/google_%s.Rdata", counter))
endtime <- Sys.time()
close(pb)

timetaken <- endtime - starttime

### Post processing ---