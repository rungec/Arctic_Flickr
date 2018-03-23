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
 #try(lapply(names(regionlist), function(i) dir.create(paste0(getwd(), "/intermediate/", i)))) #create dirs

### Set Google API authentication and vision API client ----
creds = fromJSON(paste0(dirname(wd), '/login_cloud/client_oauth_flickr_labels.json'))
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

  #loop over splits
  for(currsplit in 1:length(splits)){
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
### Post processing ---

## Combine data ----
#about 5.6gb total so we do it by region

for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  flickrshp_sub <- flickrshp[flickrshp$region %in% curregion[[1]], ]
  
  #bind all the intermediate google files for a given region  
    filelist <- list.files(paste0(wd, "/intermediate/", curregname), full.names=TRUE)
    allgoogle <- list() #set up empty list
      for(i in 1:length(filelist)){
        load(filelist[i])
        allgoogle <- append(allgoogle, google)
      }
    save(allgoogle,file=sprintf("output/byregion/Google_output_%s.Rdata", curregname)) #save the dataset
    
    #Join the google data to flickrshp and save it
    flickrshp_sub$google <- allgoogle
        save(flickrshp_sub,file=sprintf("output/byregion/Flickr_Artic_60N_plus_flickr_and_google_labels_%s.Rdata", curregname))
    
    #test that the ids match between the two datasets
    checkfun <- function(flickrshpt){
      counter=0
      for(i in 1:nrow(flickrshpt)){
        if(flickrshpt$id[i]!=as.numeric(names(flickrshpt$google)[[i]])) print(sprintf("ID mismatch row %s original %s google %s", i, flickrshpt$id[i], names(flickrshpt$google)[[i]]))
        else  (counter=counter+1)
      }
      print(sprintf("%s of %s ids match", nrow(flickrshpt), counter))
    }
    checkfun(flickrshp_sub)

  #add a summary of the data to the flickrshp
  flickrshp_sub$numtags = unlist(lapply(allgoogle, function(x) nrow(x))) #number of tags for each photo
  flickrshp_sub$firsttag = unlist(lapply(allgoogle, function(x) first(x$description))) #highest scoring tag for each photo
  flickrshp_sub$maxscore = unlist(lapply(allgoogle, function(x) max(x$score))) #max score for each photo
  flickrshp_sub$minscore = unlist(lapply(allgoogle, function(x) min(x$score))) #min score for each photo
      save(flickrshp_sub, file=sprintf("output/byregion/Flickr_Artic_60N_plus_flickr_and_google_labels_%s.Rdata", curregname))


  #make a table of the top 10 google tags  
  top10tags <- lapply(allgoogle, function(x) {
                            tags <- rep(NA, 10)
                            if(nrow(x)>=10) {
                              tags <- x$description[1:10]
                            } else {
                              tags[1:nrow(x)] <- x$description[1:nrow(x)]
                            }
                         return(tags)
                      }) #top 10 tags
  top10tags <- do.call(rbind, top10tags)
  colnames(top10tags) <- paste0(rep(c("googletag"), each=10), 1:10)
    write.csv(top10tags, sprintf("output/byregion/Flickr_Artic_60N_plus_google_labels_%s.csv", curregname), row.names = TRUE)

  #add the top 10 google tags as columns in the .shp, then save a .shp for each region
  google_summary <- within(flickrshp_sub, rm(flickr_tags, title_tags, google))
  google_summary <- cbind(google_summary, top10tags)

  #function to save as shp
  st_write(google_summary, sprintf("output/byregion/Flickr_Artic_60N_plus_google_labels_%s.shp", curregname))
  google_summary_df <- google_summary %>% st_set_geometry(NULL) %>% data.frame()
    write.csv(google_summary_df, sprintf("output/byregion/Flickr_Artic_60N_plus_top10googlelabels_%s.csv", curregname), fileEncoding = "UTF-8")

  #Make a long data.frame of all the google tags and save it
  alltags <- allgoogle %>% tibble() %>% unnest(.id="name")
    write.csv(alltags, sprintf("output/byregion/Google_labels_allphotos_%s.csv", curregname), fileEncoding = "UTF-8")
  
  #Frequency of all tags scoring above 60
  tagfreq <- plyr::count(alltags[alltags$score>=0.6, "description"]) #count how frequently each tag is used
    write.csv(tagfreq, sprintf("output/byregion/frequency_of_google_label_words_overscore60_%s.csv", curregname), fileEncoding = "UTF-8")
  
  print(paste0("FINISHED POST PROCESSING for ", curregname))
}




                   



