require(sf)
require(tidyverse)


options(stringsAsFactors = FALSE)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/to_doi/Paper_3b/googlevision"
setwd(wd)

#flickrshp
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy.Rdata") #all flickr data
#Dropped rows with all NAs, testusers and kept 2004 to 2017 photos only

#########################################
#drop duplicated rows - this is a weird error that happens, I can't find a cause for it
flickrshp <- flickrshp %>% filter(! duplicated(id))
#transform geometry into lat and lon columns

#rename columns
names(flickrshp)[grep("googletag", names(flickrshp))] <- paste0("googlelabel", 1:20)
names(flickrshp)[grep("numtags", names(flickrshp))] <- "numlabels"
names(flickrshp)[grep("location", names(flickrshp))] <- "flickr_user_location"

#make lat and lon columns (wgs84)

flickrshp <- flickrshp %>% mutate(latitude_m_laea = unlist(map(flickrshp$geometry,1)),
                                  longitude_m_laea = unlist(map(flickrshp$geometry,2)))  %>%
              st_transform(4326) 
flickrshp <- flickrshp %>% mutate(latitude_wgs84 = unlist(map(flickrshp$geometry,1)),
                      longitude_wgs84 = unlist(map(flickrshp$geometry,2))) 


#drop unnessesary columns
flickrshp <- flickrshp %>% select(-one_of("secret", "server", "farm", "title", "ispublic", "isfriend", "isfamily",  "datetakengranularity", 
                                           "datetakenunknown", "tags", "accuracy", "context", "geo_is_family", "geo_is_friend", "geo_is_contact", "geo_is_public", 
                                           "height_m", "width_m", "place_id", "woeid", "yearmon", "photo_lat", "InCity",  
                                           "touristtype", "touristtype_revised", "local_country", "usertype", "owner_date")) %>% st_set_geometry(NULL)



#################################
#load the lookup table to match google labels to escodes
gwfreq <- read.csv("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/to_doi/Paper_3b/Google_labels_vs_escodes_lookup.csv", header=TRUE)

#replace words with Escodes
codetbl <- flickrshp %>% select(c("id", grep("googlelabel", names(flickrshp), value=TRUE))) 
codetbl[,grep("googlelabel", names(flickrshp), value=TRUE)] <- gwfreq$Escode[match(unlist(codetbl[, grep("googlelabel", names(flickrshp), value=TRUE)]), gwfreq$google_label)]
names(codetbl)[grep("googlelabel", names(codetbl))] <- paste0("escode", 1:20)

#add to flickrshp
flickrshp <- merge(flickrshp, codetbl[, c("id", grep("escode", names(codetbl), value=TRUE))], by.x="id", by.y="id", all.x=TRUE)

flickrshp <- flickrshp[, c("id", "owner", "datetaken", "url_m", "month", "year", "region", 
  "flickr_user_location", "latitude_m_laea", "longitude_m_laea", 
  "latitude_wgs84", "longitude_wgs84", "numlabels", "googlelabel1", "googlelabel2", 
  "googlelabel3", "googlelabel4", "googlelabel5", "googlelabel6", 
  "googlelabel7", "googlelabel8", "googlelabel9", "googlelabel10", 
  "googlelabel11", "googlelabel12", "googlelabel13", "googlelabel14", 
  "googlelabel15", "googlelabel16", "googlelabel17", "googlelabel18", 
  "googlelabel19", "googlelabel20", "googlescore1", "googlescore2", 
  "googlescore3", "googlescore4", "googlescore5", "googlescore6", 
  "googlescore7", "googlescore8", "googlescore9", "googlescore10", 
  "googlescore11", "googlescore12", "googlescore13", "googlescore14", 
  "googlescore15", "googlescore16", "googlescore17", "googlescore18", 
  "googlescore19", "googlescore20",  "escode1", "escode2", "escode3", 
  "escode4", "escode5", "escode6", "escode7", "escode8", "escode9", 
  "escode10", "escode11", "escode12", "escode13", "escode14", "escode15", 
  "escode16", "escode17", "escode18", "escode19", "escode20")]

#################################
#Drop the empty rows
#drop rows with no google words (this could be due to no url, or the user having taken down the photo or made it private)
flickrshp <- flickrshp[ rowSums(is.na( flickrshp[, grep("googlelabel", names(flickrshp))] )) < 20, ] #drop rows with all NAs

#fix the numlabels column
flickrshp <- flickrshp %>% mutate(numlabels=20-rowSums(is.na( flickrshp[, grep("googlelabel", names(flickrshp))])))

#################################
#split by region and save
regions <- unique(flickrshp$region)

for(curregion in regions) {
  currdf <- flickrshp %>% filter(region==curregion)
  write.csv(currdf, paste0("Flickr_Artic_60N_googlelabels_escodes_", curregion, ".csv"), fileEncoding="UTF-8", row.names=FALSE)
}


