#This script does 2 things
# 1. Determines if 'superusers' take photos of different things
# 2. Determines what proportion of users are tourists, and if they take photos of different things from locals



wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(sf)
library(tidyverse)
# devtools::install_github("remi-daigle/flickRgeotag")
library(flickRgeotag)

api_key = '790ae098b7062ef9d5f4071d0933f23c'
secret = '02ee3e83c5cac3fe'

load("tag_analysis/input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
flickrshp <- flickrshp_tags

### 1. Are super users different
# 1a. Determine superusers
#we define a superuser as anyone contributing more than 473 photos 
#(this is the midpoint of the data - 50% of photos are contributed by people posting this many or more photos)
userfreq <- flickrshp %>% group_by(owner) %>% tally() 
superusers <- userfreq[userfreq$n >= 473, ] #766 superusers
averageusers <- userfreq[userfreq$n < 473, ] #the rest


#do they go further?
#do they use different flickr tags?
#do they photograph different objects (google vision)?


### 2. How many tourists ----

# 2a. Extract user info
allowners <- unique(flickrshp$owner)
userinfo <- flickr.people.getInfo(allowners)
write.csv(userinfo, "D:/Box Sync/Arctic/Data/Flickr/Flickr_user_info.csv", row.names=FALSE)

# 2b. What prop with info

# 2c. What prop tourists


### 3. Stats on user travel distance and travel times



lapply(allowners, function(currowner){
  ownerstats <- c()
 currphotos <- flickrshp[flickrshp$owner==currowner, ]
  #how long between photos
  totaltrip <- as.numeric(difftime(max(currphotos$datetkn), min(currphotos$datetkn), units="days"))
  #what is the centroid of their photos
  centroid <- st_centroid(currphotos) 
  #furtherest & average distance travelled (in km) from this centroid
  distances <- currphotos %>% st_distance(centroid)
  maxdist <- max(distances)/1000
  avgdist <- mean(distances)/1000
  
  #distance traveled along path from first to last photo
  bydate <- order(currphotos$datetkn) #order photos by date
  start <- bydate[1:length(bydate)-1]
  end <- bydate[2:length(bydate)]
  dists <- as.integer(mapply(function(a,b) {st_distance(user4[a, ], user4[b, ])}, a = start, b = end))
  timeV <- currphotos$datetkn
  timebtwphotos <- mapply(function(a,b) {as.numeric(difftime(timeV[b], timeV[a], units="hours"))}, a = start, b = end)
  
  ownerDF <- data.frame(dists, timebtwphotos)
  #how many trips did they take, defining a trip as any photos separated by 14 days or more
  counter=1
  ownerDF$tripid <- NA
  for(i in 1:nrow(ownerDF)){
        if(ownerDF$timebtwphotos[i] >= 14*24) counter=counter+1
        ownerDF$tripid[i] <- counter
  }
  #stats for each trip
  #total number of trips
  numtrips <- max(ownerDF$tripid)
  #total distance travelled each trip
  tripstats <- ownerDF %>% group_by(tripid) %>% filter(row_number()!=1) %>% #drop the first row
                                         summarise(tripdist_km=sum(dists)/1000,
                                                      triplength_hrs=sum(timebtwphotos),
                                                      triplength_days=round(sum(timebtwphotos)/24, 2))
  
  #where is the centroid of each trip, and how far is it from the total centroid?
  
  ownerstats <- list(totaltrip_days, centroid, maxdistfromcentroid_km, avgdistfromcentroid_km, numtrips, tripstats)

})


