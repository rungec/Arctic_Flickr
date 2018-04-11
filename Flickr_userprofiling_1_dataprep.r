#This script does 2 things
# 1. Determines if 'superusers' take photos of different things
# 2. Determines what proportion of users are tourists, and if they take photos of different things from locals



wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(sf)
library(tidyverse)
# devtools::install_github("remi-daigle/flickRgeotag")
#library(flickRgeotag)

#api_key = '790ae098b7062ef9d5f4071d0933f23c'
#secret = '02ee3e83c5cac3fe'


#flickrshp <- read_sf()
load("tag_analysis/input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
flickrshp <- flickrshp_tags
#drop the tags & titles
flickrshp[["flickr_tags"]] <- NULL
flickrshp[["title_tags"]] <- NULL
rm(flickrshp_tags)

### 1. Are super users different
# 1a. Determine superusers
#we define a superuser as anyone contributing more than n=superusersplitval photos (where n>=superusersplitval accounts for 50% of all photos)
#(this is the midpoint of the data - 50% of photos are contributed by people posting this many or more photos)
userfreq <- flickrshp %>% group_by(owner) %>% tally()
userfreq <- userfreq[order(userfreq$n),] #in ascending order of freq
superusersplitval <- userfreq$n[which.min(abs(cumsum(userfreq$n)-sum(userfreq$n)*0.5))] #which freq accounts for 50% of all photos
print(superusersplitval)
superusers <- userfreq[userfreq$n >= superusersplitval, ] #superusers
averageusers <- userfreq[userfreq$n < superusersplitval & userfreq$n > 2, ] #the rest
testusers <- userfreq[userfreq$n==1 | userfreq$n==2, ] #test users 1 or 2 photos


#do they go further?
#do they use different flickr tags?
#do they photograph different objects (google vision)?


### 2. How many tourists ----

# 2a. Extract user info
#allowners <- unique(flickrshp$owner)
#userinfo <- flickr.people.getInfo(allowners)
#write.csv(userinfo, "tables/Flickr_user_info.csv", row.names=FALSE)

# 2b. What prop with info

# 2c. What prop tourists


### 3. Stats on user travel distance and travel times
allowners <- unique(flickrshp$owner)

allownerstats <- lapply(allowners, function(currowner){
  #extract subset for currowner
  currphotos <- flickrshp[flickrshp$owner==currowner, ]
  
  #how long between first and last photos
  totaltrip_days <- as.numeric(difftime(max(currphotos$datetkn), min(currphotos$datetkn), units="days"))

  #distance traveled along path from first to last photo
  bydate <- order(currphotos$datetkn) #order photos by date
  start <- bydate[c(1, 1:length(bydate)-1)]
  end <- bydate[1:length(bydate)]
  dists <- as.integer(mapply(function(a,b) {st_distance(currphotos[a, ], currphotos[b, ])}, a = start, b = end))/1000
  photodates <- currphotos$datetkn
  timebtwphotos <- mapply(function(a,b) {as.numeric(difftime(photodates[b], photodates[a], units="days"))}, a = start, b = end)
  
  #reorder photos by date, add distance per day & time between consecutive photos
  currphotos <-  cbind(currphotos[bydate,], dists, timebtwphotos)
  total_trip_dist <- sum(currphotos$dists)
  
  #what is the centroid of the convex hull bounding their photos
  user_centroid <- st_geometry(currphotos) %>% st_union() %>% st_convex_hull() %>% st_centroid()
  
  #furthest & average distance travelled (in km) from this centroid
  distances <- currphotos %>% st_distance(user_centroid)
  maxdistfromcentroid_km <- max(distances)/1000
  avgdistfromcentroid_km <- mean(distances)/1000
  
  #how many trips did they take, defining a trip as any photos separated by 14 days or more
  counter=1
  currphotos$tripid <- NA
  for(i in 1:nrow(currphotos)){
        if(currphotos$timebtwphotos[i] >= 14) counter=counter+1 
        currphotos$tripid[i] <- counter
  }
  
  #total number of trips
  numtrips_overall <- max(currphotos$tripid)
  numphotos_overall <- nrow(currphotos)
  
  #total distance travelled each trip
  tripstats <- currphotos %>% st_set_geometry(NULL) %>% group_by(tripid) %>% 
                                         summarise(tripdist_km=sum(dists),
                                                   triplength_days=sum(timebtwphotos)) %>%
                                        as.list()
  #average and median distance per day, excluding days with no photos
  #tripstats2 <- currphotos %>% st_set_geometry(NULL) %>%  filter(row_number()!=1) %>%  #drop the first row
                         #      group_by(datetkn) %>% summarise(dailydist=sum(dists)) %>%
                          #         summarise(tripdist_avperday_km=mean(dailydist),
                           #                 tripdist_medianperday_km=median(dailydist))
  
  #where is the centroid of each trip, and how far is it from the total centroid?
  tripstats2 <- data.frame()
  trip_centroids <- c()
  for(currid in unique(currphotos$tripid)){
    currtrip <- currphotos[currphotos$tripid==currid,  ]
    numphotos_trip <- nrow(currtrip)
    currcentroid <- currtrip %>% st_union() %>% st_convex_hull() %>% st_centroid()
	distance_from_centroid <- currcentroid %>% st_distance(user_centroid)/1000
    avgtripdist_from_centroid <- mean(currtrip %>% st_distance(currcentroid)/1000)
    maxtripdist_from_centroid <- max(currtrip %>% st_distance(currcentroid)/1000)
   tripstats2 <- rbind(tripstats2, c(distance_from_centroid[[1]], maxtripdist_from_centroid[[1]], avgtripdist_from_centroid[[1]], numphotos_trip)) 
   trip_centroids <- rbind(trip_centroids, st_coordinates(currcentroid))
  }
  names(tripstats2) <- c("tripcentroid_distance_from_usercentroid", "maxtripdist_from_centroid", "avgtripdist_from_centroid", "numphotos_trip")
  
  #what is the average distance of their trip centroids from their user centroid, for people with more than 1 trip
  if(numtrips_overall>1){
  centroid_dists_avg=mean(tripstats2$distance_from_centroid)
  } else {
  centroid_dists_avg=NA
  }
  
  #summary
  #in this summary, the row where tripid=0 corresponds to the stats for all the photos for that user
  # the average trip distance from centroid for tripid=0 is the average distance between all the photos and the centroid of the convex hull bounding the photos, 
  # the average trip distance from centroid for all other tripids is the distance between 
  # the centroid of the convex hull bounding the trip photos and all the photos for that trip. 
  # the max distance from centroid is in relation to the centroid of the all photos for that trip (all photos for the user if tripid=0)
  # distance from centroid is the distance between the centroid of the convex hull bounding the photos for a given trip and the centroid of the convex hull bounding all the photos.
  # for tripid=0 I report the average of this value across trips, unless there is only 1 trip in which case NA is returned

  overallstats <- data.frame(owner=currowner, tripid=0, 
                             tripdist_km=total_trip_dist, 
                             triplength_days=totaltrip_days, 
							 tripcentroid_distance_from_usercentroid=centroid_dists_avg,
                             maxtripdist_from_centroid=maxdistfromcentroid_km[[1]], 
                             avgtripdist_from_centroid=avgdistfromcentroid_km[[1]], 
                             numphotos_trip=numphotos_overall, numtrips=numtrips_overall)
  tripstats <- data.frame(owner=rep(currowner, numtrips_overall), 
                          tripstats, 
                          tripstats2, 
                          numtrips=rep(NA, numtrips_overall))
  overallstats <- rbind(overallstats, tripstats)
  ownercentroids <- rbind(st_coordinates(user_centroid), trip_centroids)
  overallstats <- cbind(overallstats, ownercentroids)
  return(overallstats)
})

#Transform the list into a data.frame
allownerstats2 <- do.call(rbind, allownerstats)

#add column listing the user type
allownerstats2$usertype <- "regular"
allownerstats2$usertype[allownerstats2$owner %in% superusers$owner] <- "superuser"
allownerstats2$usertype[allownerstats2$owner %in% testusers$owner] <- "testuser"

#edit names
names(allownerstats2)[10:11] <- c("centroid_X", "centroid_Y")

#save
write.csv(allownerstats2, "tables/Flickr_user_trip_summary.csv", row.names = FALSE)

