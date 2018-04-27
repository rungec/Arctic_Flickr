#This script does 2 things
# 1. Determines if 'superusers' take photos of different things
# 2. Determines (a) what proportion of users are tourists, and (b) if they take photos of different things from locals


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(sf)
library(tidyverse)
library(readxl)
devtools::install_github("remi-daigle/flickRgeotag")
library(flickRgeotag)

api_key = '790ae098b7062ef9d5f4071d0933f23c'
secret = '02ee3e83c5cac3fe'


#flickrshp <- read_sf()
load("tag_analysis/input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
flickrshp <- flickrshp_tags
#drop the tags & titles
flickrshp[["flickr_tags"]] <- NULL
flickrshp[["title_tags"]] <- NULL
rm(flickrshp_tags)

### 1. Are super users different

#do they go further?
#do they use different flickr tags?
#do they photograph different objects (google vision)?
#https://github.com/GoranMilovanovic/Distributional-Semantics-in-R
#http://docs.quanteda.io/
#http://www.mjdenny.com/Text_Processing_In_R.html

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

#########################
### 2. Identify tourists ----
# We identify a user as a tourist if they (i) have listed their home location and it is outside the Arctic; or if not listed (ii) if they have spent more than x days ....
 
# 2a. Extract user info
allowners <- unique(flickrshp$owner)
userinfo <- lapply(allowners, function(x) flickr.people.getInfo(x))
userinfoDF <- do.call(rbind, userinfo)
userinfoDF$owner <- allowners
write.csv(userinfoDF, "tables/Flickr_user_statedlocation.csv", row.names=FALSE, ,fileEncoding="UTF-8")

# 2b. What prop of users have stated their location
userinfoDF$location[userinfoDF$location==""] <- NA
length(which(!is.na(userinfoDF$location )))/nrow(userinfoDF)
length(unique(userinfoDF$location)) #How many locations do they state?
#save the unique locations
write.csv(unique(userinfoDF$location), "tables/Flickr_user_statedlocation_unique.csv", row.names=FALSE, fileEncoding="UTF-8")

#I then manually create a lookup table from these locations
#add a column tourist_type using lookup table
lookup <- read_excel("tables/Flickr_user_statedlocation_unique.xlsx", sheet="sheet1")
userinfoDF <- merge(userinfoDF, lookup, by.x="location", by.y="x", all.x=TRUE) 

# 2c. What prop tourists in each region?

#add the user location info to the flickrshp photo dataset
flickrshp2 <- merge(flickrshp, userinfoDF, by.x="owner", by.y="owner", all.x=TRUE)
#add column listing the user type
flickrshp2$usertype <- "regular"
flickrshp2$usertype[flickrshp2$owner %in% superusers$owner] <- "superuser"
flickrshp2$usertype[flickrshp2$owner %in% testusers$owner] <- "testuser"
#save the whole dataset with the userdata
save(flickrshp2, file="tag_analysis/input/Flickr_Artic_60N_plus_userdata.Rdata")

#how many superusers, regular users in the different regions
#how many tourists vs locals in the different regions
user_prop1 <- flickrshp2 %>% st_set_geometry(NULL) %>% group_by(region, usertype) %>% summarise(n_user_type = n_distinct(owner))

flickrshp2 <- st_set_geometry(flickrshp2, NULL)
user_prop1 <- lapply(unique(flickrshp2$region), function(curreg){
				a <- flickrshp2[flickrshp2$region==curreg,]
				n_usertype <-  a %>% group_by(usertype) %>% 
										summarise(n_user_type = n_distinct(owner) )
				n_touristtype <-  a %>% group_by(tourist_type) %>% 
										summarise(n_user_type = n_distinct(owner))
				names(n_touristtype)[1] <- "usertype"						
				n_usertype <- rbind(n_usertype, n_touristtype)						
				perc_user_type <-  n_usertype$n_user_type/n_distinct(a$owner)
				return(data.frame(region=curreg, n_usertype, perc_user_type))
				})
user_prop1 <- do.call(rbind, user_prop1)				
write.csv(user_prop1, "tables/Flickr_user_types_by_region.csv", row.names=FALSE)

#What % of superusers are tourists?
user_prop2 <- flickrshp2 %>% group_by(usertype, tourist_type) %>% 
							summarise(n_user_tourist_type = n_distinct(owner),
									perc_user_tourist_type = n_distinct(owner)/n_distinct(flickrshp2$owner))

write.csv(user_prop2, "tables/Flickr_user_type_overallproportions.csv", row.names=FALSE)

#########################
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
  centroid_dists_avg=mean(tripstats2$tripcentroid_distance_from_usercentroid)
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

#fix typo in centroid distances without running the whole thing again
#centroid_dists_avg <- allownerstats2 %>% filter(tripid>1) %>% group_by(owner) %>% summarise(centroid_dists_avg=mean(tripcentroid_distance_from_usercentroid))
#centroid_dists_avg_match <- centroid_dists_avg[match(allownerstats2$owner[allownerstats2$tripid==0], centroid_dists_avg$owner), ]
#allownerstats2$tripcentroid_distance_from_usercentroid[allownerstats2$tripid==0 ] <- centroid_dists_avg_match$centroid_dists_avg

#save
write.csv(allownerstats2, "tables/Flickr_user_trip_summary.csv", row.names = FALSE)

