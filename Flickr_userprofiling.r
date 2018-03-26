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

load("input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
flickrshp <- flickrshp_tags

### 1. Are super users different
# 1a. Determine superusers
#we define a superuser as anyone contributing more than 473 photos 
#(this is the midpoint of the data - 50% of photos are contributed by people posting this many or more photos)
userfreq <- flickrshp %>% group_by(owner) %>% tally() 
superusers <- userfreq[userfreq$n >= 473, ] #766 superusers
averageusers <- userfreq[userfreq$n < 473, ] #the rest





### 2. How many tourists ----

# 2a. Extract user info
allowners <- unique(flickrshp$owner)
userinfo <- flickr.people.getInfo(allowners)
write.csv(userinfo, "D:/Box Sync/Arctic/Data/Flickr/Flickr_user_info.csv", row.names=FALSE)

# 2b. What prop with info

# 2c. What prop tourists


### 3. How far do users travel

#how long between their first and last photo
ownerdates <- flickrshp[flickrshp$owner==userfreq$owner[4],"datetkn"]
firstphoto <- min(ownerdates$datetkn)
lastphoto  <- max(ownerdates$datetkn)
  as.numeric(difftime(lastphoto, firstphoto, units="days"))

#what is the centroid of their photos
centroid <- st_centroid(userfreq[4, ]) 
#how far did they travel from this centroid
distances <- userfreq[4, ] %>% st_cast("POINT") %>% st_distance(centroid)
#furtherest & average distance travelled (in km)
maxdist <- max(distances)/1000
avgdist <- mean(distances)/1000

#distance between their centroid and their stated location??



plot(userfreq[4,"n"])
plot(centroid, col="red", add=TRUE)

