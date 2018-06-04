#This script does 2 things
# a. extracts user stated location from flickr
# b. Identifies superusers and tourists

# output: Flickr_userinfo_tourist_or_superuser.csv
# field "usertype": superuser/regular/testuser (how many photos person posts to flickr)
# field "touristtype": tourist/domestic/local/NA (from user stated location, locals fall within region bounded by 59.5N, domestic outside that region but within any Arctic country)
# field "touristtype_revised": tourist/local/NA (from user stated location, locals fall within region bounded by AMAP, AMAP_updatedRussia_clipto60N.shp)

# this is later merged with the flickrshp in Flickr_googlecloudvision_postprocessing.r
# to make "Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo.Rdata"

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
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_plus_flickr_labels.Rdata")
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

#########################
# 1. Identify superusers ----
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
# We then make a table that lists a) tourist/domestic tourists/locals and b) superusers/regular users/testusers (Step 3)
 
# 2a. Extract user info
allowners <- unique(flickrshp$owner)
userinfo <- lapply(allowners, function(x) flickr.people.getInfo(x))
userinfoDF <- do.call(rbind, userinfo)
userinfoDF$owner <- allowners
write.csv(userinfoDF, "tables/flickr_userlocation/Flickr_user_statedlocation.csv", row.names=FALSE, fileEncoding="UTF-8")

# 2b. What prop of users have stated their location
userinfoDF$location[userinfoDF$location==""] <- NA
length(which(!is.na(userinfoDF$location )))/nrow(userinfoDF)
length(unique(userinfoDF$location)) #How many locations do they state?
#save the unique locations
write.csv(unique(userinfoDF$location), "tables/flickr_userlocation/Flickr_user_statedlocation_unique.csv", row.names=FALSE, fileEncoding="UTF-8")
#I then manually create a lookup table from these locations

#########################
### 3. Combine and save user info (owner, touristtype, usertype) ----
#add a column tourist_type to the list of owners using lookup table
lookup <- read_excel("tables/flickr_userlocation/Flickr_user_statedlocation_unique_OS.xlsx", sheet="Sheet1")
#some duplicates appear because R ignores spaces after text on the import eg "Anchorage, USA" and "Anchorage, USA " look the same to the merge function
lookup <- lookup[!duplicated(lookup$x),]
userinfoDF <- read.csv("tables/flickr_userlocation/Flickr_user_statedlocation.csv", header=TRUE, fileEncoding="UTF-8")
userinfoDF <- merge(userinfoDF, lookup, by.x="location", by.y="x", all.x=TRUE) 
userinfoDF <- userinfoDF[, c("owner", "location", "User_type", "User_type_revised", "local_country")]
names(userinfoDF)[3] <- "touristtype"
names(userinfoDF)[4] <- "touristtype_revised"
userinfoDF$touristtype[userinfoDF$touristtype=="NA"] <- NA
userinfoDF$touristtype_revised[userinfoDF$touristtype_revised=="NA"] <- NA

#add column listing the user type
userinfoDF$usertype <- "regular"
userinfoDF$usertype[userinfoDF$owner %in% superusers$owner] <- "superuser"
userinfoDF$usertype[userinfoDF$owner %in% testusers$owner] <- "testuser"
#save the whole dataset 
write.csv(userinfoDF, "tables/flickr_userlocation/Flickr_userinfo_tourist_or_superuser.csv", fileEncoding="UTF-8", row.names=FALSE)

#END#########################