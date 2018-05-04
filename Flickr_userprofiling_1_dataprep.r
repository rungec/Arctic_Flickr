#This script does 2 things
# a. Identifies superusers and tourists and creates the lookup table Flickr_userinfo_tourist_or_superuser.csv
# b. Determines (a) what proportion of users are tourists/superusers, and (b) if they take photos in different places (regions, inside/outside cities) from locals/regular


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
load("tag_analysis/input/Flickr_Artic_60N_plus_flickr_labels_urban.Rdata")
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
write.csv(userinfoDF, "tables/Flickr_user_statedlocation.csv", row.names=FALSE, fileEncoding="UTF-8")

# 2b. What prop of users have stated their location
userinfoDF$location[userinfoDF$location==""] <- NA
length(which(!is.na(userinfoDF$location )))/nrow(userinfoDF)
length(unique(userinfoDF$location)) #How many locations do they state?
#save the unique locations
write.csv(unique(userinfoDF$location), "tables/Flickr_user_statedlocation_unique.csv", row.names=FALSE, fileEncoding="UTF-8")
#I then manually create a lookup table from these locations

#########################
### 3. Combine and save user info (owner, touristtype, usertype) ----
#add a column tourist_type to the list of owners using lookup table
lookup <- read_excel("tables/Flickr_user_statedlocation_unique_OS.xlsx", sheet="Sheet1")
#some duplicates appear because R ignores spaces after text on the import eg "Anchorage, USA" and "Anchorage, USA " look the same to the merge function
lookup <- lookup[!duplicated(lookup$x),]
userinfoDF <- read.csv("tables/Flickr_user_statedlocation.csv", header=TRUE, fileEncoding="UTF-8")
userinfoDF <- merge(userinfoDF, lookup, by.x="location", by.y="x", all.x=TRUE) 
userinfoDF <- userinfoDF[, c("owner", "location", "User_type")]
names(userinfoDF)[3] <- "touristtype"
userinfoDF$touristtype[userinfoDF$touristtype=="NA"] <- NA

#add column listing the user type
userinfoDF$usertype <- "regular"
userinfoDF$usertype[userinfoDF$owner %in% superusers$owner] <- "superuser"
userinfoDF$usertype[userinfoDF$owner %in% testusers$owner] <- "testuser"
#save the whole dataset 
write.csv(userinfoDF, "tables/Flickr_userinfo_tourist_or_superuser.csv", fileEncoding="UTF-8", row.names=FALSE)

#########################
### 4. User stats ----
# 4a. What prop tourists in each region?
#how many superusers, regular users in the different regions
#how many tourists vs locals in the different regions

#Merge user info with flickrshp
flickrshp <- st_set_geometry(flickrshp, NULL)
flickrshp <- merge(flickrshp, userinfoDF, by.x="owner", by.y="owner", all.x=TRUE)

#set up function
user_propfun <- function(data, outname) {
  user_prop1 <- lapply(unique(data$region), function(curreg){
				a <- flickrshp[data$region==curreg,]
				n_usertype <-  a %>% group_by(usertype) %>% 
										summarise(n_user_type = n_distinct(owner) )
				n_touristtype <-  a %>% group_by(touristtype) %>% 
										summarise(n_user_type = n_distinct(owner))
				names(n_touristtype)[1] <- "usertype"						
				n_usertype <- rbind(n_usertype, n_touristtype)						
				perc_user_type <-  n_usertype$n_user_type/n_distinct(a$owner)
				return(data.frame(region=curreg, n_usertype, perc_user_type))
				})
  user_prop1 <- do.call(rbind, user_prop1)				
write.csv(user_prop1, sprintf("tables/Flickr_user_types_%s.csv", outname), row.names=FALSE)
}

#run user function on all data
user_propfun(flickrshp, "by_region")
#run user function outside cities
flickrshp_sub <- flickrshp[is.na(flickrshp$InCity), ]
user_propfun(flickrshp_sub, "by_region_outsideCities")

#What % of superusers are tourists?
user_prop2 <- flickrshp %>% group_by(usertype, touristtype, InCity) %>% 
							summarise(n_user_tourist_type = n_distinct(owner),
									perc_user_tourist_type = n_distinct(owner)/n_distinct(flickrshp$owner))

write.csv(user_prop2, "tables/Flickr_user_types_overallproportions_inandoutsideCities.csv", row.names=FALSE)

user_prop2 <- flickrshp %>% group_by(usertype, touristtype) %>% 
							summarise(n_user_tourist_type = n_distinct(owner),
									perc_user_tourist_type = n_distinct(owner)/n_distinct(flickrshp$owner))

write.csv(user_prop2, "tables/Flickr_user_types_overallproportions.csv", row.names=FALSE)

#is the number of tourists vs locals in superusers drawn from same distribution as regular users
#drop NAs
userinfoDF <- read.csv("tables/Flickr_userinfo_tourist_or_superuser.csv", fileEncoding="UTF-8", header = TRUE)
userinfoDFnoNAs <- userinfoDF[complete.cases(userinfoDF),]

#drop testuser
userinfoDFnoNAs <- userinfoDFnoNAs[!userinfoDFnoNAs$usertype=="testuser", ] %>% droplevels() 
u1 <- table(userinfoDFnoNAs$usertype, userinfoDFnoNAs$touristtype)
u1_chi <- chisq.test(u1, simulate.p.value = TRUE, B=10000)
#combine domestic and locals
userinfoDFnoNAs$touristtype[userinfoDFnoNAs$touristtype=="domestic"] <- "local"
userinfoDFnoNAs <- droplevels(userinfoDFnoNAs)
u2 <- table(userinfoDFnoNAs$touristtype, userinfoDFnoNAs$usertype)
u2_chi <- chisq.test(u2, simulate.p.value = TRUE, B=10000)
#save chisq results
sink("tables/Flickr_userstats_chisq.txt")
  print("Is the number of tourists vs locals in superusers drawn from same distribution as regular users")
  print("drop test users")
  print(u1_chi)
  print("Actual n users")
  print(u1)
  print("Expected n users")
  print(u1_chi$expected)
  print("combine locals and domestics")
  print(u2_chi)
  print("Actual n users")
  print(u2)
  print("Expected n users")
  print(u2_chi$expected)

#is the number of tourists vs locals in cities drawn from same distribution as outside cities
flickrshp$InCity[is.na(flickrshp$InCity)] <- 0
subdat <- flickrshp[flickrshp$usertype %in% c("regular", "superuser") & !is.na(flickrshp$touristtype), ] 

#do tourists take more photos inside cities
u3 <- table(subdat$InCity, subdat$touristtype)
u3_chi <- chisq.test(u3, simulate.p.value = TRUE, B=10000)
  print("do tourists take more photos inside cities")
  print(u3_chi)
  print("Actual n photos")
  print(u3)
  print("Expected n photos")
  print(u3_chi$expected)

#do tourists take more photos inside cities, excluding superusers
subdatb <- subdat[!subdat$usertype=="superuser", ]  %>% droplevels()
u3b <- table(subdatb$InCity, subdatb$touristtype)
u3b_chi <- chisq.test(u3b, simulate.p.value = TRUE, B=10000)
  print("do tourists take more photos inside cities, excluding superusers")
  print(u3b_chi)
  print("Actual n photos")
  print(u3b)
  print("Expected n photos")
  print(u3b_chi$expected)
  
  
#do superusers take more photos inside cities
u4 <- table(subdat$InCity, subdat$usertype)
u4_chi <- chisq.test(u4, simulate.p.value = TRUE, B=10000)
  print("do superusers take more photos inside cities")
  print(u4_chi)
  print("Actual n photos")
  print(u4)
  print("Expected n photos")
  print(u4_chi$expected)

#are there more tourists or locals outside cities
user_prop5 <- subdat %>% droplevels() %>% group_by(InCity, touristtype) %>% 
  summarise(n_user_tourist_type = n_distinct(owner))
u5 <- xtabs(n_user_tourist_type ~ InCity + touristtype, data=user_prop5)
u5_chi <- chisq.test(u5, simulate.p.value = TRUE, B=10000)
  print("are there more tourists inside cities")
  print(u5_chi)
  print("Actual n users")
  print(u5)
  print("Expected n users")
  print(u5_chi$expected)
  
#are there more tourists or locals outside cities #drop domestic
user_prop5b <- subdat[!subdat$touristtype=="domestic", ] %>% droplevels() %>% group_by(InCity, touristtype) %>% 
  summarise(n_user_tourist_type = n_distinct(owner))
u5b <- xtabs(n_user_tourist_type ~ InCity + touristtype, data=user_prop5b)
u5b_chi <- chisq.test(u5b, simulate.p.value = TRUE, B=10000)
  print("are there more tourists inside cities, excluding domestic")
  print(u5b_chi)
  print("Actual n users")
  print(u5b)
  print("Expected n users")
  print(u5b_chi$expected)

#are there more regular or superusers outside cities
user_prop6 <- subdat %>% droplevels() %>% group_by(InCity, usertype) %>% 
  summarise(n_user_user_type = n_distinct(owner))
u6 <- xtabs(n_user_user_type ~ InCity + usertype, data=user_prop6)
u6_chi <- chisq.test(u6, simulate.p.value = TRUE, B=10000)
  print("are there more superusers inside cities")
  print(u6_chi)
  print("Actual n users")
  print(u6)
  print("Expected n users")
  print(u6_chi$expected)
sink()  

summary(as.factor(flickrshp$InCity))

