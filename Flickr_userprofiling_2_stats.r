#Calculate some statistics on flickr tourism
#Determines 
# (a) what proportion of users are tourists/superusers, and 
# (b) if they take photos in different places (regions, inside/outside cities) from locals/regular
# (c) if they take longer or more regular trips than locals

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tables/summary_userprofile"
setwd(wd)

### Setup ----
library(sf)
library(tidyverse)

#########################
#Setup files
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_tidy.Rdata")
#flickrshp, plus googletags, usertype, touristtype
#2004 to 2017, no testusers
#drop geometry
flickrshp <- st_set_geometry(flickrshp, NULL)


#########################
### 1. User stats ----

#set up function
user_propfun <- function(data, outname) {
  user_prop1 <- lapply(unique(data$region), function(curreg){
    a <- flickrshp[data$region==curreg,]
    n_usertype <-  a %>% group_by(usertype) %>% 
      summarise(n_users = n_distinct(owner) )
    n_touristtype <-  a %>% group_by(touristtype_revised) %>% 
      summarise(n_users = n_distinct(owner))
    names(n_touristtype)[1] <- "usertype"						
    n_usertype <- rbind(n_usertype, n_touristtype)						
    perc_usertype <-  n_usertype$n_users/n_distinct(a$owner)
    return(data.frame(region=curreg, n_usertype, perc_usertype))
  })
  user_prop1 <- do.call(rbind, user_prop1)				
  write.csv(user_prop1, sprintf("Flickr_usertypes_revised_%s.csv", outname), row.names=FALSE)
}

#########################
# 1a. What prop tourists in each region?
#how many superusers, regular users in the different regions
#how many tourists vs locals in the different regions

#run user function on all data
user_propfun(flickrshp, "by_region")
#run user function outside cities
flickrshp_sub <- flickrshp[is.na(flickrshp$InCity), ]
user_propfun(flickrshp_sub, "by_region_outsideCities")

# 1b. What % of superusers are tourists?
user_prop2 <- flickrshp %>% group_by(usertype, touristtype_revised, InCity) %>% 
  summarise(n_user_tourist_type = n_distinct(owner),
            perc_user_tourist_type = n_distinct(owner)/n_distinct(flickrshp$owner))

write.csv(user_prop2, "Flickr_usertypes_revised_overallproportions_inandoutsideCities.csv", row.names=FALSE)

user_prop2 <- flickrshp %>% group_by(usertype, touristtype_revised) %>% 
  summarise(n_user_tourist_type = n_distinct(owner),
            perc_user_tourist_type = n_distinct(owner)/n_distinct(flickrshp$owner))

write.csv(user_prop2, "Flickr_usertypes_revised_overallproportions.csv", row.names=FALSE)

# 1c. is the number of tourists vs locals in superusers drawn from same distribution as regular users
userinfoDF <- read.csv(paste0(dirname(wd), "/flickr_userlocation/Flickr_userinfo_tourist_or_superuser.csv"), fileEncoding="UTF-8", header = TRUE)
#drop testuser
userinfoDFnotest <- userinfoDF[!userinfoDF$usertype=="testuser", ] %>% droplevels() 
u1 <- table(userinfoDFnotest$usertype, userinfoDFnotest$touristtype_revised)
u1_chi <- chisq.test(u1, simulate.p.value = TRUE, B=10000)
#combine domestic and locals
#userinfoDFnoNAs$touristtype[userinfoDFnoNAs$touristtype=="domestic"] <- "local"
# userinfoDFnoNAs <- droplevels(userinfoDFnoNAs)
# u2 <- table(userinfoDFnoNAs$touristtype, userinfoDFnoNAs$usertype)
# u2_chi <- chisq.test(u2, simulate.p.value = TRUE, B=10000)
#save chisq results
sink("Flickr_userstats_chisq_revised.txt")
print("number of photos in Arctic cities (above 60N) inside=1, outside=0")
print(summary(as.factor(flickrshp$InCity)))
print("Is the number of tourists vs locals in superusers drawn from same distribution as regular users")
print("drop test users")
print(u1_chi)
print("Actual n users")
print(u1)
print("Expected n users")
print(u1_chi$expected)
#print("combine locals and domestics")
#print(u2_chi)
#print("Actual n users")
#print(u2)
#print("Expected n users")
#print(u2_chi$expected)

#is the number of tourists vs locals in cities drawn from same distribution as outside cities
flickrshp$InCity[is.na(flickrshp$InCity)] <- 0
subdat <- flickrshp[flickrshp$usertype %in% c("regular", "superuser") & !is.na(flickrshp$touristtype_revised), ] 
subdat <- droplevels(subdat)

#do tourists take more photos inside cities
u3 <- table(subdat$InCity, subdat$touristtype_revised)
u3_chi <- chisq.test(u3, simulate.p.value = TRUE, B=10000)
print("do tourists take more photos inside cities")
print(u3_chi)
print("Actual n photos")
print(u3)
print("Expected n photos")
print(u3_chi$expected)

# 1d. do tourists take more photos inside cities, excluding superusers
subdatb <- subdat[!subdat$usertype=="superuser", ]  %>% droplevels()
u3b <- table(subdatb$InCity, subdatb$touristtype_revised)
u3b_chi <- chisq.test(u3b, simulate.p.value = TRUE, B=10000)
print("do tourists take more photos inside cities, excluding superusers")
print(u3b_chi)
print("Actual n photos")
print(u3b)
print("Expected n photos")
print(u3b_chi$expected)

# 1e. do superusers take more photos inside cities
u4 <- table(subdat$InCity, subdat$usertype)
u4_chi <- chisq.test(u4, simulate.p.value = TRUE, B=10000)
print("do superusers take more photos inside cities")
print(u4_chi)
print("Actual n photos")
print(u4)
print("Expected n photos")
print(u4_chi$expected)

# 1f. are there more tourists or locals outside cities
user_prop5 <- subdat %>% droplevels() %>% group_by(InCity, touristtype_revised) %>% 
  summarise(n_user_tourist_type = n_distinct(owner))
u5 <- xtabs(n_user_tourist_type ~ InCity + touristtype_revised, data=user_prop5)
u5_chi <- chisq.test(u5, simulate.p.value = TRUE, B=10000)
print("are there more tourists inside cities")
print(u5_chi)
print("Actual n users")
print(u5)
print("Expected n users")
print(u5_chi$expected)

# #are there more tourists or locals outside cities #drop domestic
# user_prop5b <- subdat[!subdat$touristtype=="domestic", ] %>% droplevels() %>% group_by(InCity, touristtype) %>% 
#   summarise(n_user_tourist_type = n_distinct(owner))
# u5b <- xtabs(n_user_tourist_type ~ InCity + touristtype, data=user_prop5b)
# u5b_chi <- chisq.test(u5b, simulate.p.value = TRUE, B=10000)
#   print("are there more tourists inside cities, excluding domestic")
#   print(u5b_chi)
#   print("Actual n users")
#   print(u5b)
#   print("Expected n users")
#   print(u5b_chi$expected)

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





#############################
### Are super users different
#do they go further?
#do they use different flickr tags?
#do they photograph different objects (google vision)?
#https://github.com/GoranMilovanovic/Distributional-Semantics-in-R
#http://docs.quanteda.io/
#http://www.mjdenny.com/Text_Processing_In_R.html


#Stats on trip distance ----
ownerstats <- read.csv("Flickr_user_trip_summary.csv", header=TRUE)

#function to calculate some summary stats
statsfun <- function(data) {
 return(t(data %>% summarise(numphotos_avg=mean(numphotos_trip),
                     numphotos_med=median(numphotos_trip),
                     numphotos_iqr=IQR(numphotos_trip),
                     travel_dist_avg_km=mean(tripdist_km), 
                     travel_dist_med_km=median(tripdist_km), 
                     travel_dist_iqr_km=IQR(tripdist_km), 
                     trip_length_avg_days=mean(triplength_days), #for tripid=0 this is time as a flikr user
                     trip_length_med_days=median(triplength_days), 
                     trip_length_iqr_days=IQR(triplength_days), 
                     max_ranging_distance_avg_km=mean(maxtripdist_from_centroid), 
                     max_ranging_distance_med_km=median(maxtripdist_from_centroid), 
                     max_ranging_distance_iqr_km=IQR(maxtripdist_from_centroid), 
                     avg_ranging_distance_avg_km=mean(avgtripdist_from_centroid), 
                     avg_ranging_distance_med_km=median(avgtripdist_from_centroid), #for tripid=0 this is the median of the user averages
                     avg_ranging_distance_iqr_km=IQR(avgtripdist_from_centroid), 
					 trip_distance_from_usercentroid_mean_km=mean(tripcentroid_distance_from_usercentroid, na.rm=TRUE),  
					 trip_distance_from_usercentroid_med_km=median(tripcentroid_distance_from_usercentroid, na.rm=TRUE), #for tripid=0 this is the median of the user averages
					 trip_distance_from_usercentroid_IQR_km=IQR(tripcentroid_distance_from_usercentroid, na.rm=TRUE),
                     numtrips_avg=mean(numtrips, na.rm=TRUE), 
                     numtrips_med=median(numtrips, na.rm=TRUE), 
                     numtrips_iqr=IQR(numtrips, na.rm=TRUE) 
                     )))
}

#function to calculate the difference between groups
mwtestfun <- function(data1, data2, x) {
  wilcox.test(data1[,x], data2[,x])
  }

#select the different user groups
superuser <- ownerstats %>% filter(tripid==0 & usertype=="superuser") #people who contribute > 50% photos
regularuser <- ownerstats %>% filter(tripid==0 & usertype=="regular") #people who contribute between n=2 and 50% photos
testuser <- ownerstats %>% filter(tripid==0 & usertype=="testuser") #people who contribute between n=2 and 50% photos
alluser <- ownerstats %>% filter(tripid==0 & usertype=="regular" | tripid==0 & usertype=="superuser") #both super & regular users, excluding people who contribute just 1 or 2 photos (we assume they are just trialling flickr)

#select the trips for the different user groups
superuser_trips <- ownerstats %>% filter(tripid >0 & usertype=="superuser")
regularuser_trips <- ownerstats %>% filter(tripid >0 & usertype=="regular")

#calculate summary stats for the different groups
userstats <- data.frame(superusers=statsfun(superuser), 
                  regularusers=statsfun(regularuser), 
                  testusers=statsfun(testuser),
                  allusers=statsfun(alluser))

#calculate summary stats for trips of the different groups
usertripstats <- data.frame(superusers_pertrip=statsfun(superuser_trips), 
                  regularusers_pertrip=statsfun(regularuser_trips))


#are the groups statistically different?
sink("Flickr_user_trip_statistics.txt")

print("see script Flickr_userprofiling_2_stats.r")
print("")
print("All users : those who contribute more than 2 photos")
print("Super users : people who account for 50% of flickr data, contribute > 471 photos")
print("Regular users : the other 50% - 3-470 photos")
print("Test users : contribute only 1 or 2 photos")
print("")
print(userstats)
print("")
print("wilcox-tests of the user groups")
print("Have superusers travel further in their total time in the Arctic?")
print(paste("superuser", "regularuser", "tripdist_km", sep=", "))
print(mwtestfun(superuser, regularuser, "tripdist_km"))
print("Have superusers been uploading Arctic photos for longer?")
print(paste("superuser", "regularuser", "triplength_days", sep=", "))
print(mwtestfun(superuser, regularuser, "triplength_days"))
print("Do superusers take more trips?")
print(paste("superuser", "regularuser", "numtrips", sep=", "))
print(mwtestfun(superuser, regularuser, "numtrips"))
print("Are superusers more wide-ranging (average distance of their trip centroids from user centroid)?")
print("excluding users with only one trip")
print(paste("superuser", "regularuser", "tripcentroid_distance_from_usercentroid", sep=", "))
print(paste(nrow(ownerstats[ownerstats$tripid==0 & ownerstats$numtrips>1 & ownerstats$usertype=="superuser",]), "superusers", sep=" "))
print(paste(nrow(ownerstats[ownerstats$tripid==0 & ownerstats$numtrips>1 & ownerstats$usertype=="regular",]), "regular users", sep=" "))
print(mwtestfun(superuser, regularuser, "tripcentroid_distance_from_usercentroid"))
print("Are superusers more wide-ranging (max distance of all their photos from user centroid)?")
print(paste("superuser", "regularuser", "maxtripdist_from_centroid", sep=", "))
print(mwtestfun(superuser, regularuser, "maxtripdist_from_centroid"))
print("Are superusers more wide-ranging (avg distance of all their photos from user centroid)?")
print(paste("superuser", "regularuser", "avgtripdist_from_centroid", sep=", "))
print(mwtestfun(superuser, regularuser, "avgtripdist_from_centroid"))
print(usertripstats)
print("Do superusers travel further per trip?")
print(paste("superuser_trips", "regularuser_trips", "tripdist_km", sep=", "))
print(mwtestfun(superuser_trips, regularuser_trips, "tripdist_km"))
print("Do superusers contribute more photos per trip?")
print(paste("superuser_trips", "regularuser_trips", "numphotos_trip"))
print(mwtestfun(superuser_trips, regularuser_trips, "numphotos_trip"))
print("Do superusers take longer trips?")
print(paste("superuser_trips", "regularuser_trips", "triplength_days", sep=", "))
print(mwtestfun(superuser_trips, regularuser_trips, "triplength_days"))
print("Are superusers more wide-ranging (average distance of each photo in a trip from trip centroid)?")
print(paste("superuser_trips", "regularuser_trips", "avgtripdist_from_centroid", sep=", "))
print(mwtestfun(superuser_trips, regularuser_trips, "avgtripdist_from_centroid"))
print("Are superusers more wide-ranging per trip (max distance of trip photos from trip centroid)?")
print(paste("superuser_trips", "regularuser_trips", "maxtripdist_from_centroid", sep=", "))
print(mwtestfun(superuser_trips, regularuser_trips, "maxtripdist_from_centroid"))

sink()



