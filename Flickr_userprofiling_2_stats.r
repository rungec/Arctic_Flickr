#Calculate some statistics on flickr tourism

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(tidyverse)


ownerstats <- read.csv("tables/Flickr_user_trip_summary.csv", header=TRUE)

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
sink("tables/Flickr_user_trip_statistics.txt")

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
print(paste("superuser", "regularuser", "tripcentroid_distance_from_usercentroid", sep=", "))
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


