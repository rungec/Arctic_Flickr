#Calculate some statistics on flickr tourism

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(tidyverse)


ownerstats <- read.csv("tables/Flickr_user_trip_summary.csv", header=TRUE)

#function to calculate some summary stats
statsfun <- function(data) {
 return(t(data %>% summarise(travel_dist_avg_km=mean(tripdist_km), 
                     travel_dist_med_km=median(tripdist_km), 
                     time_as_arctic_user_avg_days=mean(triplength_days), 
                     time_as_arctic_user_med_days=median(triplength_days), 
                     max_ranging_distance_avg_km=mean(maxtripdist_from_centroid), 
                     max_ranging_distance_med_km=median(maxtripdist_from_centroid), 
                     avg_ranging_distance_avg_km=mean(avgtripdist_from_centroid), 
                     avg_ranging_distance_med_km=median(avgtripdist_from_centroid), 
                     numtrips_avg=mean(numtrips), 
                     numtrips_med=median(numtrips) 
                     )))
}

#function to calculate the difference between groups
ttestfun <- function(data1, data2, x) {
  t.test(data1[,x], data2[,x], paired=FALSE)
  }

#select the different user groups
superuser <- ownerstats %>% filter(tripid==0 & superuser==1) #people who contribute > 473 photos
regularuser <- ownerstats %>% filter(tripid==0 & numphotos>1 & superuser==0) #people who contribute between 1 and 473 photos
alluser <- ownerstats %>% filter(tripid==0 & numphotos>1) #both super & regular users, excluding people who contribute just 1 photo (we assume they are just trialling flickr)

#select the trips for the different user groups
superuser_trips <- ownerstats %>% filter(tripid >0 & superuser==1)
regularuser_trips <- ownerstats %>% filter(tripid >0 & superuser==0)

#calculate summary stats for the different groups
userstats <- data.frame(superusers=statsfun(superuser), 
regularusers=statsfun(regularuser), 
allusers=statsfun(alluser))

#are the groups statistically different?
sink("tables/Flickr_user_trip_statistics.txt")

print("see script Flickr_userprofiling_2_stats.r")
print("")
print("All users : those who contribute more than 1 photo")
print("Super users : people who account for 50% of flickr data, contribute > 473 photos")
print("Regular users : the other 50% - 2-472 photos")
print("")
print(userstats)
print("")
print("t-tests of the user groups")
print("Have superusers travel further in their total time in the Arctic?")
print(paste("superuser", "regularuser", "tripdist_km", sep=", "))
print(ttestfun(superuser, regularuser, "tripdist_km"))
print("Have superusers been uploading Arctic photos for longer?")
print(paste("superuser", "regularuser", "triplength_days", sep=", "))
print(ttestfun(superuser, regularuser, "triplength_days"))
print("Are superusers more wide-ranging (average distance of all their photos from user centroid)?")
print(paste("superuser", "regularuser", "avgtripdist_from_centroid", sep=", "))
print(ttestfun(superuser, regularuser, "avgtripdist_from_centroid"))
print("Are superusers more wide-ranging (max distance of all their photos from user centroid)?")
print(paste("superuser", "regularuser", "maxtripdist_from_centroid", sep=", "))
print(ttestfun(superuser, regularuser, "maxtripdist_from_centroid"))
print("Do superusers travel further per trip?")
print(paste("superuser_trips", "regularuser_trips", "tripdist_km", sep=", "))
print(ttestfun(superuser_trips, regularuser_trips, "tripdist_km"))
print("Do superusers take longer trips?")
print(paste("superuser_trips", "regularuser_trips", "triplength_days", sep=", "))
print(ttestfun(superuser_trips, regularuser_trips, "triplength_days"))
print("Are superusers more wide-ranging (distance of trip centroids from user centroid)?")
print(paste("superuser_trips", "regularuser_trips", "avgtripdist_from_centroid", sep=", "))
print(ttestfun(superuser_trips, regularuser_trips, "avgtripdist_from_centroid"))
print("Are superusers more wide-ranging per trip (max distance from trip centroid)?")
print(paste("superuser_trips", "regularuser_trips", "maxtripdist_from_centroid", sep=", "))
print(ttestfun(superuser_trips, regularuser_trips, "maxtripdist_from_centroid"))

sink()

