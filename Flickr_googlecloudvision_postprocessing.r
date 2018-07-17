#this script summarises the Google Cloud Vision https://cloud.google.com/vision/ tag for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r and preludes Flickr_googlecloudvision_datasummary.r

### Set up libraries ----
library(sf)
library(tidyverse)
library(wordcloud)
library(extrafont)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
#wd <- "/home/cru016@ad.uit.no/Documents/tag_analysis"
setwd(wd)
wd2 <- "D:/temp/gvisontemp"
 
#set list of regions to process
regionlist <- list(IcelandGreenland=c("Iceland", "Greenland"),
                    NorthAmerica = c("Alaska", "Canada"), 
                    Scandinavia=c("Norway", "Sweden"), 
                    Finland=c("Finland", "Aland"), 
                    Russia=c("Russia"), 
                    Marine=c("Marine"), 
                    OtherIslands =c("Faroe.Islands", "United.Kingdom"))

#load flickrshp
flickrshp <- load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_byregion_laea_icelandupdate.Rdata")
flickrshp <- all.sf	
rm(all.sf)


### Post processing ---
########################
## Combine data ----
########################
#about 5.6gb total so we do it by region
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
#  flickrshp_sub <- flickrshp[flickrshp$region %in% curregion[[1]], ]
  
  #bind all the intermediate google files for a given region  
    filelist <- list.files(paste(wd2, curregname, sep="/"), full.names=TRUE)
    allgoogle <- list() #set up empty list
      for(y in 1:length(filelist)){
        load(filelist[y])
        allgoogle <- append(allgoogle, google)
      }
    save(allgoogle,file=sprintf("googlevision/google_output/Google_output_%s.Rdata", curregname)) #save the dataset
    }

#########################
#Summarise the data for each photo ----
########################
#make a table of the top google tags & their scores 
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  load(sprintf("googlevision/google_output/Google_output_%s.Rdata", curregname))
  
  #add a summary of the data
	b <- list() 
	for(d in 1:length(allgoogle)){
			x <- allgoogle[[d]]
			if( is.null(x[[1]]) ){
				df_currphoto <- data.frame(numtags=NA)
			} else if (grepl("error", x, ignore.case=TRUE))	{
				df_currphoto <- data.frame(numtags=NA)
			} else if (names(x[[1]][1])=="error" )	{
				df_currphoto <- data.frame(numtags=NA)
			} else {
				numtags <- nrow(x[[1]]) #number of tags for each photo
				#all tags with scores over 60
				toptags <- x[[1]]$description[(x[[1]]$score >= 0.6)]
				topscores <- x[[1]]$score[(x[[1]]$score >= 0.6)]
				
				if( length(toptags) > 0 ){ #exclude cases where no labels that score above 0,6
					df_currphoto <- data.frame(numtags, t(toptags), t(topscores))
					names(df_currphoto)[2:length(df_currphoto)] <- paste0(rep(c("googletag", "googlescore"), each=length(toptags)), 1:length(toptags))
				} else {
					df_currphoto <- data.frame(numtags)
				}
			} 
	  b[[d]] <- df_currphoto
	}
	summaryDF <- do.call(plyr::rbind.fill, b)
	summaryDF <- data.frame(id=names(allgoogle), summaryDF)
write.csv(summaryDF, sprintf("googlevision/google_output/Google_labels_summary_foreachphoto_%s.csv", curregname), row.names = TRUE, fileEncoding = "UTF-8")
print(paste("Finished", curregname, length(which(is.na(summaryDF$numtags))), "NAs of", nrow(summaryDF), sep=" "))

}

#########################
#Make a merged dataset ----
#########################
#contains flickr tags & titles, google labels, whether urban or not, whether superuser or not, whether tourist or local.
#load flickrshp
#flickrshp <- load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_byregion_laea_icelandupdate.Rdata")
#flickrshp <- all.sf
#drop photos pre 2000 and from 2018 or later
flickrshp <- flickrshp[flickrshp$year<2018 & flickrshp$year>2001, ]
#drop rows missing urls
flickrshp <- flickrshp[!is.na(flickrshp$url_m), ]
#drop unneeded cols
keepcols <- which(!names(flickrshp) %in% c("Aland", "Canada", "Finland", "Faroe.Islands", 
                                           "United.Kingdom", "Greenland", "Iceland", "Norway", "Russia", 
                                           "Sweden", "Alaska", "Marine", "NAME_EN"))
flickrshp <- flickrshp[, keepcols]

#load userinfo
#this file was generated in Flickr_userprofiling_1_dataprep.r
userinfoDF <- read.csv(paste0(dirname(wd), "/tables/flickr_userlocation/Flickr_userinfo_tourist_or_superuser.csv"), fileEncoding="UTF-8", header=TRUE)

#load and combine google labels for each region
filelist <- list.files("googlevision/byregion/", pattern="Google_labels_summary_foreachphoto_", full.names = TRUE)
allphotosL <- lapply(filelist, function(i) {
			a <- read.csv(i, header=TRUE)
			return(a)
			})
allphotos <- do.call(rbind, allphotosL)
#reorder
allphotos <- allphotos[, c("id", "numtags", paste0(rep(c("googletag", "googlescore"), each=20), 1:20))]

#Merge user info with flickrshp
flickrshp <- merge(flickrshp, userinfoDF, by.x="owner", by.y="owner", all.x=TRUE)

#merge google labels with flickrshp
flickrshp <- merge(flickrshp, allphotos, by.x="id", by.y="id", all.x=TRUE)

#Create a new column, owner_date
flickrshp$owner_date <- paste(flickrshp$owner, as.Date(flickrshp$datetaken), sep="_")

#save
save(flickrshp, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo.Rdata")

#Tidy
#2004 to 2017 data
flickrshp <- flickrshp[flickrshp$year %in% as.factor(2004:2017), ]
#drop testusers
flickrshp <- flickrshp[flickrshp$usertype %in% c("superuser", "regular"), ]

#drop rows with no google words (this could be due to no url, or the user having taken down the photo or made it private)
flickrshp <- flickrshp[ rowSums(is.na( flickrshp[, grep("googletag", names(flickrshp))] )) < 20, ] #drop rows with all NAs

#save
save(flickrshp, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy.Rdata")


############################
#AMAP ----
############################
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy.Rdata")

###clip to AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp")
flickramap <- st_intersection(flickrshp, amap)

###Which points fall within which country
amap_regions <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_all_level_one_subdivisions_svalbard_simplified_EEZ.shp")
#drop duplicated rows - this is a weird error that happens, I can't find a cause for it
flickramap2 <- flickramap %>% filter(! duplicated(id))
flickramap <- flickramap2
rm(flickramap2)

###Which points fall within which country
amap_regions <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_all_level_one_subdivisions_svalbard_simplified_EEZ.shp")
spatialjoin <- st_intersects(flickramap, amap_regions, sparse = TRUE)
#tidy up the data. There is 1 row per row in flickramap
region_oid <- lapply(1:length(spatialjoin), function(x){
  data.frame(flickr_rowid=x, 
             amap_rowid=spatialjoin[[x]][1]) #country borders should not overlap, but in case they do, pick the first
})
region_oid2 <- do.call(rbind, region_oid)

#select the columns from the amap shp we want to merge with flickramap
amapdf <- st_set_geometry(amap_regions, NULL) #equivalent of sp@data
amapdf$amap_rowid <- as.integer(row.names(amapdf))
region_country <- merge(region_oid2, amapdf[, c("amap_rowid", "OBJECTID", "NAME_0", "NAME_1", "Country")], by.x="amap_rowid", by.y="amap_rowid", all.x=TRUE)
names(region_country) [3] <- "amap_OBJECTID"

#join to flickramap
flickramap <- bind_cols(flickramap, region_country[order(region_country$flickr_rowid), ])
#drop flickr_rowid col
flickramap <- flickramap %>% select(-flickr_rowid) 

#save
save(flickramap, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")


###END