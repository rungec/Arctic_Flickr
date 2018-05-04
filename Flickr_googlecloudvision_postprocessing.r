#this script summarises the Google Cloud Vision https://cloud.google.com/vision/ tag for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r

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
load("input/Flickr_Artic_60N_plus_flickr_labels_urban.Rdata")
flickrshp <- flickrshp_tags
rm(flickrshp_tags)					

########################
#set up functions

#set up wordcloud plotfun
wordplotfun <- function(words, freq, outname, ...){
  png(filename = sprintf("output/byregion/Google_labels_wordcloud_%s.png", outname), width=14, height=14, units="in", type='windows', antialias = "cleartype", res=600)
  wordcloud(words=words, freq=freq, random.order=FALSE, scale=c(10,1.5), family="Times New Roman", ...)
  dev.off()
  }


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
    save(allgoogle,file=sprintf("output/byregion/Google_output_%s.Rdata", curregname)) #save the dataset
    }

#########################
#Summarise the data for each photo ----
########################
#make a table of the top google tags & their scores 
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  load(sprintf("output/byregion/Google_output_%s.Rdata", curregname))
  
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
write.csv(summaryDF, sprintf("output/byregion/Google_labels_summary_foreachphoto_%s.csv", curregname), row.names = TRUE, fileEncoding = "UTF-8")
print(paste("Finished", curregname, length(which(is.na(summaryDF$numtags))), "NAs of", nrow(summaryDF), sep=" "))

}

########################
# Frequency of all tags scoring above 60
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  summaryDF <- read.csv(sprintf("output/byregion/Google_labels_summary_foreachphoto_%s.csv", curregname), header=TRUE)
  
  tagfreq <- plyr::count(unlist(summaryDF[grep("googletag", names(summaryDF), value=FALSE)])) #count how frequently each tag is used
  #drop the unlist to get the frequency by column (i.e the most frequent first word etc)
    write.csv(tagfreq, sprintf("output/byregion/Frequency_of_google_labels_overscore60_%s.csv", curregname), fileEncoding = "UTF-8", row.names=FALSE)
	
	#plot wordcloud
	wordplotfun(tagfreq$x, tagfreq$freq, outname=curregname, max.words=100, rot.per=0)
} 

#########################
# Combine frequencies for all regions
filelist <- list.files("output/byregion/", pattern="Frequency_of_google_labels_overscore60")
allfreqL <- lapply(filelist, function(i) {
			a <- read.csv(paste0("output/byregion/", i), header=TRUE, fileEncoding="UTF-8")
			return(a)
			})
allfreqDF <- do.call(rbind, allfreqL)
allfreq <- allfreqDF %>% group_by(x) %>% summarise(freq=sum(freq)) 			
write.csv(allfreq, "output/Frequency_of_google_labels_overscore60_Arctic.csv", fileEncoding="UTF-8", row.names=FALSE)
wordplotfun(allfreq$x, allfreq$freq, outname="Arctic", max.words=100, rot.per=0)


#########################
#Make a merged dataset ----
#########################
#contains flickr tags & titles, google labels, whether urban or not, whether superuser or not, whether tourist or local.
#load flickrshp
load("input/Flickr_Artic_60N_plus_flickr_labels_urban.Rdata")
flickrshp <- flickrshp_tags
rm(flickrshp_tags)

#load userinfo
userinfoDF <- read.csv("tables/Flickr_userinfo_tourist_or_superuser.csv", fileEncoding="UTF-8", header=TRUE)

#load and combine google labels for each region
filelist <- list.files("output/byregion/", pattern="Google_labels_summary_foreachphoto_", full.names = TRUE)
allphotosL <- lapply(filelist, function(i) {
			a <- read.csv(i, header=TRUE)
			return(a)
			})
allphotos <- do.call(rbind, allphotosL)

#Merge user info with flickrshp
flickrshp <- merge(flickrshp, userinfoDF, by.x="owner", by.y="owner", all.x=TRUE)
#merge google labels with flickrshp
flickrshp <- merge(flickrshp, allphotos, by.x="id", by.y="id", all.x=TRUE)
save(flickrshp, file="output/Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_urban.Rdata")


###END