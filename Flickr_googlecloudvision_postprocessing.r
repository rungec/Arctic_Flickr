#this script summarises the Google Cloud Vision https://cloud.google.com/vision/ tag for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r

### Set up libraries ----
library(sf)
library(tidyverse)

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


### Post processing ---
########################
## Combine data ----
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
#Summarise the data for each photo
#make a table of the top google tags & their scores 
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  allgoogle <- load(sprintf("output/byregion/Google_output_%s.Rdata", curregname))
  
  #add a summary of the data
	b <- list() 
	for(d in 1:length(allgoogle)){
			x <- allgoogle[[d]]
			if( is.null(x[[1]]) ){
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
write.csv(summaryDF, sprintf("output/byregion/Google_labels_summary_foreachphoto_%s.csv", curregname), row.names = TRUE)
print(paste("Finished", curregname, length(which(is.na(summaryDF$numtags))), "NAs of", nrow(summaryDF), sep=" "))

}

########################
# Frequency of all tags scoring above 60
for(i in seq_along(regionlist)){
  curregion <- regionlist[i]
  curregname <- names(curregion)
  summaryDF <- load(sprintf("output/byregion/Google_labels_summary_foreachphoto_%s.csv", curregname))
  
  tagfreq <- plyr::count(alltags[alltags$score>=0.6, "description"]) #count how frequently each tag is used
    write.csv(tagfreq, sprintf("output/byregion/Frequency_of_google_labels_overscore60_%s.csv", curregname), fileEncoding = "UTF-8")
}		




  
  
    #Join the google data to flickrshp and save it
    flickrshp_sub$google <- merge(flickrshp_sub, summaryDF, by.x="id", by.y="id", all.x=TRUE)
    save(flickrshp_sub,file=sprintf("output/byregion/Flickr_Artic_60N_plus_flickr_and_google_labels_%s.Rdata", curregname))
    
    #test that the ids match between the two datasets
    checkfun <- function(flickrshpt){
      counter=0
      for(i in 1:nrow(flickrshpt)){
        if(flickrshpt$id[i]!=as.numeric(names(flickrshpt$google)[[i]])) print(sprintf("ID mismatch row %s original %s google %s", i, flickrshpt$id[i], names(flickrshpt$google)[[i]]))
        else  (counter=counter+1)
      }
      print(sprintf("%s of %s ids match", nrow(flickrshpt), counter))
    }
    checkfun(flickrshp_sub)
  
  
  
  #function to save as shp
  st_write(google_summary, sprintf("output/byregion/Flickr_Artic_60N_plus_google_labels_%s.shp", curregname))
  google_summary_df <- google_summary %>% st_set_geometry(NULL) %>% data.frame()
    write.csv(google_summary_df, sprintf("output/byregion/Flickr_Artic_60N_plus_top10googlelabels_%s.csv", curregname), fileEncoding = "UTF-8")


  
  print(paste0("FINISHED POST PROCESSING for ", curregname))
}




                   



