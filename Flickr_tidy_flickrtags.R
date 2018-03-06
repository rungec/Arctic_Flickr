# Arctic CONNECT Project
# This script follows on from Flickr_input_exploration.r
# It tidies the tags that people have given the photos on flickr
# A separate script uses google vision to label the photos

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis"
setwd(wd)

### Setup ----
library(sf)
library(plyr)
library(tidyverse)
library(tm)

options(stringsAsFactors = FALSE)
options(tibble.width = Inf) #print all columns

### Load data ----
flickrshp <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate.shp")
#write.csv(data.frame(flickrshp$tags, flickrshp$title, flickrshp$url_m), "/tag_analysis/input/flickr_tags_and_titles_all_photos.csv")

length(unique(flickrshp$title))
length(unique(flickrshp$tags))


tags <- flickrshp$tags
#tags <- tags[1:20]
titles <- flickrshp$title
#titles <- titles[1:20]

### Tidy up data ----

#Tidy tags
taglist <- str_split(tags, pattern=" ", simplify=FALSE) #drop spaces
taglist <- lapply(taglist, function(currtags) {
  newtags <- currtags[which(str_detect(currtags, "[0-9]")==FALSE)]
}) #drop any words containing numbers

#Tidy titles
stopwords <- c("and", "this", "the", "of", "a") #define stopwords
titlelist <- str_replace_all(titles, "[[:punct:]]", "") #get rid of punctuation
titlelist <- tolower(titlelist) #lowercase
titlelist <- str_split(titlelist, pattern=" ", simplify=FALSE) #each word becomes an item in a vector, drop spaces
titlelist <- lapply(titlelist, function(currtitles){
  newtitles <- currtitles[which(str_detect(currtitles, "[0-9]")==FALSE)] #drop any words containing numbers
  newtitles <- newtitles[!newtitles %in% stopwords] #drop stopwords 
  newtitles <- newtitles[which(str_length(newtitles)>0)] #drop any blank entries
  }) 

### Calculate stats on data ----

#Number of unique tag words
length(unique(unlist(taglist)))

#Number of unique title words
length(unique(unlist(titlelist)))

#frequency table of tags
freq_tags <- plyr::count(unlist(taglist))
write.csv(freq_tags, "/tag_analysis/output/frequency_of_flickr_tag_words.csv")

#frequency table of titles
freq_titles <- plyr::count(unlist(titlelist))
write.csv(freq_titles, "/tag_analysis/output/frequency_of_flickr_title_words.csv")



