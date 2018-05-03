# Arctic CONNECT Project
# This script follows on from Flickr_input_exploration.r
# Preceeds Flickr_googlecloudvision_label.r
# It tidies the tags that people have given the photos on flickr
# A separate script uses google vision to label the photos

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
setwd(wd)

### Setup ----
library(sf)
library(plyr)
library(tidyverse)

options(stringsAsFactors = FALSE)
options(tibble.width = Inf) #print all columns

### Load data ----
flickrshp <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate_urban.shp")

#A little function to search for particular text in the tags
#f <- for(i in 1:length(flickrshp$tags)){
#  if(grepl("vikisogn", flickrshp$tags[[i]])) print(flickrshp$tags[[i]])
#}

##############################
#Preliminary processing
#drop photos pre 2000 and from 2018 or later
flickrshp <- flickrshp[flickrshp$year<2018 & flickrshp$year>2000, ]
#drop rows missing urls
flickrshp <- flickrshp[!is.na(flickrshp$url_m), ]

length(unique(flickrshp$title))
length(unique(flickrshp$tags))
length(which(is.na(flickrshp$title)))
length(which(is.na(flickrshp$tags)))

tags <- flickrshp$tags
#tags <- tags[1:20000]
titles <- flickrshp$title
#titles <- titles[1:20000]
write.csv(data.frame(flickrshp$tags, flickrshp$title, flickrshp$url_m), "input/flickr_tags_and_titles_2001to2017_photoswithurl.csv", fileEncoding="UTF-8")


##############################
#Main processing
### Tidy up data ----

#Tidy tags
taglist <- str_replace_all(tags, "[\\$\\+\\^'~|¨¦´£¤°±¬¥!@#%&*()_:\"?,./;'-]", "") #get rid of punctuation, but not : or =
#taglist <- str_replace_all(taglist, "[\\<[:alpha:][:digit:]\\>]", " ") #replace <U+0093> etc with space
taglist <- str_replace_all(taglist, "[\\{\\}]", " ") #replace {} with space
taglist <- str_split(taglist, pattern=" ", simplify=FALSE) #drop spaces
taglist <- lapply(taglist, function(currtags) {
  newtags <- currtags[which(str_detect(currtags, "[0-9]")==FALSE)] #drop any words containing numbers
  newtags <- newtags[which(str_detect(newtags, "[\\©]")==FALSE)] #drop any words containing ©
  newtags <- newtags[which(str_detect(newtags, "copyright")==FALSE)] #drop any words containing ©
  newtags <- newtags[which(str_detect(newtags, "exif")==FALSE)] #drop any words containing exif
  newtags <- newtags[which(str_length(newtags)>0)] #drop any blank entries
}) 

#Tidy titles
stopwords <- c("and", "this", "the", "of", "a", "in", "at", "on", "to", "from", "i", "for", "with", "de", "la", "is", "på", "by", "my", "og", "vs", "en", "it", "up", "you", "near", "an", "one", "our", "med", "img", "dsc", "jpg", "imgjpg", "dscjpg") #define stopwords
titlelist <- str_replace_all(titles, "[\\$\\+\\^'~|¨¦´£¤°±¬¥!@#%&*()_:\"?,/;'-\\=\\[\\]\\>]", "") #get rid of punctuation, but not : or =
titlelist <- str_replace_all(titlelist, "[:punct:]", "") #get rid of punctuation
#titlelist <- str_replace_all(titlelist, "[\\>]", " ") #replace > or < with space
titlelist <- str_replace_all(titlelist, "[\\{\\}]", "") #replace {} with space
titlelist <- tolower(titlelist) #lowercase
titlelist <- str_split(titlelist, pattern=" ", simplify=FALSE) #each word becomes an item in a vector, drop spaces
titlelist <- lapply(titlelist, function(currtitles){
  newtitles <- currtitles[which(str_detect(currtitles, "[0-9]")==FALSE)] #drop any words containing numbers
  newtitles <- newtitles[which(str_detect(newtitles, "[\\©]")==FALSE)] #drop any words containing ©
  newtitles <- newtitles[which(str_detect(newtitles, "copyright")==FALSE)] #drop any words containing ©
  newtitles <- newtitles[which(str_detect(newtitles, "exif")==FALSE)] #drop any words containing exif
  newtitles <- newtitles[!newtitles %in% stopwords] #drop stopwords 
  newtitles <- newtitles[which(str_length(newtitles)>0)] #drop any blank entries
  }) 


#if you want to change åæø etc 
#iconv(x, from="UTF-8", to = "latin1")
#iconv(x, from="latin1", to = "ASCII/TRANSLIT")
#or just save file with UTF-8 encoding

### Join to flickrshp and save ----
flickrshp_tags <- flickrshp[, c("id", "owner", "datetkn", "title", "tags", "url_m", "month", "year", "yearmon", "phot_lt", "region", "InCity")]
flickrshp_tags$flickr_tags <- taglist
flickrshp_tags$title_tags <- titlelist
  save(flickrshp_tags,file="input/Flickr_Artic_60N_plus_flickr_labels_urban.Rdata")

#save in long format - each tag is a row, with photo information duplicated on each row
flickrshp_tags_ft <- flickrshp_tags %>% data.frame() %>% unnest(flickr_tags)
  write.csv(flickrshp_tags_ft, "output/Flickr_Artic_60N_plus_flickr_labels_tags_long.csv", fileEncoding = "UTF-8")
flickrshp_tags_tt <- flickrshp_tags %>% data.frame() %>% unnest(title_tags)
  write.csv(flickrshp_tags_tt, "output/Flickr_Artic_60N_plus_flickr_labels_titles_long.csv", fileEncoding = "UTF-8")

### Calculate stats on data ----

#Number of unique tag words
length(unique(unlist(taglist)))

#Number of unique title words
length(unique(unlist(titlelist)))

#frequency table of tags
freq_tags <- plyr::count(unlist(taglist))
  write.csv(freq_tags, "output/frequency_of_flickr_tag_words.csv", fileEncoding="UTF-8")

#frequency table of titles
freq_titles <- plyr::count(unlist(titlelist))
  write.csv(freq_titles, "output/frequency_of_flickr_title_words.csv", fileEncoding="UTF-8")



