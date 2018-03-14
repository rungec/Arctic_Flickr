# this script tmakes word cloud plots
# follows on from Flickr_tidy_flickrtags.r & Flickr_googlecloudvison_labels.r


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis"
setwd(wd)

### Setup ----
library(wordcloud)

options(stringsAsFactors = FALSE)
#options(tibble.width = Inf) #print all columns



### Make word clouds ----
#load flickrshp_tag
#(file="input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
tagfreq <- read.csv("output/frequency_of_flickr_tag_words.csv", encoding="UTF-8", header=TRUE)
titlefreq <- read.csv("output/frequency_of_flickr_title_words.csv", encoding="UTF-8", header=TRUE)

wordcloud(words=tagfreq$x, freq=tagfreq$freq, max.words=200, random.order=FALSE, scale=c(3,0.5))
wordcloud(words=tagfreq$x, freq=tagfreq$freq, min.freq=3000, random.order=FALSE)
wordcloud(words=tagfreq$x, freq=tagfreq$freq, min.freq=2000, scale=c(3,0.5), random.order=FALSE)

wordcloud(words=titlefreq$x, freq=titlefreq$freq, max.words=200, random.order=FALSE, scale=c(3,0.5))
wordcloud(words=titlefreq$x, freq=titlefreq$freq, min.freq=3000, random.order=FALSE)
wordcloud(words=titlefreq$x, freq=titlefreq$freq, min.freq=2000, scale=c(3,0.5), random.order=FALSE)


