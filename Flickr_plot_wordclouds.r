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

### Summarise frequencies ----
#bins=c(0,1,10,100, 1000, 2000, 3000, max(titlefreq$freq))
#hist(titlefreq$freq, breaks=bins, xlab="Tag frequency", ylab="Frequency", freq=TRUE, xlim=c(0,4000))
#write a fun to print summary to text file
summaryfun <- function(dat, outfile){
  sink(sprintf("output/Summary_of_%s_frequency.txt", outfile))
  cat(paste0("For 2154209 photos, there were ", nrow(dat), " unique ", outfile, "s", "\n"))
   cat(paste0(nrow(dat[dat$freq==1,]), " tags used only once", "\n"))
   cat(paste0(nrow(dat[dat$freq>=100,]), " tags used at least 100 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=500,]), " tags used at least 500 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=1000,]), " tags used at least 1000 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=2000,]), " tags used at least 2000 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=3000,]), " tags used at least 3000 times", "\n"))
   cat(paste0(dat$x[which(dat$freq==max(dat$freq))], " most used word used ", max(dat$freq), " times", "\n", "\n"))
   top10perc <- dat[which(dat$freq>={0.1*nrow(dat)}),]
   cat(paste0(nrow(top10perc), " words used to tag at least 10% of photos", "\n"))
   cat("words are: ", "\n", top10perc$x[rev(order(top10perc$freq))], "\n", "\n")
   top5perc <- dat[which(dat$freq>={0.05*nrow(dat)}),]
   cat(paste0(nrow(top5perc), " words used to tag at least 5% of photos", "\n"))
   cat("words are: ", "\n", top5perc$x[rev(order(top5perc$freq))], "\n", "\n")
 sink()   
}

summaryfun(tagfreq, "flickrtag")
summaryfun(titlefreq, "flickrtitle")
