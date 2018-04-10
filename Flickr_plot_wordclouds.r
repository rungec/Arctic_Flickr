# this script tmakes word cloud plots
# follows on from Flickr_tidy_flickrtags.r & Flickr_googlecloudvison_labels.r


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

### Setup ----
library(wordcloud)
library(extrafont)
library(readxl)
#font_import()
#loadfonts(device="win")

options(stringsAsFactors = FALSE)
#options(tibble.width = Inf) #print all columns

nphotos = 2144572  

##############################
### Make word clouds ----
#load flickrshp_tag
#(file="input/Flickr_Artic_60N_plus_flickr_labels.Rdata")
tagfreq <- read.csv("tag_analysis/output/frequency_of_flickr_tag_words.csv", encoding="UTF-8", header=TRUE)
titlefreq <- read.csv("tag_analysis/output/frequency_of_flickr_title_words.csv", encoding="UTF-8", header=TRUE)

#set up plotfun
wordplotfun <- function(dat, outname, ...){
    png(filename = sprintf("figures/Wordcloud_%s.png", outname), width=960, height=960, type='windows', antialias = "cleartype")
    wordcloud(words=dat$x, freq=dat$freq, random.order=FALSE, scale=c(10,1.5), family="Times New Roman", ...)
    dev.off()
} 


#Plot tag wordclouds
wordplotfun(tagfreq, "flickrtags_top200", max.words=200)
wordplotfun(tagfreq, "flickrtags_usedin1perc_ofphotos", min.freq=0.01*nphotos)
wordplotfun(tagfreq, "flickrtags_usedin2perc_ofphotos", min.freq=0.02*nphotos)

#Plot title wordclouds
wordplotfun(titlefreq, "flickrtitlewords_top200", max.words=200)
wordplotfun(titlefreq, "flickrtitlewords_usedin1perc_ofphotos", min.freq=0.01*nphotos)
wordplotfun(titlefreq, "flickrtitlewords_usedin2perc_ofphotos", min.freq=0.02*nphotos)


##############################
### Summarise frequencies ----
#bins=c(0,1,10,100, 1000, 2000, 3000, max(titlefreq$freq))
#hist(titlefreq$freq, breaks=bins, xlab="Tag frequency", ylab="Frequency", freq=TRUE, xlim=c(0,4000))
#write a fun to print summary to text file
summaryfun <- function(dat, outfile){
  sink(sprintf("tables/Summary_of_%s_frequency.txt", outfile))
  cat(paste0("For ", nphotos, " photos, there were ", nrow(dat), " unique ", outfile, "s", "\n"))
   cat(paste0(nrow(dat[dat$freq==1,]), " tags used only once", "\n"))
   cat(paste0(nrow(dat[dat$freq>=100,]), " tags used at least 100 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=500,]), " tags used at least 500 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=1000,]), " tags used at least 1000 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=2000,]), " tags used at least 2000 times", "\n"))
   cat(paste0(nrow(dat[dat$freq>=3000,]), " tags used at least 3000 times", "\n"))
   cat(paste0(dat$x[which(dat$freq==max(dat$freq))], " most used word used ", max(dat$freq), " times", "\n", "\n"))
   top1perc <- dat[which(dat$freq>={0.01*nphotos}),]
   cat(paste0(nrow(top1perc), " words used to tag at least 1% of photos", "\n"))
   cat("words are: ", "\n", top1perc$x[rev(order(top1perc$freq))], "\n", "\n")
   top2perc <- dat[which(dat$freq>={0.02*nphotos}),]
   cat(paste0(nrow(top2perc), " words used to tag at least 2% of photos", "\n"))
   cat("words are: ", "\n", top2perc$x[rev(order(top2perc$freq))], "\n", "\n")
    ordereddat <- dat[rev(order(dat$freq)),]
   cat(paste0("The top 50 of the most frequently used words are:", "\n"))
   cat(ordereddat$x[1:50], "\n", "\n")

 sink()   
}

summaryfun(tagfreq, "flickrtag")
summaryfun(titlefreq, "flickrtitle")


#### Make wordclouds of tidied tags ----
# manually tidied title and tag words, removing disjointed letters (e.g. p), stopwords (last), and photo info (geotag, instagram, canon etc)
# then I manually assigned words as location words (eg finland, iceland) and summed all the similar words (iceland, islande, ijsland, icelandic)
# saved as .xlsx

top200 <- read_excel("tag_analysis/output/Flickr_tag_and_titlewords_tidied_2001to2017_photoswithurls.xlsx", sheet="Top200words")
top50local <- read_excel("tag_analysis/output/Flickr_tag_and_titlewords_tidied_2001to2017_photoswithurls.xlsx", sheet="Top50locations_noduplicates")

#set up plotfun
wordplotfun <- function(words, freq, outname, ...){
  png(filename = sprintf("figures/Wordcloud_%s.png", outname), width=14, height=14, units="in", type='windows', antialias = "cleartype", res=600)
  wordcloud(words=words, freq=freq, random.order=FALSE, scale=c(10,1.5), family="Times New Roman", ...)
  dev.off()  
} 

#plot wordclouds

wordplotfun(top200$Flickr_tag_words, top200$Freq_tag, "flickrtags_top100_tidied_2001to2017_photoswithurls", max.words=100, rot.per=0)
wordplotfun(top200$Flickr_title_words, top200$Freq_title, "flickrtitles_top100_tidied_2001to2017_photoswithurls", max.words=100, rot.per=0)

wordplotfun(top50local$Flickr_tags[1:40], top50local$Freq_tags[1:40], "flickrtags_top40locations_tidied_2001to2017_photoswithurls", max.words=40, rot.per=0)
wordplotfun(top50local$Flickr_titles[1:40], top50local$Freq_titles[1:40], "flickrtitles_top40locations_tidied_2001to2017_photoswithurls", max.words=40, rot.per=0)


