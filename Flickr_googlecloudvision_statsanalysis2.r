#this script performs descriptive and statistic analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r and Flickr_googlecloudvision_postprocessing.r

### Set up libraries ----
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)

options(stringsAsFactors = FALSE)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis/output"
setwd(wd)

########################
#Preliminary processing

#load data
load("Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_urban.Rdata")
#flickrshp - flickr data for each photo plus user classifications, what region and whether urban

#clip to AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/AMAP_updatedRussia_clipto60N.shp")
flickramap <- st_intersection(flickrshp, amap)
save(flickramap, file="Flickr_Artic_60N_plus_flickrandgooglelabels_amap.Rdata")

#drop urban
#flickramap <- flickramap[is.na(flickramap$InCity), ]

load("Flickr_Artic_60N_plus_flickrandgooglelabels_amap.Rdata")

#drop test users
flickramap <- flickramap[flickramap$usertype!="testuser", ]

#drop rows with no google words (this could be due to no url, or the user having taken down the photo or made it private)
flickramap <- flickramap[ rowSums(is.na( flickramap[, grep("googletag", names(flickramap))] )) < 20, ] #drop rows with all NAs

#output remaining google vision words
gwords <- flickramap %>% st_set_geometry(NULL) %>% 
          select(c("id", "region", grep("googletag", names(flickramap), value=TRUE))) 
gwfreqa <- unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]) 
gwfreq <- plyr::count(gwfreqa)
write.csv(gwfreq, "regional_word_frequency/Frequency_of_google_labels_overscore60_Amap.csv", fileEncoding="UTF-8", row.names=FALSE)

#output google vision words for each region
for(curregion in unique(gwords$region)) {
  gwfreqa <- unlist(gwords[gwords$region==curregion, grep("googletag", names(flickramap), value=TRUE)])
  gwfreq <- plyr::count(gwfreqa)
  write.csv(gwfreq, sprintf("byregion/Frequency_of_google_labels_overscore60_Amap_%s.csv", curregion), fileEncoding="UTF-8", row.names=FALSE)
}

#I then manually classified each of the remaining words as either ecosystem feature, activity, or na
#in the corresponding .xlsx file
#I started coding the 10k words for the whole arctic, then decided to only do the words for the amap region words
#so I merged the ES codes from the arctic file with the amap file, then finished coding the amap file
#the half completed file is Frequency_of_google_labels_overscore60_Arctic_ESclasses.xlsx
#the completed file is Frequency_of_google_labels_overscore60_Amap_ESclasses.xlsx

#add ES codes to words in amap region
gwfreq <- read.csv("regional_word_frequency/Frequency_of_google_labels_overscore60_Amap.csv", fileEncoding="UTF-8", header=TRUE)
escodeDF <- read_excel("regional_word_frequency/Frequency_of_google_labels_overscore60_Amap_ESclasses.xlsx")
amapfreq <- merge(gwfreq, escodeDF, by.x="x", by.y="x", all.x=TRUE)
amapfreq <- amapfreq[, !names(amapfreq)=="freq"]

#drop words used less than 3 times
nrow(amapfreq)
amapfreq <- amapfreq[amapfreq$freq_amap>3, ]
nrow(amapfreq)

########################
#Tabulate freq of photos reprenting ES

#make table of number of photos where Escode describes one or more of the words
#replace words with Escodes
codetbl <- gwords
codetbl[,grep("googletag", names(flickramap), value=TRUE)] <- amapfreq$Escode[match(unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]), amapfreq$x)]

#change to long format
codetbl_long <- gather(codetbl, googletag, escode, grep("googletag", names(codetbl)))
names(codetbl_long)[1] <- "flickrid"

#summarise number of photos representing each escode, in each region
codefreq <- codetbl_long %>% group_by(region, escode) %>% summarise(freq_amap_escode=n_distinct(flickrid))

#add col listin nphotos in a region  
nphotoDF <- rbind(data.frame(x=c("amap"), freq=c(nrow(flickramap))), plyr::count(flickramap$region))
names(nphotoDF)[2] <- "nphotos"
codefreq <- merge(codefreq, nphotoDF, by.x="region", by.y="x", all.x=TRUE)

#convert frequencies to proportion, as prop of photos in a given region
codefreq$freq_amap_escode_prop <- with(codefreq, freq_amap_escode/nphotos)

#save
write.csv(codefreq, "regional_word_frequency/Frequency_of_ESclasses_amap_byregion_long.csv", row.names=FALSE)

#convert to wide & save (it's not so straightforward to spread on two cols with tidyr)
codefreq_wide <- codefreq %>% 
  gather(variable, value, -(region:escode)) %>%
  unite(temp, region, variable) %>%
  spread(temp, value)
write.csv(codefreq_wide, "regional_word_frequency/Frequency_of_ESclasses_amap_byregion_wide.csv", row.names=FALSE)


##########################
# Tabulate frequency of the different words, by region

# make table of the frequency of google words in each region
filelist <- list.files(path="byregion/", pattern="Frequency_of_google_labels_overscore60_Amap")
for(currfile in filelist){
  curregion <- strsplit(strsplit(currfile, "_")[[1]][7], ".csv")
  currfreq <- read.csv(paste0("byregion/", currfile), fileEncoding="UTF-8", header=TRUE)
  amapfreq <- merge(amapfreq, currfreq, by="x", all.x=TRUE)
  names(amapfreq)[which(names(amapfreq)=="freq")] <- paste0("freq_", curregion)
}

#save dataset
write.csv(amapfreq, "regional_word_frequency/Frequency_of_google_labels_overscore60_Amap_ESclasses_byregion.csv", fileEncoding="UTF-8", row.names=FALSE)


#covert to long format
amapfreq_long <- gather(amapfreq, region, freq, grep("freq_", names(amapfreq)))
amapfreq_long$region <- sapply(amapfreq_long$region, function(x) strsplit(x, "freq_")[[1]][2])
amapfreq_long <- merge(amapfreq_long, nphotoDF, by.x="region", by.y="x", all.x=TRUE)

#convert frequencies to proportion, as prop of photos in a given region
amapfreq_long$freq_prop <- with(amapfreq_long, freq/nphotos)

#save dataset
write.csv(amapfreq_long, "regional_word_frequency/Frequency_of_google_labels_overscore60_Amap_ESclasses_byregion_long.csv", fileEncoding="UTF-8", row.names=FALSE)

##########################
#make some plots
codefreq$estype <- sapply(codefreq$escode, function(x) strsplit(x, "_")[[1]][1])
codefreq$esname <- sapply(codefreq$escode, function(x) {
                          sp <- strsplit(x, "_")
                          if(length(sp[[1]])==2){
                            return(sp[[1]][2])
                          } else {
                            return(paste(sp[[1]][2], sp[[1]][3], sep=" "))
                          }
                          })
#drop NA, no and pet
codefreq_sub <- codefreq[which(!codefreq$escode %in% c("no", "pet")), ]
codefreq_sub <- codefreq_sub[which(!is.na(codefreq_sub$escode)), ]

ggplot(codefreq_sub, aes(x=escode, y=freq_amap_escode_prop*100, fill=estype) ) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("% of photos") + xlab("") +
  theme_minimal(9) +
  ylim(0, 65) +
  facet_wrap(~region, scales="free_x", ncol=2) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), axis.text.y = element_text(size=2), legend.position="bottom") 
ggsave("regional_word_frequency/Barplot_Esclasses_byregion.pdf", height=21, width=14, units="cm")  

##########################
























#function to find words associated with a keyword
findfun <- function(word, threshold_freq) {
  a <- ctbl[which(dimnames(ctbl)$Var2==word), ] #pull the 'word' row
  assocwords <- names(a[a > threshold_freq]) #drop words with freq less than threshold_freq
  return(assocwords)
} 
#Contingency table
wordL <- lapply(1:nrow(subdat), function(i) {
  currow <- unname(unlist(subdat[i, !is.na(subdat[i,])])) %>% droplevels()
  d <- expand.grid(currow, currow)
  return(d)
})

wordDF <- do.call(rbind, wordL)
write.csv(wordDF, "Googlevision_nourban_long.csv", row.names=FALSE)
ctbl <- table(wordDF)
save(ctbl, file="Googlevision_contingency_table_nourban.Rdata")
















