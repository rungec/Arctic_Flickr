#this script performs descriptive analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#I categorise the google labels by ecosystem feature or activity (escode) that they represent
#and add this to the flickrshp (saved as "Flickr_Artic_60N_googlelabels_amap_escodes.Rdata")
#summary stats on how many photos contain each escode and 
#whether different types of flickr users are more or less likely to take photos representing each escode
#follows on from Flickr_googlecloudvision_label.r and Flickr_googlecloudvision_postprocessing.r

### Set up libraries ----
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)
require(wordcloud)

options(stringsAsFactors = FALSE)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis/googlevision"
setwd(wd)

#flickramap
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")

#define regions
#flickramap$region <- flickramap$Country
flickramap$region <- flickramap$NAME_0
#might also choose NAME_0 which splits countries into land/EEZ

#drop duplicated rows - this is a weird error that happens, I think it was linked to the problem I had with st_intersects
#flickramap2 <- flickramap %>% filter(! duplicated(id))
#flickramap <- flickramap2

########################
#Preliminary processing ----
#set up functions

#set up wordcloud plotfun
wordplotfun <- function(words, freq, outname, ...){
  png(filename = sprintf("freq_googlelabels/Google_labels_wordcloud_%s.png", outname), width=14, height=14, units="in", type='windows', antialias = "cleartype", res=600)
  wordcloud(words=words, freq=freq, random.order=FALSE, scale=c(10,1.5), family="Times New Roman", ...)
  dev.off()
}

#function to find words associated with a keyword
findfun <- function(word, threshold_freq) {
  gwcols <- grep("googletag", names(gwords), value=TRUE)
  data_gw <- filter_at(gwords, gwcols, any_vars(. %in% word)) %>% select(gwcols) #pull the 'word' row
  a <- plyr::count(unlist(data_gw))
  assocwords <- a[a$freq > threshold_freq, ] %>% droplevels() #drop words with freq less than threshold_freq
  return(assocwords)
} 

####################################
# Main processing ----
####################################
########################
#Exploration of google labels ----
########################
# Output tables of the google labels by region

#output the frequency of google vision words used in the amap boundaries
gwords <- flickramap %>% st_set_geometry(NULL) %>% 
          select(c("id", "region", "usertype", "touristtype_revised", grep("googletag", names(flickramap), value=TRUE))) 
gwfreqa <- unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]) 
gwfreq <- plyr::count(gwfreqa)
write.csv(gwfreq, "freq_googlelabels/Frequency_of_google_labels_overscore60_Amap.csv", fileEncoding="UTF-8", row.names=FALSE)


#output google vision wordclouds for each region
 for(curregion in unique(gwords$region)) {
   curregname <- gsub(" ", "", curregion)
   curregname <- gsub("\\.", "", curregname)
   gwfreqa <- unlist(gwords[gwords$region==curregion, grep("googletag", names(flickramap), value=TRUE)])
   gwfreqr <- plyr::count(gwfreqa)
  #make wordclouds of common words by region
  wordplotfun(gwfreqr$x, gwfreqr$freq, outname=curregname, max.words=100, rot.per=0)
#   write.csv(gwfreqr, sprintf("byregion/Frequency_of_google_labels_overscore60_Amap_%s.csv", curregion), fileEncoding="UTF-8", row.names=FALSE)
 }

#Tabulate freq of photos reprenting each ES 

#I then manually classified each of the remaining words as either ecosystem feature, activity, or na
#in the corresponding .xlsx file
#I started coding the 10k words for the whole arctic, then decided to only do the words for the amap region words
#so I merged the ES codes from the arctic file with the amap file, then finished coding the amap file
#the half completed file is Frequency_of_google_labels_overscore60_Arctic_ESclasses.xlsx
#the completed file is Frequency_of_google_labels_overscore60_Amap_ESclasses.xlsx

########################
#Add es codes to amap ----
########################

#add ES codes to words in amap region
gwfreq <- read.csv("freq_googlelabels/Frequency_of_google_labels_overscore60_Amap.csv", fileEncoding="UTF-8", header=TRUE)
escodeDF <- read_excel("freq_escodes/Frequency_of_google_labels_overscore60_Amap_ESclasses.xlsx")
amapfreq <- merge(gwfreq, escodeDF, by.x="x", by.y="x", all.x=TRUE)
amapfreq <- amapfreq[, !names(amapfreq)=="freq"]

#drop words used less than 3 times
nrow(amapfreq)
amapfreq <- amapfreq[amapfreq$freq_amap>3, ]
nrow(amapfreq)

#make table of number of photos where Escode describes one or more of the words
#replace words with Escodes
codetbl <- gwords
codetbl[,grep("googletag", names(flickramap), value=TRUE)] <- amapfreq$Escode[match(unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]), amapfreq$x)]
names(codetbl)[grep("googletag", names(codetbl))] <- paste0("escode", 1:20)

#add to flickramap
flickramap <- merge(flickramap, codetbl[, c("id", grep("escode", names(codetbl), value=TRUE))], by.x="id", by.y="id", all.x=TRUE)

#fix the numtags column
flickramap <- flickramap %>% mutate(numtags=20-rowSums(is.na( flickramap[, grep("googletag", names(flickramap))]))) %>% select(-numtags2)

save(flickramap, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap.Rdata")

########################
#Exploration of es codes ----
########################
codetbl <- flickramap %>% st_set_geometry(NULL) %>% 
  select(c("id", "region", "usertype", "touristtype_revised", grep("escode", names(flickramap), value=TRUE))) 
#change to long format
codetbl_long <- gather(codetbl, escode1, escode, grep("escode", names(codetbl)))
names(codetbl_long)[1] <- "flickrid"
#drop extra col
codetbl_long <- codetbl_long[, !(names(codetbl_long)=="escode1")]

#summarise number of photos representing each escode
codefreq_total <- codetbl_long %>% group_by(escode) %>% summarise(freq_amap_escode=n_distinct(flickrid))
codefreq_total$nphotos <- nrow(flickramap) #add col listin nphotos  
codefreq_total$freq_amap_escode_prop <- with(codefreq_total, freq_amap_escode/nphotos) #convert frequencies to proportion, as prop of photos in a given region
write.csv(codefreq_total, "freq_escodes/Frequency_of_ESclasses_amap.csv", row.names=FALSE)


#summarise number of photos representing each escode, in each region
codefreq <- codetbl_long %>% group_by(region, escode) %>% summarise(freq_amap_escode=n_distinct(flickrid))

#add col listin nphotos in a region  
nphotoDF <- rbind(data.frame(x=c("amap"), freq=c(nrow(flickramap))), plyr::count(flickramap$region))
names(nphotoDF)[2] <- "nphotos"
codefreq <- merge(codefreq, nphotoDF, by.x="region", by.y="x", all.x=TRUE)

#convert frequencies to proportion, as prop of photos in a given region
codefreq$freq_amap_escode_prop <- with(codefreq, freq_amap_escode/nphotos)

#save
write.csv(codefreq, "freq_escodes/Frequency_of_ESclasses_amap_byregion_long.csv", row.names=FALSE)

#convert to wide & save (it's not so straightforward to spread on two cols with tidyr)
codefreq_wide <- codefreq %>% 
  gather(variable, value, -(region:escode)) %>%
  unite(temp, region, variable) %>%
  spread(temp, value)
write.csv(codefreq_wide, "freq_escodes/Frequency_of_ESclasses_amap_byregion_wide.csv", row.names=FALSE)

########################
# User profiling ----
# Tabulate freq of photos by user

#how many photos do each of the different user types take
nusers_inregion <- codetbl %>% group_by(region) %>% summarise(nusers_region=n_distinct(id))
nusers_inregion_type <- codetbl %>% group_by(region, usertype) %>% summarise(nusers_region_type=n_distinct(id))
nusers_inregion_type_tourist <- codetbl %>% group_by(region, usertype, touristtype_revised) %>% summarise(nusers_region_type_tourist=n_distinct(id))
nuserDF <- merge(nusers_inregion, nusers_inregion_type, by=c("region"), all.y=TRUE)
nuserDF <- merge(nuserDF, nusers_inregion_type_tourist, by=c("region", "usertype"), all.y=TRUE)
nuserDF$propinregion_bytype <- nuserDF$nusers_region_type/nuserDF$nusers_region
nuserDF$propinregion_tourist <- nuserDF$nusers_region_type_tourist/nuserDF$nusers_region_type
write.csv(nuserDF, "freq_escodes/NumPhotos_byregion_anduser_amap.csv", row.names=FALSE)

#Do different users use different ES in each region? 
#summarise number of photos representing each escode, in each usertype (regular/superuser)
usercodefreq <- codetbl_long %>% group_by(usertype, escode) %>% 
                      summarise(freq_amap_escode=n_distinct(flickrid)) %>%
                      spread(usertype, freq_amap_escode)
touristcodefreq <- codetbl_long %>% group_by(touristtype_revised, escode) %>% 
                      summarise(freq_amap_escode=n_distinct(flickrid)) %>%
                      spread(touristtype_revised, freq_amap_escode)
usercodefreqDF <- merge(usercodefreq, touristcodefreq, by="escode")
nphotos_byusers <- codetbl_long %>% group_by(usertype) %>% 
  summarise(nphotos=n_distinct(flickrid)) %>% data.frame()
nphotos_bytourists <- codetbl_long %>% group_by(touristtype_revised) %>% 
  summarise(nphotos=n_distinct(flickrid, na.rm=TRUE)) %>% data.frame()
usercodefreqDF$regular_prop <- usercodefreqDF$regular/nphotos_byusers[nphotos_byusers$usertype=="regular", "nphotos"]
usercodefreqDF$superuser_prop <- usercodefreqDF$superuser/nphotos_byusers[nphotos_byusers$usertype=="superuser", "nphotos"]
#usercodefreqDF$domestic_prop <- usercodefreqDF$domestic/nphotos_bytourists[nphotos_bytourists$touristtype_revised %in% "domestic", "nphotos"]
usercodefreqDF$local_prop <- usercodefreqDF$local/nphotos_bytourists[nphotos_bytourists$touristtype_revised %in% "local", "nphotos"]
usercodefreqDF$tourist_prop <- usercodefreqDF$tourist/nphotos_bytourists[nphotos_bytourists$touristtype_revised %in% "tourist", "nphotos"]
usercodefreqDF <- rbind(usercodefreqDF, c("total_nphotos", rep(c(nphotos_byusers$nphotos, nphotos_bytourists$nphotos), times=2)))  
write.csv(usercodefreqDF, "freq_escodes/NumPhotos_byescode_anduser_amap.csv", row.names=FALSE)

#group into biotic, abiotic, recreation, harvesting and count
codetbl_long$esgroup <- sapply(codetbl_long$escode, function(x) {
                              a <- strsplit(x, "_")
                              if ("harvesting" %in% a[[1]]){
                                return(a[[1]][2])
                              } else {
                                return(a[[1]][1])
                              }
                        })
  usergroupfreq <- codetbl_long %>% group_by(usertype, esgroup) %>% 
    summarise(freq_amap_esgroup=n_distinct(flickrid)) %>%
    spread(usertype, freq_amap_esgroup)
  touristgroupfreq <- codetbl_long %>% group_by(touristtype_revised, esgroup) %>% 
    summarise(freq_amap_esgroup=n_distinct(flickrid)) %>%
    spread(touristtype_revised, freq_amap_esgroup)
usergroupfreqDF <- merge(usergroupfreq, touristgroupfreq, by="esgroup")
write.csv(usergroupfreqDF, "freq_escodes/NumPhotos_byesgroup_anduser_amap.csv", row.names=FALSE)
  

##########################
# Region profiling ES ----

#how many photos do each of the different user types take of the different ES in the different regions
nphtos_inregion <- codetbl_long %>% group_by(region, escode, touristtype_revised) %>% summarise(freq_es=n_distinct(flickrid))
nphtos_inregion_total <- codetbl_long %>% group_by(region, touristtype_revised) %>% summarise(freq_total=n_distinct(flickrid))
nesDF <- merge(nphtos_inregion, nphtos_inregion_total, by=c("region", "touristtype_revised"), all.x=TRUE)
write.csv(nesDF, "freq_escodes/NumPhotos_byescode_byregion_anduser_amap.csv", row.names=FALSE)


##########################
# Region profiling google words ----
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
write.csv(amapfreq, "freq_escodes/Frequency_of_google_labels_overscore60_Amap_ESclasses_byregion.csv", fileEncoding="UTF-8", row.names=FALSE)

#covert to long format
amapfreq_long <- gather(amapfreq, region, freq, grep("freq_", names(amapfreq)))
amapfreq_long$region <- sapply(amapfreq_long$region, function(x) strsplit(x, "freq_")[[1]][2])
amapfreq_long <- merge(amapfreq_long, nphotoDF, by.x="region", by.y="x", all.x=TRUE)

#convert frequencies to proportion, as prop of photos in a given region
amapfreq_long$freq_prop <- with(amapfreq_long, freq/nphotos)

#save dataset
write.csv(amapfreq_long, "freq_escodes/Frequency_of_google_labels_overscore60_Amap_ESclasses_byregion_long.csv", fileEncoding="UTF-8", row.names=FALSE)


##########################
#make some plots ----
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
ggsave("freq_escodes/Barplot_Esclasses_byregion.pdf", height=21, width=14, units="cm")  


############################
#Make a contingency table of how often the different EScodes are associated in the one photo ----
codetbl <- flickramap[, c(grep("escode", names(flickramap), value=TRUE))]  %>% st_set_geometry(NULL)

esL <- lapply(1:nrow(codetbl), function(i) {
  currow <- unname(unlist(codetbl[i, !is.na(codetbl[i,])])) %>% unique()
  d <- expand.grid(currow, currow, stringsAsFactors = FALSE)
  d <- d[d$Var1!=d$Var2, ]
  return(d)
})

esDF <- do.call(rbind, esL)
estbl <- table(esDF)
estbl2 <- data.frame(estbl)
names(estbl2) <- c("es1", "es2", "Freq")
write.csv(estbl2, "freq_escodes/ESclasses_contingency_table_nphotos_amap.csv", row.names=TRUE)

###########################
#Plot contingency table
require(ggplot2)
ggplot(estbl2, aes(es1, es2)) +
  geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(name = 'co-occurence', low = 'royalblue', high = 'gold', trans='log10') + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
        axis.text.x =element_text(size=1, angle=90, hjust=1), axis.text.y=element_text(size=1))
ggsave("freq_escodes/Googlevision_ecosystemservices_contingency_table_heatmap.pdf", width=14, height=14, units=c("in"))


require(corrplot)
estbl3 <- log(estbl)
estbl3[estbl3==Inf | estbl3==-Inf] <- 0
pdf("freq_escodes/Googlevision_ecosystemservices_contingency_table_corrplot_log.pdf", width=14, height=14)
corrplot(estbl3, method='color', type='lower', diag = FALSE, order="original", tl.cex=1, tl.col = "black", tl.srt = 45, is.corr = FALSE)
dev.off()
pdf("freq_escodes/Googlevision_ecosystemservices_contingency_table_corrplot.pdf", width=14, height=14)
corrplot(estbl, method='color', type='lower', diag = FALSE, order="original", tl.cex=1, tl.col = "black", tl.srt = 45, is.corr = FALSE)
dev.off()




#END###########################

