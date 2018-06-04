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

options(stringsAsFactors = FALSE)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis/googlevision"
setwd(wd)

########################
#Preliminary processing ----

#function to find words associated with a keyword
findfun <- function(word, threshold_freq) {
  a <- ctbl[which(dimnames(ctbl)$Var2==word), ] #pull the 'word' row
  assocwords <- names(a[a > threshold_freq]) #drop words with freq less than threshold_freq
  return(assocwords)
} 

#load data
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_tidy.Rdata")
#flickrshp - flickr data for each photo plus user classifications, what region and whether urban
#drop rows with no google words (this could be due to no url, or the user having taken down the photo or made it private)
flickrshp <- flickrshp[ rowSums(is.na( flickrshp[, grep("googletag", names(flickrshp))] )) < 20, ] #drop rows with all NAs

#clip to AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/AMAP/AMAP_updatedRussia_clipto60N.shp")
flickramap <- st_intersection(flickrshp, amap)
save(flickramap, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")

#drop urban
#flickramap <- flickramap[is.na(flickramap$InCity), ]

load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")


####################################
# Main processing ----
# Output tables of the google labels by region

#output the frequency of google vision words used in the amap boundaries
gwords <- flickramap %>% st_set_geometry(NULL) %>% 
          select(c("id", "region", "usertype", "touristtype_revised", grep("googletag", names(flickramap), value=TRUE))) 
gwfreqa <- unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]) 
gwfreq <- plyr::count(gwfreqa)
write.csv(gwfreq, "regional_word_frequency/Frequency_of_google_labels_overscore60_Amap.csv", fileEncoding="UTF-8", row.names=FALSE)

#output google vision words for each region
for(curregion in unique(gwords$region)) {
  gwfreqa <- unlist(gwords[gwords$region==curregion, grep("googletag", names(flickramap), value=TRUE)])
  gwfreq <- plyr::count(gwfreqa)
  write.csv(gwfreq, sprintf("byregion/Frequency_of_google_labels_overscore60_Amap_%s.csv", curregion), fileEncoding="UTF-8", row.names=FALSE)
}

#Tabulate freq of photos reprenting each ES 

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

#make table of number of photos where Escode describes one or more of the words
#replace words with Escodes
codetbl <- gwords
codetbl[,grep("googletag", names(flickramap), value=TRUE)] <- amapfreq$Escode[match(unlist(gwords[, grep("googletag", names(flickramap), value=TRUE)]), amapfreq$x)]
names(codetbl)[grep("googletag", names(codetbl))] <- paste0("escode", 1:20)

#add to flickramap
flickramap <- merge(flickramap, codetbl[, c("id", grep("escode", names(codetbl), value=TRUE))], by.x="id", by.y="id", all.x=TRUE)
save(flickramap, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap.Rdata")

#change to long format
codetbl_long <- gather(codetbl, escode1, escode, grep("escode", names(codetbl)))
names(codetbl_long)[1] <- "flickrid"
#drop extra col
codetbl_long <- codetbl_long[, !(names(codetbl_long)=="escode1")]

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
write.csv(nuserDF, "regional_word_frequency/NumPhotos_byregion_anduser_amap.csv", row.names=FALSE)

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
write.csv(usercodefreqDF, "regional_word_frequency/NumPhotos_byescode_anduser_amap.csv", row.names=FALSE)

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
write.csv(usergroupfreqDF, "regional_word_frequency/NumPhotos_byesgroup_anduser_amap.csv", row.names=FALSE)
  
##########################
# Region profiling ----
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
ggsave("regional_word_frequency/Barplot_Esclasses_byregion.pdf", height=21, width=14, units="cm")  


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
write.csv(estbl2, "regional_word_frequency/ESclasses_contingency_table_nphotos_amap.csv", row.names=TRUE)

###########################
#Plot contingency table
require(ggplot2)
ggplot(estbl2, aes(es1, es2)) +
  geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(name = 'co-occurence', low = 'royalblue', high = 'gold', trans='log10') + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
        axis.text.x =element_text(size=1, angle=90, hjust=1), axis.text.y=element_text(size=1))
ggsave("Googlevision_ecosystemservices_contingency_table_heatmap.pdf", width=14, height=14, units=c("in"))


require(corrplot)
estbl3 <- log(estbl)
estbl3[estbl3==Inf | estbl3==-Inf] <- 0
pdf("Googlevision_ecosystemservices_contingency_table_corrplot_log.pdf", width=14, height=14)
corrplot(estbl3, method='color', type='lower', diag = FALSE, order="original", tl.cex=1, tl.col = "black", tl.srt = 45, is.corr = FALSE)
dev.off()
pdf("Googlevision_ecosystemservices_contingency_table_corrplot.pdf", width=14, height=14)
corrplot(estbl, method='color', type='lower', diag = FALSE, order="original", tl.cex=1, tl.col = "black", tl.srt = 45, is.corr = FALSE)
dev.off()




#END###########################

