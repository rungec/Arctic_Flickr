### Set up libraries ----
require(sf)
require(tidyverse)

options(stringsAsFactors = FALSE)

#wd <- "C:/Users/cru016/Documents/connect/Paper_3_flickr/analysis"
wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_flickr/Analysis/tag_analysis/googlevision"
setwd(wd)

#flickramap
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap.Rdata")

#define regions
flickramap$region <- flickramap$Country
#might also choose NAME_0 which splits countries into land/EEZ

########################
#Preliminary processing ----
#set up functions

#set up wordcloud plotfun
wordplotfun <- function(words, freq, outname, ...){
  png(filename = sprintf("googlevision/freq_googlelabels/Google_labels_wordcloud_%s.png", outname), width=14, height=14, units="in", type='windows', antialias = "cleartype", res=600)
  wordcloud(words=words, freq=freq, random.order=FALSE, scale=c(10,1.5), family="Times New Roman", ...)
  dev.off()
}

#function to find words associated with a keyword
findfun <- function(data, word, nwords) {
  gwcols <- grep("googletag", names(data), value=TRUE)
  data_gw <- data %>% filter_at(gwcols, any_vars(. %in% word)) %>% select(gwcols) #pull the 'word' row
  a <- plyr::count(unlist(data_gw))
  assocwords <- a %>% top_n(nwords, freq) %>%  arrange(desc(freq)) #pick the top nwords most frequent words
  return(assocwords)
} 


#############
#set up data
codetbl <- flickramap[, c("id", "owner", grep("escode", names(flickramap), value=TRUE))]  %>% st_set_geometry(NULL)
#gather, 20 rows per photo
codetbl_long <- gather(codetbl, escode1, escode, grep("escode", names(codetbl)))
#drop extra col
codetbl_long <- codetbl_long[, !(names(codetbl_long)=="escode1")]

codetbl_long$esgroup <- sapply(codetbl_long$escode, function(x) {
    a <- strsplit(x, "_")
    if ("harvesting" %in% a[[1]]){
      return(a[[1]][2])
    } else {
      return(a[[1]][1])
    }
  })

#Summarise percent of photos and users containing ES:
nphotos <- nrow(codetbl)
nusers <- n_distinct(codetbl$owner)
table1 <- codetbl_long %>% group_by(esgroup) %>% 
                            summarise(freq_photos=n_distinct(id),
                                      perc_photos=100*n_distinct(id)/nphotos, 
                                       freq_users=n_distinct(owner),
                                        perc_users=100*n_distinct(owner)/nusers) %>%
                            data.frame()
otherbiotic <- codetbl_long %>% filter(escode %in% c("biotic_managed", "biotic_bird", "biotic_wildlife")) %>% group_by(escode) %>% 
                       summarise(freq_photos=n_distinct(id),
                                  perc_photos=100*n_distinct(id)/nphotos, 
                                  freq_users=n_distinct(owner), 
                                  perc_users=100*n_distinct(owner)/nusers) %>% 
                      data.frame()
names(otherbiotic)[which(names(otherbiotic)=="escode")] <- "esgroup"
table1 <- rbind(table1, otherbiotic)
write.csv(table1, "Summary_stats_for_Paper3b_Table1.csv", row.names=FALSE)

#Summarise top 10 words for each es:

codetbl2 <- flickramap[, c("id", "owner", grep("googletag", names(flickramap), value=TRUE))]  %>% st_set_geometry(NULL)
#gather, 20 rows per photo
#this is slow, but I had to add the mutate as gather had issues with googletag columns having different levels
codetbl_long2 <- codetbl2 %>% mutate_at(vars(contains("googletag")), funs(as.character)) %>%
                              gather(googletagV, googletag, grep("escode", names(codetbl))) 
#drop extra col
codetbl_long2 <- codetbl_long2[, !(names(codetbl_long2)=="googletagV")]
#merge long datasets of escodes and google tags
codetbl_long3 <- data.frame(codetbl_long, googletag=codetbl_long2[, "googletag"])

#count frequency of labels in each esgroup
#(eg counts every instance of a word, even when used in a single photo e.g. "dog", "dog-like mammal", "husky" would each be counted once).
count_es <- codetbl_long3 %>% drop_na() %>%
                              group_by(esgroup, googletag) %>% 
                              summarise(freq=n_distinct(id)) %>% #how many photos is each google word used in
                              group_by(esgroup) %>% #for each esgroup,
                              top_n(10, freq) %>% #pick the top 10 most frequent words
                              arrange(esgroup, desc(freq)) #arrange by descending frequency
count_es2 <- codetbl_long3 %>% filter(escode %in% c("biotic_managed", "biotic_bird", "biotic_wildlife")) %>%
                  group_by(escode, googletag) %>% 
                  summarise(freq=n_distinct(id)) %>%
                  group_by(escode) %>% #for each esgroup,
                  top_n(10, freq) %>% #pick the top 10 most frequent words
                  arrange(escode, desc(freq))
names(count_es2)[1] <- "esgroup"
count_es <- rbind(count_es, count_es2)
count_es$oup <- with(count_es, paste0(googletag, " (", freq, ")"))
write.csv(count_es, "Summary_stats_for_Paper3b_Table1_freqgoogletags.csv", row.names=FALSE)


#count frequency of the top ranked word used per photo, from each es group 
#(eg ignores synonmys in a single photo e.g. "dog", "dog-like mammal", "husky" only "dog" would be counted). 
es_firsttag <- codetbl_long3 %>% drop_na() %>% group_by(id, esgroup) %>% 
                  top_n(1, googletag) 
count_es_firsttag <- es_firsttag %>% group_by(esgroup, googletag) %>% 
                 summarise(freq=n_distinct(id)) %>%
                 top_n(10, freq) %>%
                 arrange(esgroup, desc(freq))
es_firsttag2 <- codetbl_long3 %>% drop_na() %>% filter(escode %in% c("biotic_managed", "biotic_bird", "biotic_wildlife")) %>% 
                group_by(id, escode) %>% 
                top_n(1, googletag) 
count_es_firsttag2 <- es_firsttag2 %>% group_by(escode, googletag) %>% 
                summarise(freq=n_distinct(id)) %>%
                top_n(10, freq) %>%
                arrange(escode, desc(freq))
names(count_es_firsttag2)[1] <- "esgroup"
count_es_firsttag <- rbind(count_es_firsttag, count_es_firsttag2)
count_es_firsttag$oup <- with(count_es_firsttag, paste0(googletag, " (", freq, ")"))
write.csv(count_es_firsttag, "Summary_stats_for_Paper3b_Table1_freqgoogletags_v2.csv", row.names = FALSE)
                              
############
#Other results
#find words used with 'water'
waterwords <- findfun(codetbl2, "water", 10)
 
head(waterwords)

sum(flickramap$numtags)
