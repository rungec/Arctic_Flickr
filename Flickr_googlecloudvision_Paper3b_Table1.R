### Set up libraries ----
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)

options(stringsAsFactors = FALSE)

wd <- "C:/Users/cru016/Documents/connect/Paper_3_flickr/analysis"
setwd(wd)

#flickramap
load("Flickr_Artic_60N_googlelabels_escodes_amap.Rdata")

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
findfun <- function(word, threshold_freq) {
  gwcols <- grep("googletag", names(gwords), value=TRUE)
  data_gw <- filter_at(gwords, gwcols, any_vars(. %in% word)) %>% select(gwcols) #pull the 'word' row
  a <- plyr::count(unlist(data_gw))
  assocwords <- a[a$freq > threshold_freq, ] %>% droplevels() #drop words with freq less than threshold_freq
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
write.csv(table1, "Summary_stats_for_Paper3b_Table1.csv")

#Summarise top 10 words for each es:

codetbl2 <- flickramap[, c("id", "owner", grep("googletag", names(flickramap), value=TRUE), grep("escode", names(flickramap), value=TRUE))]  %>% st_set_geometry(NULL)
#gather, 20 rows per photo
#this is slow, but I had to add the mutate as gather had issues with googletag columns having different levels
codetbl_long2 <- codetbl2 %>% mutate_at(vars(contains("googletag")), funs(as.character)) %>%
                              gather(variable, value, googletag1:escode20) 
codetbl_long2$variable <- sapply(codetbl_long2$variable, function(x) gsub(".$", "", x)) 
codetbl_long3 <- spread(codetbl_long2, key=variable, value=value)

count_es <- codetbl_long2 %>% group_by(escode, googletag) %>% 
                              summarise(freq=n_distinct(id)) %>%
                              arrange(desc(freq))
                              


