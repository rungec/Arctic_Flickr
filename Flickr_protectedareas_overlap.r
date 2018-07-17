### Set up libraries ----
require(sf)
require(tidyverse)

options(stringsAsFactors = FALSE)

###################
###Load files
###################
#wd <- "C:/Users/cru016/Documents/connect/Paper_3_flickr/analysis"
wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_flickr/Analysis/protectedarea_overlap"
setwd(wd)

#load flickr data
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap.Rdata")
#define regions
flickramap$region <- flickramap$Country
#might also choose NAME_0 which splits countries into land/EEZ

#load shapefile of protected areas
pas <- read_sf("D:/Box Sync/Arctic/Data/Ecological/Arctic_Protected_areas_2017/CAFF_Protected_Areas_20_01_2017_nplaea.shp")

###################
###Preliminary Processing
###################
###Which points fall within a protected area
spatialjoin <- st_intersects(flickramap, pas, sparse = TRUE)
#in_pa <- spatialjoin %>% lengths > 0 #TRUE if in PA, else FALSE

region_oid <- lapply(1:length(spatialjoin), function(x){
                      data.frame(flickr_rowid=x, 
                                 pa_rowid=spatialjoin[[x]][1], 
                                 in_pa=length(spatialjoin[[x]])>0 )
              })
region_oid2 <- do.call(rbind, region_oid)


padf <- st_set_geometry(pas, NULL) #equivalent of sp@data
padf$pa_rowid <- as.integer(row.names(padf))
region_pas <- merge(region_oid2, padf[, c("pa_rowid", "OBJECTID", "NAME", "IUCNCAT", "MARINE")], by.x="pa_rowid", by.y="pa_rowid", all.x=TRUE)
names(region_pas)[4:7] <- c("PA_OBJECTID", "PA_NAME", "PA_IUCNCAT", "PA_MARINE")

#join to flickramap
flickramap <- bind_cols(flickramap, region_pas[order(region_pas$flickr_rowid), ])
#drop flickr_rowid col
flickramap <- flickramap %>% select(-flickr_rowid) 

#save
save(flickramap, file="D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

###################
###Main Processing
###################
#Summarise data

#load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

#select columns
pa_tbl <- flickramap[, c("id", "owner", "region", "in_pa", grep("escode", names(flickramap), value=TRUE))] %>% st_set_geometry(NULL) 

#gather, 20 rows per photo
patbl_long <- gather(pa_tbl, escode1, escode, grep("escode", names(pa_tbl)))
#drop extra col
patbl_long <- patbl_long[, !(names(patbl_long)=="escode1")]

#add "esgroup" column
patbl_long$esgroup <- sapply(patbl_long$escode, function(x) {
  a <- strsplit(x, "_")
  if ("harvesting" %in% a[[1]]){
    return(a[[1]][2])
  } else {
    return(a[[1]][1])
  }
})

#Summarise percent of photos and users containing ES:
esoverlap <- patbl_long %>% group_by(esgroup, region) %>% 
          mutate(nphotos=n_distinct(id), nusers=n_distinct(owner)) %>% 
            group_by(esgroup, region, in_pa) %>% 
              summarise(freq_photos=n_distinct(id),
                        perc_photos=100*n_distinct(id)/max(nphotos),
                        nphotos=max(nphotos),
                        freq_users=n_distinct(owner), 
                        perc_users=100*n_distinct(owner)/max(nusers),
                        nusers=max(nusers)) %>%  data.frame()

otherbiotic <- patbl_long %>% filter(escode %in% c("biotic_managed", "biotic_bird", "biotic_wildlife")) %>% 
                group_by(escode, region) %>% 
                mutate(nphotos=n_distinct(id), nusers=n_distinct(owner)) %>% 
                  group_by(esgroup, region, in_pa) %>% 
                    summarise(freq_photos=n_distinct(id),
                              perc_photos=100*n_distinct(id)/max(nphotos),
                              nphotos=max(nphotos),
                              freq_users=n_distinct(owner), 
                              perc_users=100*n_distinct(owner)/max(nusers),
                              nusers=max(nusers)) %>%  data.frame()
names(otherbiotic)[which(names(otherbiotic)=="escode")] <- "esgroup"
esoverlap <- rbind(esoverlap, otherbiotic)

alloverlap <- patbl_long[grep("EEZ", patbl_long$region, invert=TRUE),] %>% group_by(esgroup) %>% 
  mutate(nphotos=n_distinct(id), nusers=n_distinct(owner)) %>% 
  group_by(esgroup, in_pa) %>% 
  summarise(freq_photos=n_distinct(id),
            perc_photos=100*n_distinct(id)/max(nphotos),
            nphotos=max(nphotos),
            freq_users=n_distinct(owner), 
            perc_users=100*n_distinct(owner)/max(nusers),
            nusers=max(nusers)) %>%  data.frame()
alloverlap$region <- "Arctic"
alloverlapeez <- patbl_long[grep("EEZ", patbl_long$region),] %>% group_by(esgroup) %>% 
  mutate(nphotos=n_distinct(id), nusers=n_distinct(owner)) %>% 
  group_by(esgroup, in_pa) %>% 
  summarise(freq_photos=n_distinct(id),
            perc_photos=100*n_distinct(id)/max(nphotos),
            nphotos=max(nphotos),
            freq_users=n_distinct(owner), 
            perc_users=100*n_distinct(owner)/max(nusers),
            nusers=max(nusers)) %>%  data.frame()
alloverlap$region <- "Arctic_EEZ"
esoverlap <- rbind(esoverlap, alloverlapeez)

write.csv(esoverlap, "Summary_stats_for_Paper3b_PAoverlap_byesgroup.csv", row.names=FALSE)



###################
###Make plots
###################

paphotos <- subset(esoverlap, esoverlap$in_pa==TRUE & esoverlap$esgroup %in% c("abiotic", "biotic", "recreation"))
paphotos_noeez <- paphotos[grep("EEZ", paphotos$region, invert=TRUE), ]
paphotos_noeez$region <- as.factor(paphotos_noeez$region )
paphotos_noeez$esgroup <- as.factor(paphotos_noeez$esgroup )
paphotos_noeez <- droplevels(paphotos_noeez)


p <- ggplot(paphotos_noeez, aes(x=region, y=perc_photos, fill=esgroup)) +
  geom_bar(stat='identity', position = 'dodge') +
  ylab("Photos inside PAs (%)") +
  theme_minimal() +
  theme(legend.position="bottom", axis.text.x=element_text(angle=90,hjust=1), axis.title.x=element_blank()) +
  scale_fill_manual(values = c("grey70", rev(viridisLite::viridis(5)[2:3])),
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30/length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = F, label.position = "bottom"))  
    #coord_flip() + scale_x_discrete(limits=rev(levels(paphotos_noeez$region)))
ggsave(filename="Perc_photos_inPAs_byregion_esgroup.png"), p)



