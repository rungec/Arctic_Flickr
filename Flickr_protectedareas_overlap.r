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
flickramap$region <- flickramap$NAME_0
#might also choose NAME_0 which splits countries into land/EEZ

#load shapefile of protected areas
pas <- read_sf("D:/Box Sync/Arctic/Data/Ecological/WDPA_protectedplanet/WDPA_plusCAFF_PAs_Amap60N.shp")

#load number on % protected in each country
perc_inPAs <- read.csv("WDPA_CAFF_2017_PercentinPAs_byCountry.csv") 

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
                  group_by(escode, region, in_pa) %>% 
                    summarise(freq_photos=n_distinct(id),
                              perc_photos=100*n_distinct(id)/max(nphotos),
                              nphotos=max(nphotos),
                              freq_users=n_distinct(owner), 
                              perc_users=100*n_distinct(owner)/max(nusers),
                              nusers=max(nusers)) %>%  data.frame()
names(otherbiotic)[which(names(otherbiotic)=="escode")] <- "esgroup"
esoutdf <- rbind(esoverlap, otherbiotic)

alloverlap <- patbl_long 
alloverlap$region[grep("EEZ|Seas", alloverlap$region, invert=TRUE)] <- "Arctic Land"
alloverlap$region[grep("EEZ|Seas", alloverlap$region)] <- "Arctic Marine"
alloverlapdf <- alloverlap %>% group_by(esgroup, region) %>% 
  mutate(nphotos=n_distinct(id), nusers=n_distinct(owner)) %>% 
  group_by(esgroup, region, in_pa) %>% 
  summarise(freq_photos=n_distinct(id),
            perc_photos=100*n_distinct(id)/max(nphotos),
            nphotos=max(nphotos),
            freq_users=n_distinct(owner), 
            perc_users=100*n_distinct(owner)/max(nusers),
            nusers=max(nusers)) %>%  data.frame()

esoutdf<- rbind(esoutdf, alloverlapdf)

write.csv(esoutdf, "Summary_stats_for_Paper3b_PAoverlap_byesgroup.csv", row.names=FALSE)



###################
###Make plots
###################
esoutdf <- read.csv("Summary_stats_for_Paper3b_PAoverlap_byesgroup.csv", header=TRUE)

#set up data
paphotos <- subset(esoutdf, esoutdf$in_pa==TRUE & 
                     esoutdf$esgroup %in% c("abiotic", "biotic") & 
                     esoutdf$region!="High Seas") #only a few points, dropped to make graph more succinct
paphotos$region[paphotos$region=="Svalbard and Jan Mayen"] <- "Svalbard"
paphotos_noeez <- paphotos[grep("EEZ", paphotos$region, invert=TRUE), ]
paphotos_noeez[, c("region", "esgroup")] <- lapply(paphotos_noeez[, c("region", "esgroup")], factor)
#add numbers on % area in PAs in each country
paphotos_noeez <- merge(paphotos_noeez, perc_inPAs, by.x='region', by.y = 'Country', all.x=TRUE)

#plot percent of photos inside protected areas
p1 <- ggplot(paphotos_noeez, aes(x=region, y=perc_photos, fill=esgroup)) +
  coord_flip() + scale_x_discrete(limits=rev(levels(paphotos_noeez$region))) +
  geom_bar(stat='identity', position = 'dodge') +
  geom_errorbar(aes(ymin=Percent_inPAs, ymax=Percent_inPAs), color='grey40', lty='solid', lwd=0.75) +
  ylab("Photos inside PAs (%)") +
  theme_minimal(20) +
  theme(legend.position="bottom", panel.grid.major.y=element_blank(), 
        #axis.text.x=element_text(angle=90,hjust=1), 
        axis.title.y=element_blank(), axis.text.y=element_blank()) +
  #scale_fill_manual(values = c("grey70", viridisLite::viridis(5)[3], viridisLite::magma(5)[2]),
  #scale_fill_manual(values = c("grey80", wesanderson::wes_palette(5, name="Zissou1", type="discrete")[c(1,3)]),
  scale_fill_manual(values = c("grey80","#00BFC4"),
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30/length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))  
    
ggsave(filename="Perc_photos_inPAs_byregion_esgroup_WDPACAFF.png", p1)
ggsave(filename="Perc_photos_inPAs_byregion_esgroup_WDPACAFF.pdf", p1)

#plot number of users taking photos inside protected areas
p2 <- ggplot(paphotos_noeez, aes(x=region, y=perc_users, fill=esgroup)) +
  coord_flip() + scale_x_discrete(limits=rev(levels(paphotos_noeez$region))) +
  geom_bar(stat='identity', position = 'dodge') +
  ylab("Users taking photos inside PAs (%)") +
  theme_minimal(20) +
  theme(legend.position="bottom", panel.grid.major.y=element_blank(), 
        #axis.text.x=element_text(angle=90,hjust=1), 
        axis.title.y=element_blank(), axis.text.y=element_blank()) +
  #scale_fill_manual(values = c("grey70", viridisLite::viridis(5)[3], viridisLite::magma(5)[2]),
  #scale_fill_manual(values = c("grey80", wesanderson::wes_palette(5, name="Zissou1", type="discrete")[c(1,3)]),
  scale_fill_manual(values = c("grey80","#00BFC4"),
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30/length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))
ggsave(filename="Perc_users_inPAs_byregion_esgroup_WDPACAFF.png", p2)
ggsave(filename="Perc_users_inPAs_byregion_esgroup_WDPACAFF.pdf", p2)

###########
photosbyregion <- subset(esoutdf, esoutdf$esgroup %in% c("abiotic", "biotic") & 
                           esoutdf$region!="High Seas") #only a few points, dropped to make graph more succinct
photosbyregion$region[photosbyregion$region=="Svalbard and Jan Mayen"] <- "Svalbard"
photosbyregion$region[photosbyregion$region=="Arctic"] <- "Arctic Land"
photosbyregion <- photosbyregion[grep("EEZ", photosbyregion$region, invert=TRUE), ]
photosbyregion <- photosbyregion %>% group_by(region, esgroup) %>% summarise(nphotos=first(nphotos))
photosbyregion[, c("region", "esgroup")] <- lapply(photosbyregion[, c("region", "esgroup")], factor)

#plot number of photos in each region, by esgroup
p3 <- ggplot(photosbyregion, aes(x=region, y=nphotos/100000, group=esgroup, fill=esgroup)) +
  coord_flip() + scale_x_discrete(limits=rev(levels(paphotos_noeez$region))) +
  geom_bar(stat='identity', position = 'dodge') +
  ylab(expression(Number~of~photos~(x~10^{5}))) +
  theme_minimal(20) +
  theme(legend.position="bottom", panel.grid.major.y=element_blank(),  
        #axis.text.x=element_text(angle=90,hjust=1), 
        axis.title.y=element_blank()) +
  #scale_fill_manual(values = c("grey70", viridisLite::viridis(5)[3], viridisLite::magma(5)[2]),
  #scale_fill_manual(values = c("grey80", wesanderson::wes_palette(5, name="Zissou1", type="discrete")[c(1,3)]),
  scale_fill_manual(values = c("grey80","#00BFC4"), 
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30/length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom")) 
ggsave(filename="Num_photos_byregion_esgroup.png", p3)
ggsave(filename="Num_photos_byregion_esgroup.pdf", p3)


################
#COMBINE ALL THREE PLOTS
require(ggpubr)

pout <- ggarrange(p3, p2, p1, labels=c("(a)", "(b)", "(c)"), ncol=3, nrow=1, common.legend = TRUE, legend="bottom", hjust=0, font.label = list(size = 18, face='plain'))
ggexport(pout, filename = "Figure2_paper3b_graphs_ofPAs_vertical_erl.png", width=1280, height=480)
ggexport(pout, filename = "Figure2_paper3b_graphs_ofPAs_vertical_erl.pdf", width=16, height=7)
ggexport(pout, filename = "Figure2_paper3b_graphs_ofPAs_vertical_erl.eps", width=16, height=7)
