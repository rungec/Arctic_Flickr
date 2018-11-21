# This script selects a subset of photos for validation of googleVision for identifying Artic ecosystem services.
# Preceded by Flickr_tidy_flickrtags.r runs concurrently with Flickr_googlecloudvision_label.r
# photos are selected from hotspots outside developed areas

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis"
setwd(wd)

### set libraries
library(sf)
library(raster)
library(tidyverse)


### load data
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

### prelim processing
# add lulc that each photo falls within
#CAFF (2015). Land Cover Type: Arctic Land Cover Change Index. Conservation of Arctic Flora and Fauna, Arctic Biodiversity Data Service (ABDS).
# https://www.abds.is/index.php/land-cover-change-index-docman/landcovertype

  #extract the lulc codes
  #flickrshp$lulc_code <- lulc[cellFromXY(lulc, x=st_coordinates(flickrshp)[,"X"], y=st_coordinates(flickrshp)[,"Y"])]
  # match them up to the lulc description
  #flickrshp$lulc_desc <- lulc_lookup[flickrshp$lulc_code]
  # Save the full dataset with lulc added
  #save(flickrshp, file="input/Flickr_Artic_60N_plus_lulc.Rdata")

#### Make a list of photos for validation ----
# Pull out only the rows not classes as developed
#flickr_nodev <- flickrshp[!which(flickrshp$lulc_code %in% c()), ]

# Extract a subset of 1000 from each region
#set list of regions to process (drop uk islands)
flickramap$region <- unlist(lapply(1:nrow(flickramap),  function(i) {
                      x <- strsplit(flickramap$region[i], " EEZ")[[1]][1]
                      return(x)
}))

unique(flickramap$region)

set.seed(42)
#extract rows for that region
flickrshp_val <- flickramap %>% filter(!region %in% c("UK", "United Kingdom", "Jan Mayen", "High Seas")) %>%
                    mutate(region = if_else(region=="Faroe Is.", "Faroe Islands", region),
                            region = if_else(region=="Svalbard", "Svalbard and Jan Mayen", region)) %>% 
                    group_by(region) %>%
                    sample_n(300) %>%
                    ungroup()

#Save the validation subset
write.csv(flickrshp_val, "validation/Flickr_Artic_60N_validationdata.csv", fileEncoding = "UTF-8")


#### Make a list of the common keywords ----

wd <- "D:/temp/gvisontemp"
setwd(wd)
filelist <- list.files(wd, ".Rdata")

allgoogle <- lapply(filelist, function(currfile){
  desclist <- c()
  google2 <- get(load(currfile))
  for(x in 1:length(google2)){ 
    desc <- try(google2[[x]][[1]]$description) #some photos have no url, so threw google errors, hence try().
    desclist <- append(desclist, desc)
  }
  return(desclist)  
})

uniquewords <- allgoogle %>% unlist() %>% unique() %>% length()
wordfreq <- allgoogle %>% unlist() %>% table() 
write.csv(wordfreq, "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/validation/Keywords_for_validation.csv")
3000*59

