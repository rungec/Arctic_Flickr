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
flickrshp <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate_urban.shp")

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
#set list of regions to process (drop faroes & uk islands)
set.seed(42)
regionlist <- list(IcelandGreenland=c("Iceland", "Greenland"),
                   NorthAmerica = c("Alaska", "Canada"), 
                   Scandinavia=c("Norway", "Sweden"), 
                   Finland=c("Finland", "Aland"), 
                   Russia=c("Russia"), 
                   Marine=c("Marine"))

flickr_l <- lapply(seq_along(regionlist), function(i){
  curregion <- regionlist[i] 
  #extract rows for that region, drop unwanted cols
  flickrshp_sub <- flickrshp[flickrshp$region %in% curregion[[1]] & is.na(flickrshp$InCity), c("id", "owner", "datetkn", "title", "tags", "url_m", "month", "year", "yearmon", "phot_lt", "region")]
  flickrshp_sample <- flickrshp_sub[sample(nrow(flickrshp_sub), 1000), ]
})

flickr_val <- do.call(rbind, flickr_l)
#Save the validation subset as .shp
st_write(flickr_val, "validation/Flickr_Artic_60N_validationdata.shp")
#Save as .csv
flickr_val_df <- flickr_val[] %>% st_set_geometry(NULL) %>% data.frame() 
write.csv(flickr_val_df, "validation/Flickr_Artic_60N_validationdata.csv", fileEncoding = "UTF-8")


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

