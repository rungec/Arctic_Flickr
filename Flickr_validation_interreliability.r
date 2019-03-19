# This script estimates intercoder agreement between googleVision and manual classification of images. 
# Follows on from Flickr_validation_dataset.r

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/validation"
setwd(wd)

##################
### set libraries
require(sf)
require(tidyverse)
require(irr)

##################
#set up function to reclassify escodes into esgroup
escode_to_esgroup <- function(col) {
  sapply(col, function(x){
    a <- strsplit(x, "_")
    if ("harvesting" %in% a[[1]]){
      return(a[[1]][2])
    } else if (x %in% c("no", "pet", "biotic_wildlife", "biotic_bird", "biotic_plant")){
      return(x) 
    } else {
      return(a[[1]][1])
    }
  })}

##################
### load data on google classification
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
#drop extra columns
codetbl <- flickramap[, c("id", grep("escode", names(flickramap), value=TRUE))]  %>% st_set_geometry(NULL)

### load data on manual classification
valdf <- read.csv("Flickr_validation_data_all_filled.csv", stringsAsFactors = FALSE)
valdf[is.na(valdf)] <- 0

# drop rows where photos were deleted
table(valdf$photo_deleted) #how many rows deleted?
valdf %>% group_by(photo_deleted) %>% summarise(n=n_distinct(id)) #how many unique photos were analysed?
valdf <- valdf %>% filter(photo_deleted==0)

##################
# INTERCODER RELIABILITY between manual coders
#Set up data
valdf$coder[valdf$coder=="CR2"] <- "CR"
table(valdf$coder)

#Calculate IRR
esgroups <- c("Abiotic", "Biotic", "Wildlife", "Bird", "Plant", "Non.nature", "Pet")

Fleisstbl <- sapply(1:length(esgroups), function(x){
  cat_col = as.character(esgroups[x])
  dat <- valdf %>% select(id, coder, cat_col) %>% spread(coder, cat_col) %>%
          select(CM, CR, RD, VH) %>%
          mutate(sum = rowSums(!is.na(.))) %>% filter(sum>3) %>% select(-sum) #drop rows where <4 person coded the photo
  a <- agree(dat)
  k <- kappam.fleiss(dat)
  oup <- data.frame("esgroup_cat"=cat_col,
                    "subjects"=a$subjects,
                    "raters"=a$raters,
                    "percent_in_cat_CM"=100*table(dat[,1])[2]/nrow(dat),
                    "percent_in_cat_CR"=100*table(dat[,2])[2]/nrow(dat),
                    "percent_in_cat_RD"=100*table(dat[,3])[2]/nrow(dat),
                    "percent_in_cat_VH"=100*table(dat[,4])[2]/nrow(dat),
                    "perc_agree"=a$value,
                    "fleiss_kappa"=k$value, 
                    "z_stat"=k$statistic,
                    "p_value"=k$p.value, stringsAsFactors = FALSE)
  return(oup)
}) %>% data.frame() %>% t()

write.csv(Fleisstbl, "Intercoder_reliability_between_4_manual_coders.csv", row.names=FALSE)



#iota coefficient for the interrater agreement of multivariate observations
iotatbl <- lapply(1:length(esgroups), function(x){
  cat_col = as.character(esgroups[x])
  dat <- valdf %>% select(id, coder, cat_col) %>% spread(coder, cat_col) %>%
    select(CM, CR, RD, VH) %>%
    mutate(sum = rowSums(!is.na(.))) %>% filter(sum>3) %>% select(-sum) #drop rows where <4 person coded the photo
  return(dat)
})

sink("Intercoder_reliability_iota.txt")
print("Iota between manual coders, all esgroups")
print(iota(iotatbl))
print("Iota between manual coders, without biotic_plant")
print(iota(iotatbl[-5]))
sink()


##################
# INTERCODER RELIABILITY between manual and google vision

###Set up data
# merge manual classification with google classification
valdfGV <- left_join(valdf, codetbl, by="id")
valdfGV[is.na(valdfGV)] <- 0
#valdfGV <- valdfGV[valdf$coder!="VH", ]

#select the 20 escode columns
escols <- grep("escode", names(valdfGV), value=TRUE) #pull the columns named 'escode'

#reformat google data - rows = photos, 2 columns=escode & esgroup (value=text)
valdfGV_long <- valdfGV %>% select(id, escols) %>% gather(escolname, escode, escols) %>% 
                filter(escode!="0") %>% #drop rows with no google label
                mutate(esgroup_gv = escode_to_esgroup(escode)) %>% #reclassify escodes as esgroups that we used for manual classification (e.g. "abioitic_aurora" to "abiotic")
                select(id, esgroup_gv) #drop unneeded columns

#select relevant columns from manual data
valdfM_wide <- valdfGV %>% select(id, Non.nature, Abiotic, Biotic, Wildlife, Bird, Plant, Pet)

#reformat data - rows = photos, columns=1 column for each esgroup (value=1/NA)
#and merge
valdf_wide <- valdfGV_long %>%             
                distinct(id, esgroup_gv) %>% #remove duplicates (these happen because in valdf photos are listed multiple times (for each coder))
                mutate(value=1) %>% #add acolumn where value=1 if escode is present              
                spread(esgroup_gv, value, fill=0) %>%
                right_join(valdfM_wide, by="id")

###IF YOU WANT TO KEEP ONLY THE FIRST ROW for each photo (eg 2645 photos instead of 3944 which contains the 315 duplicated set of photos)
#valdf_wide <- valdf_wide %>% distinct(id, .keep_all=TRUE)

### Calculate IRR between each ES
esgrouplist <- data.frame(google_col=c("abiotic", "biotic", "biotic_wildlife", "biotic_bird", "biotic_plant", "no", "pet"),
                          manual_col=c("Abiotic", "Biotic", "Wildlife", "Bird", "Plant", "Non.nature", "Pet"))

IRRtbl <- sapply(1:nrow(esgrouplist), function(x){
          google_col = as.character(esgrouplist$google_col[x])
          manual_col = as.character(esgrouplist$manual_col[x])
          dat <- valdf_wide %>% select(google_col, manual_col)
          a <- agree(dat)
          k <- kappa2(dat)
          oup <- data.frame("google_cat"=names(dat)[1],
                            "manual_cat"=names(dat)[2],
                            "subjects"=a$subjects,
                            "percent_in_cat_google"=100*table(dat[,1])[2]/nrow(dat),
                            "percent_in_cat_manual"=100*table(dat[,2])[2]/nrow(dat),
                            "perc_agree"=a$value,
                            "kappa"=k$value, 
                            "z_stat"=k$statistic,
                            "p_value"=k$p.value, stringsAsFactors = FALSE)
          return(oup)
}) %>% data.frame() %>% t()

write.csv(IRRtbl, "Intercoder_reliability_google_vs_manual.csv", row.names=FALSE)

#iota coefficient for the interrater agreement of multivariate observations
iotadf <- lapply(1:nrow(esgrouplist), function(x){
  google_col = as.character(esgrouplist$google_col[x])
  manual_col = as.character(esgrouplist$manual_col[x])
  dat <- valdf_wide %>% select(google_col, manual_col)
  return(dat)
})

sink("Intercoder_reliability_iota.txt", append=TRUE)
print("Iota for google vs manual, all esgroups")
print(iota(iotadf))
print("Iota for google vs manual, without non nature")
print(iota(iotadf[-6]))
sink()

####END
