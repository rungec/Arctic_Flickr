#### load R packages ####
# devtools::install_github("remi-daigle/flickRgeotag")
library(flickRgeotag)

api_key = 'd4a3326bfa4bdbfaba49ab6bd8b40bab'

#### set search parameters ####
bbox='-180,60,180,90'

#### download photo metadata ####
sub_bbox_Arctic <- splitbbox(api_key, bbox)
writeLines(sub_bbox_Arctic,"sub_bbox_Arctic")
sub_bbox_Arctic <- readLines("sub_bbox_Arctic")
photos <- flickr.meta.dl(api_key, sub_bbox_Arctic)
any(duplicated(photos))

write.csv(photos,'FlickrPhotosNorthOf60.csv',row.names = FALSE)

# iceland redo
#### set search parameters ####
bbox='-27,62,-12,68'
#### download photo metadata ####
sub_bbox_Iceland <- splitbbox(api_key, bbox)
writeLines(sub_bbox_Iceland,"sub_bbox_Iceland")
sub_bbox_Iceland <- readLines("sub_bbox_Iceland")
photos <- flickr.meta.dl(api_key, sub_bbox_Iceland)
any(duplicated(photos))

write.csv(photos,'FlickrPhotosIceland.csv',row.names = FALSE)
