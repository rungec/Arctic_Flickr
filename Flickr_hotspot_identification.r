# Identifying spatial hotspots in Arctic visitation from Flickr data
# For a good explanation of this tool see   https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-how-hot-spot-analysis-getis-ord-gi-spatial-stati.htm 
#this script follows on from Flickr_density_mapping.r

library(raster)
library(spdep) # for Getis-Ord Gi* statistic

wd <- wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/density_mapping/"
setwd(wd)


#remove zeros before analysing?
# do morans i in raster package instead?
# read this https://community.esri.com/thread/109190

function(currfile){
	
	currRast <- raster(currfile)

}

# Where are the photo hotspots

# Where are the owner hotspots

# Where do people take more photos than expected from visitor numbers (normalise nphotos by nowners)

# Where do people visit more in summer than in winter? (Are some hotspots seasonal)
# https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/space-time-analysis.htm 