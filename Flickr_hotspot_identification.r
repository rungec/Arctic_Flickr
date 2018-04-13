# Identifying spatial hotspots in Arctic visitation from Flickr data
# For a good explanation of this tool see   https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-how-hot-spot-analysis-getis-ord-gi-spatial-stati.htm 
# Local Moran's I: the value of the feature being analyzed is NOT included in that analysis...only the neighboring values are.  Alternatively, when the local analysis is being done with Getis-Ord Gi*, the value of each feature is included in its own analysis.  In other words, the local mean for Moran's I includes only neighboring features, whereas the local mean for Getis-Ord Gi* includes all features, including the one in question.  
# to determine a distance for the neighbourhood, you can calculate the global morans for different distances, and see there the z-score peaks (incremental spatial autocorrelation). Z-score peaks reflect distances where the spatial processes promoting clustering are most pronounced https://resources.arcgis.com/en/help/main/10.1/index.html#//005p0000004z000000
#help on interpretation of morans local https://gis.stackexchange.com/questions/90691/r-raster-package-morans-i-interpretation
#getis http://resources.esri.com/help/9.3/ArcGISEngine/java/Gp_ToolRef/Spatial_Statistics_tools/how_hot_spot_analysis_colon_getis_ord_gi_star_spatial_statistics_works.htm
#this script follows on from Flickr_density_mapping.r

library(raster) #for raster and local morans i
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