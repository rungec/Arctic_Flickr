# Identifying (statistically) spatial hotspots in Arctic visitation from Flickr data
# For a good explanation of this tool see   https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-how-hot-spot-analysis-getis-ord-gi-spatial-stati.htm 
# Local Moran's I: the value of the feature being analyzed is NOT included in that analysis...only the neighboring values are.  Alternatively, when the local analysis is being done with Getis-Ord Gi*, the value of each feature is included in its own analysis.  In other words, the local mean for Moran's I includes only neighboring features, whereas the local mean for Getis-Ord Gi* includes all features, including the one in question.  
# to determine a distance for the neighbourhood, you can calculate the global morans for different distances, and see there the z-score peaks (incremental spatial autocorrelation). Z-score peaks reflect distances where the spatial processes promoting clustering are most pronounced https://resources.arcgis.com/en/help/main/10.1/index.html#//005p0000004z000000
#help on interpretation of morans local https://gis.stackexchange.com/questions/90691/r-raster-package-morans-i-interpretation
#getis http://resources.esri.com/help/9.3/ArcGISEngine/java/Gp_ToolRef/Spatial_Statistics_tools/how_hot_spot_analysis_colon_getis_ord_gi_star_spatial_statistics_works.htm
#this script follows on from Flickr_density_mapping.r

library(raster) #for raster and local morans i
#library(spdep) # for Getis-Ord Gi* statistic
library(ggplot2)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/density_mapping/"
setwd(wd)


#Set up function 
hotspotfun <- function(currfile, scale, hoodim, rmzeros){
	#load raster
  currRast <- raster(currfile)
  nhood <- focalWeight(currRast, hoodim, "circle") #set a hoodim km radius circular neighbourhoud
  nhood[ceiling(length(nhood)/2)] <- 0 #set 0 in centre
  #remove zeros before analysing
	if(rmzeros==TRUE){ 
	  currRast[currRast==0] <- NA
	}
	#run morans        
	if(scale=="global"){
	  vals <- Moran(currRast, w=nhood) 
  } else if(scale=="local"){
    vals <- MoranLocal(currRast, w=nhood)
  }
	return(vals)
}

#########################
# What is the best distance for neighbourhood radius ----
#########################
#set up function

thresholdfun <- function(currfile, rastres, fileapp, rmzeros){
  radii <- ncells*rastres
  global_morans <- lapply(radii, function(currad){
    hotspotfun(currfile=currfile, scale="global", hoodim=currad, rmzeros=rmzeros)
  })
  global_morans <- unlist(global_morans)
  moranDF <- data.frame(radii, global_morans)
  write.csv(moranDF, sprintf("hotspots/Hotspot_global_morans_by_neighbourhood_radius_%s_%s.csv", fileapp, rastres))
  ggplot(moranDF, aes(x=radii/1000, y=global_morans)) +
    geom_line() +
    xlab("Neighbourhood radius (km)") + ylab("Global Morans") +
    theme_minimal(16)
  ggsave(sprintf("hotspots/Hotspot_threshold_distances_%sm_%s.png", rastres, fileapp), width=10.5, height=3.17, units=c("in"))
}

#with zeros included  
#thresholdfun("static_rasters_puds/Flickr_allseasons_per1kmcell.tif", rastres=1000, "withzeros", rmzeros=FALSE)
#replace zeros with NAs
ncells <- c(1,3,5,7,9,11, 15, 21)
thresholdfun("static_rasters_puds/Flickr_allseasons_per5kmcell.tif", rastres=5000, "nozeros_pud", rmzeros=TRUE)

#replace zeros with NAs
ncells <- c(1,3,5,7,9,11, 15, 21, 31, 41)
thresholdfun("static_rasters_puds/Flickr_allseasons_per1kmcell.tif", rastres=1000, "nozeros_pud", rmzeros=TRUE)

ncells <- c(1,3,5,7,9, 11)
#250m, replace zeros with NAs
thresholdfun("static_rasters_puds/Flickr_allseasons_per250mcell.tif", rastres=250, "nozeros_pud", rmzeros=TRUE)


#########################
# Where are the pud hotspots ----
#########################
  pudRast <- raster("static_rasters_pud/Flickr_allseasons_per250mcell.tif")
  pud_hotspots <- hotspotfun(currfile=pudRast, 
                               scale="local", hoodim=9*250, rmzeros=TRUE)
  writeRaster(pud_hotspots, "hotspots/Flickr_hotspots_pud_morans_nozeros_250m.tif")
  pud_hotspots2 <- hotspotfun(currfile=pudRast, 
                               scale="local", hoodim=9*250, rmzeros=FALSE)
  writeRaster(pud_hotspots2, "hotspots/Flickr_hotspots_pud_morans_withzeros_250m.tif")

#########################
# Where are the photo hotspots ----
#########################
  photoRast <- raster("static_rasters_nphotos/Flickr_allseasons_per250mcell.tif")
  photo_hotspots <- hotspotfun(currfile=photoRast, 
                               scale="local", hoodim=9*250, rmzeros=TRUE)
  writeRaster(photo_hotspots, "hotspots/Flickr_hotspots_photos_morans_nozeros_250m.tif")

#########################  
# Where are the owner hotspots ----
#########################
  ownerRast <- raster("static_rasters_nowners/Flickr_allseasons_per250mcell.tif")
  owner_hotspots <- hotspotfun(currfile=ownerRast, 
                               scale="local", hoodim=9*250, rmzeros=TRUE)
  writeRaster(photo_hotspots, "hotspots/Flickr_hotspots_owners_morans_nozeros_250m.tif")

#########################  
# Where do people take more photos than expected from visitor numbers (normalise nphotos by nowners)
#########################
  norm_photos <- photoRast/ownerRast
  norm_hotspots <- hotspotfun(currfile=norm_photos, 
                               scale="local", hoodim=9*250, rmzeros=TRUE)
  writeRaster(norm_hotspots, "hotspots/Flickr_hotspots_photosnormalisedbyowner_morans_250m.tif")
  
# Where do people visit more in summer than in winter? (Are some hotspots seasonal)
# https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/space-time-analysis.htm 
  
  