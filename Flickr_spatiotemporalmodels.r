# Examining spatiotemporal trends in Flickr data
# Key reference: Helwig NE, Gao Y, Wang S, Ma P. 2015. Analyzing spatiotemporal trends in social media data via smoothing spline analysis of variance. Spatial Statistics 14:491–504.
# bigsplines was written by Nathaniel E. Helwig <helwig@umn.edu>

#this script follows on from Flickr_density_mapping.r

library(raster)
library(bigsplines)

wd <- wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/density_mapping/"
setwd(wd)




inpDir="annual_rasters_nphotos"
res="5km"
function(inpDir, res){
	filelist <- list.files(paste(wd, inpDir, sep="/"), sprintf("%s.tif$", res), full.names=TRUE)
	rastStack <- stack(filelist)
	rastStack <- log(rastStack)
	
}