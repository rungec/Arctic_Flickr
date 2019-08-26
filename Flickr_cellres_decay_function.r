#This script plots decay functions of resolution vs footprint
#using a square grid
#to work out the best resolution rasters to use 

### Set up libraries ----
require(sf)
require(tidyverse)
require(raster)
require(rgdal)
require(ggplot2)
require(ggpubr)

wd <- "/data/Claire/rasters"
setwd(wd)


##########################
### Setup ----

load("Flickr_Artic_60N_googlelabels_userinfo_tidy_amap.Rdata")
#photos from amap region, 
#regular and super users only, 
#2004 to 2017
#dropped rows with no google vision labels
#includes photos in towns/cities

#load AMAP boundaries - I updated the Yamal borders, and clipped out any areas south of 60N. 
amap <- read_sf("Boundaries/Arctic_circle/AMAP/flickr_AMAP60N_dissolve.shp")
rcrs <- st_crs(amap)

#######################
#Function to rasterise es where the value in each cell = PUD
#PUD = photo unit days
#res is resolution of the grid cell length (NOT area) in metres
rastfun <- function(curres) {
  #set the blank raster  
  rasttemplate <- raster(xmn=-3335000, xmx=3335000, ymn=-3335000, ymx=3335000, res=curres, crs=rcrs$proj4string)
    if(file.exists(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))==FALSE){ 
      rastamap <- rasterize(amap, rasttemplate, filename=sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif",curres))
    } else {
      rastamap <- raster(sprintf("Boundaries/Arctic_circle/AMAP/AMAP_%smres.tif", curres))
    }
    ncells_studyregion <- cellStats(rastamap, stat='sum') #the number of cells in the study region
    rastamap[rastamap==1] <- 0
       #fill with pud
	if(file.exists(sprintf("density_mapping/decay_function/Flickr2017_PUDper%scell.tif", curres))==FALSE){
    subdat <- flickramap[flickramap$year==2017, ]
    currrast <- rasterize(subdat, rastamap, fun=function(x, ...){ length(unique(x))}, field="owner_date", update=TRUE, filename=sprintf("density_mapping/decay_function/Flickr2017_PUDper%scell.tif", curres), overwrite=TRUE)
  } else {
    currrast <- raster(sprintf("density_mapping/decay_function/Flickr2017_PUDper%scell.tif", curres))
  }
  currrast[currrast > 0] <- 1
	ncells_footprint <- cellStats(currrast, stat='sum')
	return(data.frame(curres, ncells_footprint, ncells_studyregion))
  }

#######################
#Apply function across range of resolutions
resolutions <- list(100000, 50000, 20000, 10000, 5000, 1000, 500)
decayfun <- lapply(resolutions, function(x) rastfun(x))
decaydf <- do.call(rbind, decayfun)
decaydf$footprint_prop <- with(decaydf, ncells_footprint/ncells_studyregion)
write.csv(decaydf, "Decay_function_Flickr2017_ncells_vs_cellres_m.csv")

#######################
#Plot
p1 <- ggplot(decaydf, aes(x=(curres*curres/1000000), y=footprint_prop*100)) +
	geom_line() +
	xlab("Cell area (square km)") +
	ylab("Footprint in 2017 (% of Arctic)") +
  coord_trans(x="log2") +
  #scale_x_reverse() +
  theme_minimal(18)+
  scale_x_continuous(breaks=c(.25, 1,25,100,400,2500,10000), labels=c(.25, 1,25,100,400,2500,10000))

p2 <- ggplot(decaydf, aes(x=curres/1000, y=footprint_prop*100)) +
  geom_line() +
  xlab("Cell size (km)") +
  ylab("Footprint in 2017 (% of Arctic)") +
  #scale_x_reverse() +
  coord_trans(x="log2") +
  theme_minimal(18)+
  scale_x_continuous(breaks=c(.5, 1,5,10,20,50,100))
  
	
pout <- ggarrange(p1, p2, labels=c("A", "B"), ncol=2, nrow=1, common.legend = TRUE, legend="right", font.label = list(size = 18, face='plain'))
ggexport(pout, filename = "Decay_function_flickr_2017data_squaregrids.png", width=1280, height=480)
ggexport(pout, filename = "Decay_function_flickr_2017data_squaregrids.pdf", width=16, height=7)


######################
#Histogram of puds at different resolutions
rlist <- list.files("density_mapping/decay_function/", full.names=TRUE)
rstack <- lapply(rlist, function(x) {
            curres <- strsplit(strsplit(x, "cell")[[1]][1], "per")[[1]][2]
            r <- raster(x)
            rhist <- hist(r, breaks=c(0, 1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 200, 300, 500, 1000, 1500, 2000), maxpixels=10000000, plot=FALSE)
            #cellStats(r, stat='max')
            return(data.frame(breaknum=c(1:(length(rhist$breaks)-1)), rhist$counts, rhist$density, curres))
})

histdf <- do.call(rbind, rstack)
histdf$curres <- factor(histdf$curres, levels=c("500", "1000", "5000", "10000", "20000", "50000", "1e+05"))
histdf$labels <- rep(c("0", "1", "2", "3", "4", "5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-100", "101-200", "201-300", "301-500", "501-1000", ">1000"), times=length(rlist))
write.csv(histdf, "Histogram_data_ArcticFlickr_byresolution.csv")

#######################
#Plot
p1 <- ggplot(histdf, aes(x=breaknum, y=rhist.counts, colour=curres)) +
  geom_line() +
  coord_trans(y="log1p") +
  xlab("\nPhoto-unit-days in cell") +
  ylab("Count of cells") +
  theme_minimal(18) +
  scale_color_discrete(name="Resolution (m)")+
  scale_x_continuous(breaks=c(1:17), labels=c("0", "1", "2", "3", "4", "5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-100", "101-200", "201-300", "301-500", "501-1000", ">1000")) +
  scale_y_continuous(breaks=c(0, 1,10,100,1000,10000,100000, 1000000, 7500000))+
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.5), 
        panel.grid.minor=element_blank())


p2 <- ggplot(histdf[histdf$breaknum!=1,], aes(x=breaknum, y=rhist.counts, colour=curres)) +
  geom_line() +
  coord_trans(y="log1p") +
  xlab("\nPhoto-unit-days in cell") +
  ylab("Count of cells") +
  theme_minimal(18) +
  scale_color_discrete(name="Resolution (m)")+
  scale_x_continuous(breaks=c(1:17), labels=c("0", "1", "2", "3", "4", "5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-100", "101-200", "201-300", "301-500", "501-1000", ">1000")) +
  scale_y_continuous(breaks=c(0, 1,5, 10,50, 100, 400))+
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.5), 
        panel.grid.minor=element_blank())

pout <- ggarrange(p1, p2, labels=c("A", "B"), ncol=2, nrow=1, common.legend = TRUE, legend="right", font.label = list(size = 18, face='plain'))
ggexport(pout, filename = "Flickr_histogram_of_pud_frequency_by_cellresolution.png", width=1280, height=480)
ggexport(pout, filename = "Flickr_histogram_of_pud_frequency_by_cellresolution.pdf", width=16, height=7)

##END####
	
	
