#ARCTIC CONNECT Project
#Flickr data
#This script reads a .csv listing the flickr photos for a given location (1,782,987 rows), transforms to spatial data and makes plots.


#  https://www.r-bloggers.com/mapping-france-at-night-with-the-new-sf-package/
wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis"
setwd(wd)

library(readr)
library(rgdal)
library(sf)
library(zoo) #as.yearmon
library(plyr)
library(maps)
library(animation)
#library(leaflet)
#library(tidyverse)
library(devtools)
#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)

options(stringsAsFactors = FALSE)
options(tibble.width = Inf) #print all columns

#load data
dat <- read_csv("D:/Box Sync/Arctic/Data/Flickr/FlickrPhotosNorthOf60.csv")
#drop rows that are outside 60N, drop 904 rows, leaving 1782083
dat <- dat[dat$latitude >= 60, ] 
#problems(dat) #check if load issues
#spec(dat) #check column formats
#add time rows
dat$month <- format(dat$datetaken,"%m")
dat$year <- format(dat$datetaken,"%Y")
dat$yearmon <- format(dat$datetaken,"%Y%m")


#load country borders shapefile
worldmap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBorders_60degreesN_lambert.shp")


############################
#convert to spatial points
############################
# WGS84 = EPSG: 4326
#North pole azimithul lambert equal area ESRI:102017 +proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 

#test dataset
subdat <- dat[1:10000,]
simple.sf <- st_as_sf(subdat, coords=c('longitude', 'latitude'))
st_crs(simple.sf) <- 4326 #WGS84
#transform to north pole lambert
simple.sf2 <- st_transform(simple.sf, crs=102017)

#full dataset
all.sf <- st_as_sf(dat, coords=c('longitude', 'latitude'))
st_crs(all.sf) <- 4326 #WGS84

############################
#Plot
############################
#Plot test
simple.sf %>% ggplot() + geom_sf(aes(size=5)) +
	coord_sf(crs = st_crs(102017)) #plot in north pole lambert

#Plot full dataset #this takes about a day
#all.sf %>% ggplot() + geom_sf(size=1, alpha=0.7, colour="#fceccf") +
	#coord_sf(crs = st_crs(102017)) #plot in north pole lambert
#	ggsave(paste0("Flickr_60N_allpoints.png"))

############################
#Time series
############################
#How many photos each month/year
counts <- ddply(dat,.(yearmon, month,year),nrow)
countsub <- subset(counts, year <2018 & year > 1996)
#fill in missing dates
countsub <- rbind(countsub, c(199804, "04", 1998, 0))
countsub <- countsub[with(countsub, order(countsub$yearmon)),]

a<-ts(countsub$V1,start=c(1997,1),freq=12)
#print(a)
write.csv(matrix(c(a, 0), ncol=12, byrow=TRUE, dimnames=list(c(1997:2017), format(seq.Date(as.Date('2000-01-01'), by = 'month', len = 12), "%b"))), "Flickr_60N_numberofpoints_byyearmon.csv", row.names=TRUE)

png(paste0(wd, "/Timeseries_flickr_allarticphotos.png"),width=1000, height=400) 
	plot(a, type="l", lwd=2, col="red", ylab= "Number of photos",xlim=c(2004,2017),axes=F)
	axis(1,at=2004:2018,labels=2004:2018);axis(2);box()
dev.off()

#86 photos have dates after 2017
#7303 photos from before 2000

############################
#Summarise by country
############################
#Which points fall within which country
spatialjoin <- st_intersects(simple.sf2, worldmap, sparse = FALSE)
spatialjoin <- cbind(spatialjoin, apply(spatialjoin, 1, function(x) all(x==FALSE)))
dimnames(spatialjoin) <- list(NULL, c(worldmap$ADMIN[1:10], "Alaska", "Marine"))
all.sf <- cbind(all.sf, spatialjoin)

#Summarise
timeList <- lapply(c(worldmap$ADMIN[1:10], "Alaska", "Marine"), function(region) {
    sub.sf <- all.sf[match.fun('==')(all.sf[[region]], TRUE), ] #select points in region
    #if(nrow(sub.sf)>0) {
      counts <- ddply(sub.sf, .(yearmon, month, year), nrow)
      countsub <- subset(counts, year <2018 & year > 1996)
    #} else {
    #countsub <- 0
    #}
    return(countsub)
})



all.sf$month <- format(all.sf$datetaken,"%m")
all.sf$year <- format(all.sf$datetaken,"%Y")
all.sf$yearmon <- format(all.sf$datetaken,"%Y%m")



timeDF <- do.call(merge, timeList)
?or merge
func <- function(x,y){merge(x, y, by.x=names(x)[1], by.y=names(y)[1])}
lapply(list.df, func, df)

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

s <- seq(as.yearmon(as.character(199701), "%Y%m"), as.yearmon(as.character(201711), "%Y%m"), 1/12) # create yearmon sequence
as.numeric(format(s, "%Y%m")) # convert to numeric yyyymm

#fill in missing dates


#save
write.csv(matrix(c(a, 0), ncol=12, byrow=TRUE, dimnames=list(c(1997:2017), format(seq.Date(as.Date('2000-01-01'), by = 'month', len = 12), "%b"))), "Flickr_60N_numberofpoints_byyearmon.csv", row.names=TRUE)

png(paste0(wd, "/xxxx.png"),width=1000, height=400) 
library(ggplot2)
library(reshape2)

df_melt = melt(df, id.vars = 'date')
ggplot(df_melt, aes(x = date, y = value)) + 
  geom_line() + 
  facet_wrap(~ variable, scales = 'free_y', ncol = 1)
dev.off()




############################
#Animations
############################

all.sf$year <- format(all.sf$datetaken, "%Y")
all.sf$month <- format(all.sf$datetaken, "%m")

	
plotfun <- function(x, timevar){
	#set up data
	currYrSub.sf <- all.sf[all.sf$year < 2018 & all.sf$year > 2000,]
	if (timevar=="month"){
		timex <- sprintf("%02d", x)
		currYrSub.sf <- currYrSub.sf[currYrSub.sf$month==timex,]
		monthtext <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
		currtime <- monthtext[x]
	} else if (timevar=="year"){	
		timex <- as.character(x)
		currYrSub.sf <- currYrSub.sf[currYrSub.sf$year==timex,]
		currtime <- paste0("Year ", timex)
	}	
	#plot #fceccf
	currYrSub.sf %>% ggplot() + 
	geom_sf(data=worldmap, fill="NA", color="grey30", size=0.15) +
	geom_sf(size=2, alpha=0.7, colour="#fce2ba") +
	coord_sf(crs = st_crs(102017)) + #plot in north pole lambert
	labs(title = currtime, subtitle = "Each point is a Flickr photo" ) +
    theme(text = element_text(color = "#E1E1E1")
          ,plot.title = element_text(size = 18, color = "#E1E1E1")
          ,plot.subtitle = element_text(size = 10)
          ,plot.background = element_rect(fill = "#000223")
          ,panel.background = element_rect(fill = "#000223")
          ,panel.grid.major = element_line(color = "#000223")
          ,axis.text = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position='none'
          )
	#ggsave(paste0("test_", currtime, ".png"))	  
}

oopt <- animation::ani.options(interval = 1)


#plotfun(6, "month")

FUN2 <- function(timeList, timevar) {
  lapply(timeList, function(curryear) {
    print(plotfun(x=curryear, timevar=timevar))
  })
}

#run across years
saveHTML(FUN2(timeList=c(2004:2017), timevar="year"), img.name="Flickr_60N_byYear", imgdir="animation_images", htmlfile="Flickr_60N_byYear.html", autoplay = FALSE, loop = FALSE, verbose = FALSE, single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
graphics.off()

#run across months		 
saveHTML(FUN2(timeList=c(1:12), timevar="month"), img.name="Flickr_60N_byMonth", imgdir="animation_images", htmlfile="Flickr_60N_byMonth.html", autoplay = FALSE, loop = FALSE, verbose = FALSE, single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
graphics.off()
