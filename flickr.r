#ARCTIC CONNECT Project
#Flickr data
#This script reads a .csv listing the flickr photos for a given location (1,782,987 rows), transforms to spatial data and makes plots.


#  https://www.r-bloggers.com/mapping-france-at-night-with-the-new-sf-package/
wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis"
setwd(wd)

#library(rgdal)
library(sf)
library(zoo) #as.yearmon
library(maps)
library(animation)
#library(leaflet)
library(devtools)
library(plyr)
#devtools::install_github("tidyverse/ggplot2")
library(tidyverse)
#library(showtext) #for other fonts 
#library(Cairo) # for embedding fonts in PDF; may not need to be loaded here
library(wesanderson) #colors

#font_add_google("Hind", "Hind") #google fonts
#font_add_google("Roboto", "Roboto") 

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
# subdat <- dat[1:10000,]
# simple.sf <- st_as_sf(subdat, coords=c('longitude', 'latitude'))
# st_crs(simple.sf) <- 4326 #WGS84
# #transform to north pole lambert
# simple.sf2 <- st_transform(simple.sf, crs=102017)

#full dataset
all.sf <- st_as_sf(dat, coords=c('longitude', 'latitude'))
st_crs(all.sf) <- 4326 #WGS84
#transform to north pole lambert
all.sf <- st_transform(all.sf, crs=102017)

############################
#Plot
############################
#Plot test
#simple.sf %>% ggplot() + geom_sf(aes(size=5)) +
	#coord_sf(crs = st_crs(102017)) #plot in north pole lambert

#Plot full dataset #this takes about a day
#all.sf %>% ggplot() + geom_sf(size=1, alpha=0.7, colour="#fceccf") +
	#coord_sf(crs = st_crs(102017)) #plot in north pole lambert
#	ggsave(paste0("Flickr_60N_allpoints.png"))

############################
#Timeseries by region
############################
#Which points fall within which country
spatialjoin <- st_intersects(all.sf, worldmap, sparse = FALSE)
spatialjoin <- cbind(spatialjoin, apply(spatialjoin, 1, function(x) all(x==FALSE))) #add "Marine" column TRUE where points dont overlap any polygon in worldmap
dimnames(spatialjoin) <- list(NULL, c(worldmap$ADMIN[1:10], "Alaska", "Marine"))
all.sf <- cbind(all.sf, spatialjoin)

#set up dummy df listing times
s <- seq(as.yearmon(as.character(199701), "%Y%m"), as.yearmon(as.character(201711), "%Y%m"), 1/12) # create yearmon sequence
yearmonDF <- data.frame(yearmon=as.numeric(format(s, "%Y%m")), year=as.numeric(format(s, "%Y")), month=as.numeric(format(s, "%m")), datelabels=s) 

#Summarise
timeList <- lapply(names(all.sf)[28:39], function(region) {
	sub.sf <- all.sf[match.fun('==')(all.sf[[region]], TRUE), ] #select points in region
    if(nrow(sub.sf)>0) {
      counts <- plyr::ddply(sub.sf, .(yearmon), nrow)
      countsub <- subset(counts, yearmon < 201801 & yearmon > 199612)
    } else {
    countsub <- data.frame(yearmon=yearmonDF$yearmon, 0)
    }
    return(countsub)
})

#merge the lists
mergeFun <- function(x,y){
              m1 <-merge(y, x, by.x=names(x)[1], by.y=names(y)[1], all.x=TRUE)[, 5] #return only counts
              return(m1)
              }
timeDF <- lapply(timeList, mergeFun, yearmonDF) #fill in missing dates
timeDF <- data.frame(yearmonDF, do.call(cbind, timeDF)) #each list item becomes a column
timeDF[is.na(timeDF)] <- 0 #replace NAs with 0s
names(timeDF) <- c(names(yearmonDF), names(all.sf)[28:39]) # add region names
timeDF$All_regions <- apply(timeDF[,5:16], 1, sum) #add column summing number of photos for whole region

#save
write.csv(timeDF, "Flickr_60N_numberofphotos_byregion_yearmon.csv", row.names=FALSE)

#summarise each region by year
YrRegionSummary <- timeDF %>% group_by(year) %>% summarise_at(names(timeDF[5:16]), sum, na.rm=FALSE)
write.csv(YrRegionSummary, "Flickr_60N_numberofphotos_byregion_year.csv", row.names=FALSE)

#summarise each region by month
MonRegionSummary <- timeDF %>% group_by(month) %>% summarise_at(names(timeDF[5:16]), sum, na.rm=FALSE)
write.csv(MonRegionSummary, "Flickr_60N_numberofphotos_byregion_month.csv", row.names=FALSE)

#shape data for ggplot, drop Aland, Faroes, UK
timeDFlong <- timeDF %>% gather(region, numphotos, 5:16) %>% filter(region %in% c("Canada", "Finland", "Greenland", "Iceland", "Norway", "Russia", "Sweden", "Alaska", "Marine"))

#plot temporal trends by region
p <- ggplot(timeDFlong, aes(x = as.Date(datelabels), y = numphotos, colour=region, linetype=region)) + 
  geom_line(size=1 ) + 
  #scale_color_manual(values=c(wes_palette(9, name="Zissou", type="continuous")))+
  scale_color_manual(name="Region", values=c(wes_palette(5, name="Zissou", type="discrete"), wes_palette(4, name="Darjeeling", type="discrete")))+
  scale_linetype_manual(name="Region", values=c(rep("solid", 5), rep("dashed", 4)))+
  #facet_wrap(~ region, scales = 'free_y', ncol = 1)
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use across time",
     subtitle="By region",
     x="Year",
     y="Number of photos") +
	 scale_x_date(date_breaks="2 years", date_labels="%Y", limits=c(as.Date(as.yearmon("200401", format="%Y%m")), as.Date(as.yearmon("201801", format="%Y%m")))) +
  theme(legend.position = c(0.12, 0.75)) +
theme_minimal(base_size=9) #to change font add base_family="Hind" 
ggsave("Flickr_60N_numberofphotos_byregion.png", p)

#plot temporal trends by region #facetwrap
p <- ggplot(timeDFlong, aes(x = as.Date(datelabels), y = numphotos)) + 
  geom_line(size=1, colour=wes_palette(1, name="Zissou", type="discrete")) + 
  facet_grid(region ~ . , scales = 'free_y') +
  #geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use across time",
       subtitle="By region",
       x="Year",
       y="Number of photos") +
	   scale_x_date(date_breaks="2 years", date_labels="%Y", limits=c(as.Date(as.yearmon("200401", format="%Y%m")), as.Date(as.yearmon("201801", format="%Y%m")))) +
   theme_minimal(base_size=10)
   
ggsave("Flickr_60N_numberofphotos_byregion_facet.png", p, height = 9, width = 4)

############################
#Timeseries for whole region
############################
#Temporal trends for all regions

#How many photos each month/year, make a matrix with month as columns & year as rows
counts <- plyr::ddply(dat,.(yearmon, month,year),nrow)
countsub <- subset(counts, year <2018 & year > 1996)
#fill in missing dates
countsub <- rbind(countsub, c(199804, "04", 1998, 0))
countsub <- countsub[with(countsub, order(countsub$yearmon)),]
a<-ts(countsub$V1,start=c(1997,1),freq=12)
#print(a)
write.csv(matrix(c(a, 0), ncol=12, byrow=TRUE, dimnames=list(c(1997:2017), format(seq.Date(as.Date('2000-01-01'), by = 'month', len = 12), "%b"))), "Flickr_60N_numberofphotos_yearmon_matrix.csv", row.names=TRUE)

# png(paste0(wd, "/Flickr_60N_numberofphotos_byyearmon.png"),width=1000, height=400) 
	# plot(a, type="l", lwd=2, col="red", ylab= "Number of photos",xlim=c(2004,2017),axes=F)
	# axis(1,at=2004:2018,labels=2004:2018);axis(2);box()
# dev.off()

#86 photos have dates after 2017
#7303 photos from before 2000

#plot temporal trends by region
p <- ggplot(timeDF, aes(x = as.Date(datelabels), y = All_regions)) + 
  geom_line(size=1, colour=wes_palette(1, name="Zissou", type="discrete")) + 
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use across time",
     x="Year",
     y="Number of photos") +
	 scale_x_date(date_breaks="2 years", date_labels="%Y", limits=c(as.Date(as.yearmon("200401", format="%Y%m")), as.Date(as.yearmon("201801", format="%Y%m"))))+
theme_minimal(base_size=12) #to change font add base_family="Hind" 
ggsave("Flickr_60N_numberofphotos_allregions.png", p, height=4, width=7)

############################
#Photos with tags
############################
tagcount <- dat %>% group_by(tags) %>% summarise(no_rows = length(tags))
nrow(tagcount) #496767 different tags
write.csv(tagcount, "Flickr_60N_tags_summary.csv", row.names=TRUE)

tagcount[tagcount$no_rows==469599,] #469599 photos with no tags
sum(tagcount$no_rows)-469599 #1312484 photos with tags

############################
#Density plots
############################

http://ryanruthart.com/using-r-to-perform-a-spatial-join-and-aggregate-data/


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