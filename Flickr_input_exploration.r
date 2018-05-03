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
dat_all <- read_csv("D:/Box Sync/Arctic/Data/Flickr/FlickrPhotosNorthOf60.csv")
dat_iceland <- read_csv("D:/Box Sync/Arctic/Data/Flickr/FlickrPhotosIceland.csv")

#drop iceland from original dataset
dat_sub <- dat_all[!(dat_all$longitude <=-12 
					& dat_all$longitude >=-27 
					& dat_all$latitude >=62 
					& dat_all$latitude <=68), ]

#sub in the new iceland data
dat <- rbind(dat_sub, dat_iceland)

#drop rows that are outside 60N, drop 904 rows, leaving 1782083
dat <- dat[dat$latitude >= 60, ] 
#problems(dat) #check if load issues
#spec(dat) #check column formats
#add time rows
dat$month <- format(dat$datetaken,"%m")
dat$year <- format(dat$datetaken,"%Y")
dat$yearmon <- format(dat$datetaken,"%Y%m")

#add latitude column
dat$photo_lat <- dat$latitude

#load country borders shapefile
worldmap <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/60degreesN/CountryBorders_60degreesN_lambert.shp")

#load urban areas shapefile
# Urban areas are defined as a 10km radius around any the gps coordinates of any population centre of 50 000 or more
# lat and lon of urban centers came from http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-populated-places/
urbanshp <- read_sf("D:/Box Sync/Arctic/Data/Landuse/Urban_areas/ne_10m_populated_places/ne_10m_populated_places_Arctic60N_laea_10kmbuffer_popnmorethan50k.shp")

############################
#convert to spatial points ----
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
#Identify region ----
############################
#Which points fall within which country
spatialjoin <- st_intersects(all.sf, worldmap, sparse = FALSE)
spatialjoin <- cbind(spatialjoin, apply(spatialjoin, 1, function(x) all(x==FALSE))) #add "Marine" column TRUE where points dont overlap any polygon in worldmap
dimnames(spatialjoin) <- list(NULL, c(worldmap$ADMIN[1:10], "Alaska", "Marine"))
all.sf <- cbind(all.sf, spatialjoin)

#add a column called 'region'
all.df <- st_set_geometry(all.sf, NULL) #equivalent of sp@data
all.sf$region <- apply(all.df[, 29:40],1, function(x) names(all.df[29:40][which(x)]))
all.sf$region <- as.factor(all.sf$region)


#check latitude for all regions
p <- ggplot(all.sf, aes(photo_lat)) + 
  geom_histogram() + 
  facet_grid(region ~ . , scales = 'free_y') +
  #geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr by latitude",
       subtitle="By region",
       x="Latitude",
       y="Number of photos") +
    theme_minimal(base_size=10)
ggsave("figures/Flickr_60N_histogram_latitude_byregion_facet.png", p, height = 9, width = 4)


#save all.sf
#all.sf$region <- as.character(all.sf$region)
#st_write(all.sf, dsn="D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate.shp")

############################
#Identify urban areas ----
############################
urbanshp <- urbanshp[, "NAME_EN"]
urbanshp$InCity <- 1
# drop photos from urban areas
all.sf <- st_join(all.sf, urbanshp)
st_write(all.sf, "D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate_urban", driver="ESRI Shapefile")

############################
#How many users in each region ----
############################
all.sf <- read_sf("D:/Box Sync/Arctic/Data/Flickr/Flickr_Artic_60N_byregion_laea_icelandupdate_urban.shp")
all.sf.dat <- st_set_geometry(all.sf, NULL)  
names(all.sf)
#include only photos from 2000 to 2017 inclusive, with urls
all.sf.dat <- all.sf.dat[all.sf.dat$year<2018 & all.sf.dat$year>2000,]
all.sf.dat <- all.sf.dat[!is.na(all.sf.dat$url_m),]
length(unique(all.sf.dat$owner)) 
#How many photos per user in each region
usercounts <- all.sf.dat %>% group_by(region) %>% count(owner) %>% summarise(Av_photos_per_user=mean(n), Med_photos_per_user=median(n))

#number of users per region
Num_users_per_region<- all.sf.dat %>% group_by(region, owner) %>% tally() %>% count() 

#save
userDF <- merge(usercounts, Num_users_per_region)
names(userDF)[4] <- "n_users"
write.csv(userDF, "tables/Flickr_60N_users_byregion.csv")

#number of users per region per year
Num_users_per_regionyr<- all.sf.dat %>% group_by(region, year, owner) %>% tally() %>% count() %>% as.data.frame()
Num_users_per_regionyr$year <- as.numeric(Num_users_per_regionyr$year)
write.csv(Num_users_per_regionyr, "tables/Flickr_60N_users_byregion_year.csv")

p <- ggplot(Num_users_per_regionyr, aes(x = year, y = nn, colour=as.factor(region), linetype=as.factor(region))) + 
  geom_line(size=1 ) + 
  scale_color_manual(name="Region", values=c(wes_palette(6, name="Zissou", type="continuous"), wes_palette(6, name="Zissou", type="continuous"))) +
  scale_linetype_manual(name="Region", values=c(rep("solid", 6), rep("dashed", 6)))+
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use across time",
       subtitle="By region",
       x="Year",
       y="Number of users") +
  xlim(2000, 2017) +
  theme(legend.position = c(0.12, 0.75)) +
  theme_minimal(base_size=14) #to change font add base_family="Hind" 
ggsave("figures/Flickr_60N_numberofusers_byregion_year.png", p, scale=0.5, width=18, height=14)

p <- ggplot(Num_users_per_regionyr, aes(x = year, y = nn, colour=as.factor(region), linetype=as.factor(region))) + 
  geom_line(size=1, show.legend = FALSE ) + 
  scale_color_manual(name="Region", values=c(wes_palette(6, name="Zissou", type="continuous"), wes_palette(6, name="Zissou", type="continuous"))) +
  scale_linetype_manual(name="Region", values=c(rep("solid", 12)))+
  facet_wrap(~ region, scales = 'free_y', ncol = 2) +
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use across time",
       subtitle="By region",
       x="Year",
       y="Number of users") +
  xlim(2000, 2017) +
  theme(legend.position = c(0.12, 0.75)) +
  theme_minimal(base_size=12) #to change font add base_family="Hind" 
#ggsave("figures/Flickr_60N_numberofusers_byregion_year.png", p)
ggsave("figures/Flickr_60N_numberofusers_byregion_year_facet.png", p, width=14, height=21, scale=0.4)


#How many photos does each user contribute, overall
usercounts_overall <- all.sf.dat %>% group_by(owner) %>% tally() %>% count(n)
names(usercounts_overall) <- c("num_photos_contributed", "num_users")
write.csv(usercounts_overall, "tables/Number_of_photos_contributed_by_users.csv", row.names = FALSE)

usercounts_overall <- read.csv("tables/Number_of_photos_contributed_by_users.csv", header=TRUE)

p <- ggplot(usercounts_overall, aes(x=num_photos_contributed, y=num_users)) + 
  geom_line() +
  xlim(0,1000) +
  xlab("Number of photos contributed")+ylab("Number of users")+
  theme_minimal(base_size=16)
ggsave("figures/Histogram_of_photos_per_user.png", p, scale=0.4)

p <- ggplot(usercounts_overall, aes(x=num_photos_contributed, y=num_users)) + 
  geom_line() +
  xlim(0,250) + ylim(0,1000)+
  xlab("Number of photos contributed")+ylab("Number of users")+
  theme_minimal(base_size=16)
ggsave("figures/Histogram_of_photos_per_user_zoom.png", p,  scale=0.4)

usercounts_overall$usertype <- "regular"
usercounts_overall$usertype[usercounts_overall$num_photos_contributed>=471] <- "superuser"
#usercounts_overall$usertype[usercounts_overall$num_photos_contributed>=10000] <- "more than 10000"
usercounts_overall$usertype[usercounts_overall$num_photos_contributed<=2] <- "testuser"

#make a nicer plot of the histograms
# scientific_10 <- function(x) {
  # parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
# }
vp <- viewport(width=0.35, height=0.35, x=0.95, y=0.95, just=c("right", "top"))
pmain <- ggplot(usercounts_overall, aes(x=num_photos_contributed, fill=usertype))+
		geom_histogram(bins=50) +
		scale_fill_manual(name="User type", values=c(wes_palette(3, name="GrandBudapest", type="discrete"))) +
		scale_x_log10(breaks=c(1,10,100,1000,10000), labels=c(1,10,100,1000,10000)) +
		xlab("Number of photos contributed")+ylab("Number of users")+
		theme_minimal(base_size=16) +
		theme(legend.position="bottom", plot.margin=unit(c(5.5,100,5.5,5.5), "pt"), panel.grid.minor = element_blank()) 
		#theme(legend.position="right", legend.justification = "bottom") 
		
psub <- ggplot(usercounts_overall, aes(x=num_photos_contributed)) + 
		geom_histogram(bins=50) +
		xlab("# photos")+ylab("# users") +
		#scale_x_continuous(label=scientific_10) +
		scale_x_continuous(limits=c(0, 20000)) +
		theme_bw(base_size=14) +
		theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey70"))

png("figures/Histogram_of_photos_per_user.png", width=640, height=480, units="px", type = c("windows"))
	print(pmain)
	print(psub, vp=vp)
dev.off()	
  
############################
#Timeseries by region ----
############################

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
write.csv(timeDF, "tables/Flickr_60N_numberofphotos_byregion_yearmon.csv", row.names=FALSE)

#summarise each region by year
YrRegionSummary <- timeDF %>% group_by(year) %>% summarise_at(names(timeDF[5:16]), sum, na.rm=FALSE)
write.csv(YrRegionSummary, "tables/Flickr_60N_numberofphotos_byregion_year.csv", row.names=FALSE)

#summarise each region by month
MonRegionSummary <- timeDF %>% group_by(month) %>% summarise_at(names(timeDF[5:16]), sum, na.rm=FALSE)
write.csv(MonRegionSummary, "tables/Flickr_60N_numberofphotos_byregion_month.csv", row.names=FALSE)

#shape data for ggplot, drop Aland, Faroes, UK
timeDFlong <- timeDF %>% gather(region, numphotos, 5:16) %>% filter(region %in% c("Canada", "Finland", "Greenland", "Iceland", "Norway", "Russia", "Sweden", "Alaska", "Marine"))

#plot temporal trends by region ----
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
ggsave("figures/Flickr_60N_numberofphotos_byregion.png", p)

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
   
ggsave("figures/Flickr_60N_numberofphotos_byregion_facet.png", p, height = 9, width = 4)


#plot seasonal trends by region #facetwrap ----
timeDFlong$season[timeDFlong$month %in% c(12, 1, 2)] <- "Winter"
timeDFlong$season[timeDFlong$month %in% c(3,4,5)] <- "Spring"
timeDFlong$season[timeDFlong$month %in% c(6,7,8)] <- "Summer"
timeDFlong$season[timeDFlong$month %in% c(9,10,11)] <- "Autumn"
timeDFlong$season <- factor(timeDFlong$season, levels=c("Spring", "Summer", "Autumn", "Winter"))

p <- ggplot(timeDFlong, aes(x = season, y = numphotos)) + 
  geom_col(colour=wes_palette(1, name="Zissou", type="discrete"), fill=wes_palette(1, name="Zissou", type="discrete")) + 
  facet_wrap(~region, ncol=3, scales = 'free_y') +
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use by season",
       subtitle="By region",
       x="Season",
       y="Number of photos") +
   theme_minimal(base_size=10)

ggsave("figures/Flickr_60N_numberofphotos_byregion_andseason_facet.png", p, height =7, width =8)

#plot barplot by month and region ----
p <- ggplot(timeDFlong, aes(x = month, y = numphotos)) + 
  geom_col(colour=wes_palette(1, name="Zissou", type="discrete"), fill=wes_palette(1, name="Zissou", type="discrete")) + 
  facet_wrap(~region, ncol=3, scales = 'free_y') +
  geom_hline(yintercept=0, size=0.4, color="black") +
  labs(title="Flickr use by month",
       subtitle="By region",
       x="Month",
       y="Number of photos") +
  scale_x_continuous(breaks=c(2,5,8,11), labels=month.abb[c(2,5,8,11)]) +
  theme_minimal(base_size=10)+
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))
ggsave("figures/Flickr_60N_numberofphotos_byregion_andmonth_facet.png", p, height =7, width =8)

#plot heatmap of photos by year, month and region ----
tmp1 <- group_by(timeDFlong,region) # grouping the data by type
tmp2 <- mutate(tmp1, numphotos_norm = (numphotos-mean(numphotos))/sd(numphotos)) #groupwise standardization 
p <- ggplot(tmp2 ,aes(month, year, fill=numphotos_norm)) +
  geom_tile() + 
  facet_wrap(~region, ncol=3) +
  theme_minimal(10) +
  scale_y_continuous(limits=c(2010, 2017)) +
  scale_x_continuous(breaks=c(2,5,8,11), labels=month.abb[c(2,5,8,11)])+
  scale_fill_continuous(type='viridis', labels=NULL) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1), legend.title=element_blank() )
ggsave("figures/Flickr_60N_numberofphotos_byregion_andyearmon_facet.png", p, height =7, width =8)

############################
#Timeseries for whole region ----
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
write.csv(matrix(c(a, 0), ncol=12, byrow=TRUE, dimnames=list(c(1997:2017), format(seq.Date(as.Date('2000-01-01'), by = 'month', len = 12), "%b"))), "tables/Flickr_60N_numberofphotos_yearmon_matrix.csv", row.names=TRUE)

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
ggsave("figures/Flickr_60N_numberofphotos_allregions.png", p, height=4, width=7)

############################
#Photos with tags ----
############################
tagcount <- dat %>% group_by(tags) %>% summarise(no_rows = length(tags))
nrow(tagcount) #589736 different tags
write.csv(tagcount, "tables/Flickr_60N_tags_summary.csv", row.names=TRUE)

tagcount[tagcount$no_rows==max(tagcount$no_rows),] #photos with no tags
sum(tagcount$no_rows)-max(tagcount$no_rows) #photos with tags

############################
#Density plots ----
############################

#http://ryanruthart.com/using-r-to-perform-a-spatial-join-and-aggregate-data/


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
	geom_sf(size=2, alpha=0.7, colour="orangered") +
	coord_sf(crs = st_crs(102017)) + #plot in north pole lambert
	labs(title = currtime, subtitle = "Each point is a Flickr photo" ) +
    theme(text = element_text(color = "white")
          ,plot.title = element_text(size = 18, color = "white")
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
saveHTML(FUN2(timeList=c(2004:2017), timevar="year"), img.name="Flickr_60N_byYear", imgdir="interactive/animation_images", htmlfile="interactive/Flickr_60N_byYear.html", autoplay = FALSE, loop = FALSE, verbose = FALSE, single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
graphics.off()

#run across months		 
saveHTML(FUN2(timeList=c(1:12), timevar="month"), img.name="Flickr_60N_byMonth", imgdir="interactive/animation_images", htmlfile="interactive/Flickr_60N_byMonth.html", autoplay = FALSE, loop = FALSE, verbose = FALSE, single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
graphics.off()
