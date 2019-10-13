
#########################
##LIBRARIES and FILES
#########################

require(sf)
require(tidyverse)
require(lubridate)
require(mgcv)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model_and_footprint/input/"
setwd(wd)
#wd <- paste0(getwd(), "/Documents/flickr")
#setwd(wd)
# flickr data
#load("Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
#load("Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

#load grid for footprint analysis
load("ArcticAMAP_templatehexgrid_5000_m.Rdata")

# global flickr number of photos by year
AllFlickr <- read.csv("D:/Box Sync/Arctic/Data/Flickr/global_flickr/Flickr_global_nphotostaken_byhr_2000to2018.csv") %>%   
  mutate(year=year(datetime), month=month(datetime)) %>% 
#AllFlickr <- read.csv("flickr/Flickr_global_nphotostaken_byhr_2000to2018.csv") %>%   mutate(year=year(datetime), month=month(datetime)) %>% 
  filter(year>=min(flickramap$year) & year<=2017) %>% 
  mutate(season=if_else(month %in% c(1, 2,3,4, 11, 12), "winter", "summer")) %>%
  mutate(year_new=if_else(month %in% c(11,12), year+1, year)) %>%
  mutate(yearseason = paste(year_new, season, sep="_")) %>%
  group_by(yearseason, year_new, season) %>% 
  summarize("Total Flickr Photos" = sum(total)) %>%
  separate(yearseason, c("year", "season"), sep="_", remove=FALSE)   %>%
  ungroup()
summerval <- pull(AllFlickr[AllFlickr$yearseason=="2004_summer", "Total Flickr Photos"])
winterval <- pull(AllFlickr[AllFlickr$yearseason=="2004_winter", "Total Flickr Photos"])
AllFlickr <- AllFlickr %>% 
  mutate("Global increase" = case_when(season=="summer"~.$"Total Flickr Photos"/summerval, 
                          season=="winter"~.$"Total Flickr Photos"/winterval)) %>%
  select(-c(year, season))
write.csv(AllFlickr, "Global_nphotos_by_yearseason.csv", row.name=FALSE)

#########################
##SET UP DATA
#########################
## We need to calculate the number of Flickr photos per year and correct it based on total Flickr usage over the years

#add yearseason to flickramap
flickramap$season <- "summer"
flickramap$season[flickramap$month %in% c("01", "02", "03", "04", "11", "12")] <- "winter"
flickramap$year_old <- as.numeric(flickramap$year)
flickramap <- flickramap %>% 
        mutate(year=(if_else(month %in% c("11", "12"), year_old + 1, as.numeric(year)))) %>%
        mutate(yearseason = paste(year, season, sep="_"))
flickramap <- flickramap %>% select(c(id, year, season, yearseason, month)) %>% filter(year<=2017)

#function to calculate the number of photos (not PUD) in each grid cell in each season 
photo_grid <- function(flickrrecords, time, grid){
  df <- flickrrecords %>% split(time) %>% 
    map( function(x){
      x$col.id <- as.integer(1:nrow(x)) #each col.id represents a separate photo
      grid$row.id <- as.integer(row.names(grid)) #each row.id represents a grid cell
      PUDdf <- as.data.frame(st_intersects(grid, x)) %>% 
        left_join(x, by="col.id") %>% 
        group_by(row.id) %>%
        summarize(nphotos=n()) %>% 
        right_join(grid, by="row.id") %>% 
        mutate(nphotos=if_else(is.na(nphotos), 0, as.numeric(nphotos))) %>%
        as.tibble() 
      return(PUDdf)
    }) %>% bind_rows(.id="yearseason") %>%       
    separate(yearseason, c("year", "season"), sep="_", remove=FALSE)
  return (df)
}

#function to adjust by global flickr trends
adj_fun <- function(datainp){
          byYear <- datainp %>%
                      data.frame() %>% 
                      group_by(yearseason) %>% 
                      summarise("Arctic photos"=sum(nphotos)) %>% 
                      left_join(AllFlickr,by="yearseason") %>% 
                      mutate("Percent Traffic"=.$"Arctic photos"/.$"Total Flickr Photos", 
                             "Global bias sample size" =round(.$"Arctic photos"/.$"Global increase", 0)) %>%
                      gather(key="variable",value="value",-yearseason) %>%
                      separate(yearseason, c("year", "season"), sep="_", remove=FALSE) 
                     return(byYear)
}

#how many photos in each grid cell in each year-season, for the footprint analysis
gridYear_nphoto <- photo_grid(flickramap, flickramap$yearseason, grid_footprint) 
saveRDS(gridYear_nphoto,file = paste0("gridYear_nphoto_footprint",5000,"_m.Rdata"))
#for each yearseason, how many Arctic photos, global photos, percent arctic traffic, correction factor
byYear <- adj_fun(gridYear_nphoto)
write.csv(byYear, paste0("nPhotos_byYear_andseason_",5000,"_m.csv"), row.names=FALSE)
#byYear <- read.csv(paste0("nPhotos_byYear_andseason_",5000,"_m.csv"), header=TRUE)


######################
###FOOTPRINT ANALYSIS
######################
## Now that we have all our basic data ready to go, let's vizualize!

# First, lets see if there is a change in the number of photos over time?
#plot showing all, arctic, and % traffic
plotData <- byYear %>% 
  filter(variable %in% c("Total Flickr Photos","Arctic photos","Percent Traffic")) %>%
  droplevels() %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(value = (if_else(variable=="Percent Traffic", value*100, value)))
plotData$variable <- ordered(plotData$variable, levels=c("Total Flickr Photos","Arctic photos","Percent Traffic"))
plotData$season <- factor(plotData$season, labels=c("Summer", "Winter"))

p1 <- ggplot(plotData) + 
  geom_area(aes(x=year,y=value, fill=season), position = "identity") +
  scale_fill_manual(name="", values=c("#1f78b4","#a6cee3")) +
  facet_grid(vars(variable),scales="free_y") +
  theme_bw(14) +
  scale_x_continuous(expand=c(0,0)) +
  xlab("Year") +
  theme(strip.background=element_rect(fill="white", color=NA), strip.text=element_text(size=12), 
        axis.text=element_text(size=12), 
        axis.title.x=element_text(vjust=-0.35), 
        axis.title.y=element_text(vjust=2.0), 
        legend.position="bottom", legend.text=element_text(size=14))
ggsave(filename="Flickr_footprint_vs_global.png", p1)

#How many photos per month in the Arctic?
plotData1 <- flickramap %>%
              group_by(month, season) %>%
              summarise(nphotos=n())
plotData1$season <- factor(plotData1$season, labels=c("Summer", "Winter"))

p2 <- ggplot(plotData1) +
  geom_col(aes(x=month, y=nphotos/1000, fill=season)) +
  scale_fill_manual(name="", values=c("#1f78b4","#a6cee3")) +
  xlab("Month") + ylab("Number of photos (thousand)") +
  scale_y_continuous(expand=c(0,0)) +
  theme_classic(16) +
  theme(legend.position="bottom")
ggsave(filename="Flickr_Arctic_nphotos_bymonth.png", p2, width=7, height=6)



## Both the total number of photos on Flickr and in the Arctic increase over time, but the Arctic represents an increasing share of Flickr's yearly photo traffic.
# Are these increasing numbers of tourists always visiting the same places, or are they exploring new grounds?
# spatial footprint expanded?

plotData2 <- gridYear_nphoto %>% 
  group_by(yearseason) %>% 
  summarise(footprint = mean(nphotos>0)) %>% #this calculates the proportion of cells with photos
  mutate(type="Uncorrected")

 
  # this samples n records from each year in flickramap
  # n was the number of records in the first year (2004)
  # we want an n equal to the smallest year*season combo, so min(table(flickramap$yearseason))
doup <- c()
for (i in 1:10){ #10 replicates for uncertainty analysis
    equaln_sample_flickr_s <- flickramap %>% 
    filter(season=="summer") %>%
    group_by(yearseason) %>% 
    sample_n(as.numeric(table(flickramap$yearseason)[1])) %>%
    ungroup()
  equaln_sample_flickr_w <- flickramap %>% 
    filter(season=="winter") %>%
    group_by(yearseason) %>% 
    sample_n(as.numeric(table(flickramap$yearseason)[2])) %>%
    ungroup()
  
  # this recalculates nphotos by yearseason by grid cell for equaln_sample_flickr
  plotData_equaln_s <- photo_grid(equaln_sample_flickr_s, equaln_sample_flickr_s$yearseason, grid_footprint) %>%
    group_by(yearseason) %>% 
    summarise(footprint=mean(nphotos>0) )%>% 
    mutate(type="Equal sample size")
  plotData_equaln_w <- photo_grid(equaln_sample_flickr_w, equaln_sample_flickr_w$yearseason, grid_footprint)  %>%
    group_by(yearseason) %>% 
    summarise(footprint=mean(nphotos>0) )%>% 
    mutate(type="Equal sample size")
  
  doup <- rbind(doup, data.frame(rep=i, rbind(plotData_equaln_s, plotData_equaln_w)))
}

#calculate the mean and standard dev around the equaln footprint estimate
uncert <- doup %>% group_by(yearseason, type) %>%
          summarise(footprint_mean = mean(footprint), footprint_sd = sd(footprint),
                    footprint_se = sd(footprint)/sqrt(10))

doup2 <- spread(doup, key='rep', value='footprint')
equaln <- merge(uncert, doup2, by=c('yearseason', 'type'))
write.csv(equaln, "Flickr_footprint_uncertainty.csv", row.names=FALSE)

#plot the trend with error bars
plotDataEn <- separate(equaln, yearseason, c("year", "season"), sep="_", remove=FALSE) %>%     mutate(year=as.numeric(year))
plotDataEn$season <- factor(plotDataEn$season, labels=c("Summer", "Winter"))
plotDataEn$footprint_percent <- plotDataEn$footprint_mean*100
plotDataEn$footprint_se_perc <- plotDataEn$footprint_se*100
write.csv(plotDataEn, "Flickr_footprint_equaln_withuncert_for_plots.csv", row.names=FALSE)

p4 <- ggplot(plotDataEn) +
  geom_line(aes(x=year, y=footprint_percent, col=season),position = "identity") +
  geom_errorbar(aes(x=year, ymin=footprint_percent-footprint_se_perc, ymax=footprint_percent+footprint_se_perc, col=season), width=.1) +
  xlab("Year") + ylab("Footprint (% of Arctic)") +
  scale_x_continuous(expand=c(0,0), breaks=c(2007, 2010, 2013, 2016)) +
  #scale_y_continuous(expand=c(0,0), limits=c(0,0.39)) +
  theme_bw(18) +
  theme(strip.background=element_rect(fill="white", color=NA), strip.text=element_text(size=18), 
        axis.text=element_text(size=16), 
        axis.title.x=element_text(vjust=-0.35), 
        axis.title.y=element_text(vjust=2.0), 
        legend.position="bottom", legend.text=element_text(size=18))
ggsave(filename="Flickr_footprint_equaln_withuncertainty.png", p4, width=12, height = 7)

#next lets correct for the global increase in flickr use
correct <- byYear %>% 
  filter(variable=="Global bias sample size") %>% 
  select(c(yearseason, value)) %>%
  mutate(yearseason = as.character(yearseason))

#take a sample of flickramap corrected for the global increase in flickr use
trafficn_sample_flickr <- flickramap %>% 
  nest(-yearseason)  %>% 
  left_join(correct, by="yearseason") %>% 
  #map2(data, value, sample_n)
  mutate(Sample = map2(data, value, sample_n)) %>%
  unnest(Sample) %>% 
  st_as_sf(crs = st_crs(flickramap))

table(trafficn_sample_flickr$yearseason)
table(flickramap$yearseason)

#calculate the footprint for the global increase in flickr use
plotData_trafficn <- photo_grid(trafficn_sample_flickr, trafficn_sample_flickr$yearseason, grid_footprint) %>% 
  group_by(yearseason) %>% 
  summarise(footprint=mean(nphotos>0)) %>% 
  mutate(type="Global bias-corrected")

plotDataAll <- rbind(plotData2, equaln[,1:3], plotData_trafficn)

plotDataAll$type <- ordered(plotDataAll$type,levels = unique(plotDataAll$type)[c(1,3,2)])
plotDataAll <- separate(plotDataAll, yearseason, c("year", "season"), sep="_", remove=FALSE) %>%     mutate(year=as.numeric(year))
plotDataAll$season <- factor(plotDataAll$season, labels=c("Summer", "Winter"))
plotDataAll$footprint_percent <- plotDataAll$footprint*100
write.csv(plotDataAll, "Flickr_footprint_across_time.csv", row.names=FALSE)


p3 <- ggplot(plotDataAll) +
  geom_area(aes(x=year, y=footprint_percent, fill=type),position = "identity") +
  scale_fill_manual(name="", values=c("#1f78b4","#b2df8a","#a6cee3")) +
  facet_wrap(vars(season)) +
  xlab("Year") + ylab("Footprint (% of Arctic)") +
  scale_x_continuous(expand=c(0,0), breaks=c(2007, 2010, 2013, 2016)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.39)) +
  theme_bw(18) +
  theme(strip.background=element_rect(fill="white", color=NA), strip.text=element_text(size=18), 
        axis.text=element_text(size=16), 
        axis.title.x=element_text(vjust=-0.35), 
        axis.title.y=element_text(vjust=2.0), 
        legend.position="bottom", legend.text=element_text(size=18))
ggsave(filename="Flickr_footprint_metrics.png", p3, width=12, height = 7)
ggsave(filename="Flickr_footprint_metrics.eps", p3, width=12, height = 7)

