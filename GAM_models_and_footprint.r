
require(sf)
require(tidyverse)
require(lubridate)
require(mgcv)

#wd <- "D:/Box Sync/Arctic/Data"
#setwd(wd)
wd <- paste0(getwd(), "/Documents")
setwd(wd)
# flickr data
#load("Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
#load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")
load("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusPAs.Rdata")

#load data on accessibility and PA that has been gridded to 10000m (script Flickr_spatialoverlaps_for_gams.r).
#load("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model/input/ArcticAMAP_griddedaccessibilitydata_10000_m.Rdata")
load("flickr/ArcticAMAP_griddedaccessibilitydata_10000_m.Rdata")

#load grid for footprint analysis
#load("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model/input/ArcticAMAP_templatehexgrid_5000_m.Rdata")
load("flickr/ArcticAMAP_templatehexgrid_5000_m.Rdata")

# global flickr number of photos by year
#AllFlickr <- read.csv("D:/Box Sync/Arctic/Data/Flickr/global_flickr/Flickr_global_nphotostaken_byhr_2000to2018.csv") %>% 
AllFlickr <- read.csv("flickr/Flickr_global_nphotostaken_byhr_2000to2018.csv") %>%   mutate(year=year(datetime), month=month(datetime)) %>% 
  filter(year>=min(flickramap$year)) %>% 
  mutate(season=if_else(month %in% c(1, 2,3,4, 11, 12), "winter", "summer")) %>%
  mutate(year_new=if_else(month %in% c(11,12), year+1, year)) %>%
  mutate(yearseason = paste(year_new, season, sep="_")) %>%
  group_by(yearseason) %>% 
  summarize("Total Flickr Photos" = sum(total))


## We need to calculate the number of Flickr photos per year and correct it based on total Flickr usage over the years

#add yearseason to flickramap
flickramap$season <- "summer"
flickramap$season[flickramap$month %in% c("01", "02", "03", "04", "11", "12")] <- "winter"
flickramap$year_old <- as.numeric(flickramap$year)
flickramap <- flickramap %>% 
        mutate(year=(if_else(month %in% c("11", "12"), year_old + 1, as.numeric(year)))) %>%
        mutate(yearseason = paste(year, season, sep="_"))
# 
#photo-unit-day function
PUD_grid <- function(flickrrecords, time, grid){
  df <- flickrrecords %>% split(time) %>% 
    map( function(x){
           x$col.id <- as.integer(1:nrow(x)) #each col.id represents a separate photo
           grid$row.id <- as.integer(row.names(grid)) #each row.id represents a grid cell
           PUDdf <- as.data.frame(st_intersects(grid, x)) %>% 
             left_join(x, by="col.id") %>% 
             select(row.id, owner_date) %>% 
             unique() %>% 
             group_by(row.id) %>%
             summarize(PUD=n()) %>% 
             right_join(grid, by="row.id") %>% 
             mutate(PUD=if_else(is.na(PUD), 0, as.numeric(PUD))) %>%
             as.tibble() 
            return(PUDdf)
           }) %>% bind_rows(.id="yearseason") %>%       
        separate(yearseason, c("year", "season"), sep="_", remove=FALSE)
  return (df)
  }
    
#function to adjust by global flickr trends
adj_fun <- function(gridYearPUD){
          byYear <- gridYearPUD %>%
                      data.frame() %>% 
                      group_by(yearseason) %>% 
                      summarise("PUD"=sum(PUD)) %>% 
                      left_join(AllFlickr,by="yearseason") %>% 
                      mutate("Percent Traffic"=.$"PUD"/.$"Total Flickr Photos",
                             correctionFact=mean(.$"Total Flickr Photos")/.$"Total Flickr Photos") %>%
                      gather(key="variable",value="value",-yearseason) %>%
                      separate(yearseason, c("year", "season"), sep="_", remove=FALSE)
                     return(byYear)
}

#how many pud in each grid cell in each year-season, for the models
gridYearPUD_models <- PUD_grid(flickramap, flickramap$yearseason, grid_models) 
saveRDS(gridYearPUD_models,file = paste0("gridYearPUD_models",10000,"_m.Rdata"))
byYear_models <- adj_fun(gridYearPUD_models)
saveRDS(byYear_models,file = paste0("flickr/byYear_models",10000,"_m.Rdata"))
rm(gridYearPUD_models)

#how many photos in each grid cell in each year-season, for the footprint analysis
gridYearPUD_footprint <- PUD_grid(flickramap, flickramap$yearseason, grid_footprint) 
saveRDS(gridYearPUD_footprint,file = paste0("gridYearPUD_footprint",5000,"_m.Rdata"))
byYear_footprint <- adj_fun(gridYearPUD_footprint)
saveRDS(byYear_footprint,file = paste0("flickr/byYear_footprint",5000,"_m.Rdata"))

## Now that we have all our basic data ready to go, let's vizualize!

# First, lets see if there is a change in the number of PUDs over time?

correct <- byYear_footprint %>% 
  filter(variable=="correctionFact") %>% 
  select(-variable)
byYear <- byYear_footprint %>% 
  filter(variable!="correctionFact")

plotData <- byYear
#plot showing all, arctic, and % traffic
unique(plotData$variable)
#reorder
plotData$variable <- ordered(plotData$variable,levels=c("Total Flickr Photos","PUD","Percent Traffic"))
plotData$year <- as.numeric(plotData$year)
p1 <- ggplot(plotData) + 
  geom_line(aes(x=year,y=value, color=season)) +
  facet_grid(vars(variable),scales="free_y")
ggsave(filename="flickr/Flickr_footprint_vs_global.png", p1)


## Both the total number of photos on Flickr and in the Arctic increase over time, but the Arctic represents an increasing share of Flickr's yearly photo traffic.

# Are these increasing numbers of tourists always visiting the same places, or are they exploring new grounds?

# spatial footprint expanded?

plotData2 <- filter(gridYearPUD_footprint, PUD>0) %>% 
  left_join(correct, by="yearseason") %>% 
  mutate(correctedPUD = PUD*value)

p2 <- ggplot(plotData2)+
  geom_histogram(aes(x=correctedPUD))+
  scale_y_log10() + 
  xlim(0,200) +
  facet_wrap(vars(yearseason), ncol=4, strip.position="right")
ggsave(filename="flickr/Flickr_footprint_yearseason.png", p2)

# or
plotData <- gridYearPUD_footprint %>% 
  group_by(yearseason) %>% 
  summarise(footprint = mean(PUD>0)) %>% #this calculates the proportion of cells with photos
  mutate(type="Overall Flickr Footprint")

# this samples n records from each year in flickramap
# n was the number of records in the first year
# we want an n equal to the smallest year*season combo, so min(table(flickramap$yearseason))
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
  
# this recalculates PUD by yearseason by grid cell for equaln_sample_flickr
plotData_equaln_s <- PUD_grid(equaln_sample_flickr_s, equaln_sample_flickr_s$yearseason, grid_footprint) %>%
  group_by(yearseason) %>% 
  summarise(footprint=mean(PUD>0) )%>% 
  mutate(type="With Equal N")
plotData_equaln_w <- PUD_grid(equaln_sample_flickr_w, equaln_sample_flickr_w$yearseason, grid_footprint)  %>%
  group_by(yearseason) %>% 
  summarise(footprint=mean(PUD>0) )%>% 
  mutate(type="With Equal N")

trafficn_sample_flickr <- flickramap %>% 
  # mutate(year=as.numeric(year)) %>% 
  nest(-yearseason)  %>% 
  left_join(correct,by="yearseason") %>% 
  mutate(n=round(map_dbl(data, nrow)/max(value)*value)) %>%
  mutate(Sample = map2(data, n, sample_n)) %>%
  unnest(Sample) %>% 
  st_as_sf(crs = st_crs(flickramap))

table(trafficn_sample_flickr$yearseason)
table(flickramap$yearseason)

plotData_trafficn <- PUD_grid(trafficn_sample_flickr, trafficn_sample_flickr$yearseason, grid_footprint) %>% 
  group_by(yearseason) %>% 
  summarise(footprint=mean(PUD)) %>% 
  mutate(type="With Increased Arctic Traffic")

plotDataAll <- rbind(plotData,plotData_equaln_s, plotData_equaln_w, plotData_trafficn)

plotDataAll$type <- ordered(plotDataAll$type,levels = unique(plotDataAll$type)[c(1,3,2)])
plotDataAll <- separate(plotDataAll, yearseason, c("year", "season"), sep="_", remove=FALSE) %>%     mutate(year=as.numeric(year))

p3 <- ggplot(plotDataAll)+
  geom_area(aes(x=year, y=footprint, fill=type),position = "identity")+
  scale_fill_manual(values=c("#1f78b4","#b2df8a","#a6cee3"))+
  facet_wrap(vars(season))
ggsave(filename="flickr/Flickr_footprint_metrics.png", p3)

# or
#plotData3 <- gridYearPUD_footprint %>% 
 # group_by(year, season, country) %>% 
 # summarise(footprint=mean(PUD>0)) %>% 
#  mutate(year=as.numeric(year))

#ggplot(plotData3)+
 # geom_line(aes(x=year,y=footprint,color=country))

rm(gridYearPUD_footprint)

## Let's see if we can see what is driving these changes. First lets model whether people visit protected areas more than expected

gridYearPUD_models <- readRDS("gridYearPUD_models10000_m.Rdata")

gridYearPUD_models <- gridYearPUD_models %>%
  select(-one_of("geometry")) %>%
  filter(country!="Russia") %>%
  mutate(dist2airports=dist2airports/1000,
         dist2ports = dist2ports/1000,
         dist2populated_places = dist2populated_places/1000,
         dist2road = dist2road/1000,
         dist2urban_areas = dist2urban_areas/1000, 
         PA = as.factor(PA), 
         PUDlog10=log10(PUD+1),
         country=as.factor(country))


#set norway as the reference level
gridYearPUD_models <- within(gridYearPUD_models, country <- relevel(country, ref = "NOR"))
#dropped year, dropped populatedplaces, dist2popplaces, urbanareas
g <- gam(PUDlog10 ~ s(Latitude)+
           s(Longitude)+
           country+
           propPA+
           season+
           PA+
           roadlength+
           dist2road+
           airports+
           dist2airports+
           ports+
           dist2ports+
           dist2urban_areas,
         data = gridYearPUD_models, method = "REML")
sink("GAM_model1_summary.txt")
print(gam.check(g))
print(summary(g))
print(AIC(g))
sink()
saveRDS(file="GAM_model1.Rdata", g)


#dropped year, dropped populatedplaces, dist2popplaces, urbanareas
g <- gam(PUDlog10 ~ s(Latitude)+
           s(Longitude)+
           country+
           PA+
           roadlength+
           dist2road+
           dist2airports+
           dist2ports+
           dist2populated_places,
         data = gridYearPUD_models, method = "REML")
sink("GAM_model2_summary.txt")
print(gam.check(g))
print(summary(g))
print(AIC(g))
sink()
saveRDS(file="GAM_model2.Rdata", g)


# Now lets see if people use different types of access in different seasons.

gridYearsummermod <- gridYearPUD_models%>% 
  filter(season=="summer") 

gridYearwintermod <- gridYearPUD_models %>% 
  filter(season=="winter") 

#Summer
gridYearsummermod <- within(gridYearsummermod, country <- relevel(country, ref = "NOR"))
gs <- gam(photoCountlog10 ~ s(Latitude)+
           s(Longitude)+
           year+
           country+
           propPA+
           PA+
           roadlength+
           dist2road+
           airports+
           dist2airports+
           ports+
           dist2ports+
           populated_places+
           dist2populated_places+
           urban_areas+
           dist2urban_areas,
         data = gridYearsummermod, method = "REML")
sink("GAM_model1_summer_summary.txt")
print(gam.check(gs))
print(summary(gs))
print(AIC(gs))
sink()
saveRDS(file="GAM_model1_summer.Rdata", gs)

#Winter
gridYearwintermod <- within(gridYearwintermod, country <- relevel(country, ref = "Norway"))
gw <- gam(photoCountlog10 ~ s(Latitude)+
           s(Longitude)+
           year+
           country+
           propPA+
           PA+
           roadlength+
           dist2road+
           airports+
           dist2airports+
           ports+
           dist2ports+
           populated_places+
           dist2populated_places+
           urban_areas+
           dist2urban_areas,
         data = gridYearwintermod, method = "REML")
sink("GAM_model1_winter_summary.txt")
print(gam.check(gw))
print(summary(gw))
print(AIC(gw))
sink()
saveRDS(file="GAM_model1_winter.Rdata", gw)
