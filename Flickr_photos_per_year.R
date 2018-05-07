 devtools::install_github("remi-daigle/flickRgeotag")
require(flickRgeotag) # must have the latest version, commited April 23, 2018
require(ggplot2)
require(tidyverse)
require(lubridate) 
 
api_key = '790ae098b7062ef9d5f4071d0933f23c'

options(timeout=2000) #increase the time before it times out

wd <- "/data/Claire"
#wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

# we need to grab stats by hour, otherwise the flickr api estimate the total poorly!
df <- expand.grid(datetime=seq(as_datetime("2000-01-01 00:00:00"), as_datetime("2018-04-23 23:59:59"), by="+1 hour"),total=0)
for(y in df$datetime){
  print(as_datetime(y))
  df$total[df$datetime==y] <- try(flickr.photos.search(api_key,
                                               output = "total",
                                               .allpages = F,
                                               min_taken_date=as_datetime(y),
                                               max_taken_date=as_datetime(y+60*60-1))
  )
}

write.csv(df, "tables/Flickr_global_nphotostaken_byhr_2000to2018.csv")

#plot by hour
ggplot(data = df,aes(x=datetime,y=total))+
  geom_line() +
  scale_x_datetime(date_minor_breaks="1 year") +
  labs(title="Flickr photos taken by hour",
       x="Year",
       y="Number of photos") +
  theme_minimal(16)
ggsave("figures/Flickr_global_nphotostaken_byhr_2000to2018_ppt.png")

#plot by year
dfByYear = df %>%
  mutate(year=year(datetime)) %>% 
  group_by(year) %>% 
  summarise(total=sum(total))

ggplot(data = dfByYear,aes(x=year,y=total/1000000))+
  geom_line()+
  #geom_line(lwd=1.5, col="darkslategray3")+
  labs(title="Flickr photos taken by year",
       x="Year",
       y="Million photos") +
  scale_x_continuous(minor_breaks=c(2000:2017), limits=c(2000, 2017))+
    theme_minimal(16)
ggsave("figures/Flickr_global_nphotostaken_byyr_2000to2018.png")



####
#function to fill in missing totals (had some issues with internet dropping out)
filelist <- list.files(getwd(), "Flickr_global")
fL <- lapply(filelist, function(x) {
  a <- read.csv(x, header=TRUE)
  return(a[a$total!=0, c("datetime", "total")])
})
fdf <- do.call(rbind, fL)

fdf <- fdf[!duplicated(fdf$datetime),]
fdf$datetime <- as_datetime(fdf$datetime)

fdf <- merge(df, fdf, by.x="datetime", by.y="datetime", all.x=TRUE)
fdf$total <- as.numeric(fdf$total.y)

empty <- fdf[is.na(fdf$total.z), "datetime"]
for(y in empty){
  print(as_datetime(y))
  fdf$total.z[df$datetime==y] <- try(flickr.photos.search(api_key,
                                                          output = "total",
                                                          .allpages = F,
                                                          min_taken_date=as_datetime(y),
                                                          max_taken_date=as_datetime(y+60*60-1))
  )
}
fdf <- fdf[, c("datetime", "total")]
write.csv(fdf, "tables/Flickr_global_nphotostaken_byhr_2000to2018.csv")

