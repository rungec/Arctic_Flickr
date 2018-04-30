 devtools::install_github("remi-daigle/flickRgeotag")
require(flickRgeotag) # must have the latest version, commited April 23, 2018
require(ggplot2)
require(tidyverse)
require(lubridate) 
 
api_key = '790ae098b7062ef9d5f4071d0933f23c'

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/"
setwd(wd)

# we need to grab stats by hour, otherwise the flickr api estimate the total poorly!
df <- expand.grid(datetime=seq(as_datetime("2002-08-25 08:00:00"), as_datetime("2018-04-23 23:59:59"), by="+1 hour"),total=0)
for(y in df$datetime){
  print(as_datetime(y))
  df$total[df$datetime==y] <- flickr.photos.search(api_key,
                                               output = "total",
                                               .allpages = F,
                                               min_taken_date=as_datetime(y),
                                               max_taken_date=as_datetime(y+60*60-1)
  )
}

write.csv(df, "tables/Flickr_global_nphotostaken_byhr_2002to2018.csv")

#plot by hour
ggplot(data = df,aes(x=datetime,y=total))+
  geom_line()
ggsave("figures/Flickr_global_nphotostaken_byhr_2000to2018.png")

#plot by year
dfByYear = df %>%
  mutate(year=year(datetime)) %>% 
  group_by(year) %>% 
  summarise(total=sum(total))

ggplot(data = dfByYear,aes(x=year,y=total))+
  geom_line()
ggsave("figures/Flickr_global_nphotostaken_byyr_2000to2018.png")
