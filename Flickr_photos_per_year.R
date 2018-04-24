# devtools::install_github("remi-daigle/flickRgeotag")
require(flickRgeotag) # must have the latest version, commited April 23, 2018
require(ggplot2)
require(tidyverse)
api_key = 'd4a3326bfa4bdbfaba49ab6bd8b40bab'
# we need to grab stats by hour, otherwise the flickr api estimate the total poorly!
df <- expand.grid(datetime=seq(as_datetime("2000-01-01 00:00:00"), as_datetime("2018-04-23 23:59:59"), by="+1 hour"),total=0)
for(y in df$datetime){
  print(as_datetime(y))
  df$total[df$datetime==y] <- flickr.photos.search(api_key,
                                               output = "total",
                                               .allpages = F,
                                               min_taken_date=as_datetime(y),
                                               max_taken_date=as_datetime(y+60*60-1),
  )
}

#plot by hour
ggplot(data = df,aes(x=datetime,y=total))+
  geom_line()

#plot by year
dfByYear = df %>%
  mutate(year=year(datetime)) %>% 
  group_by(year) %>% 
  summarise(total=sum(total))

ggplot(data = dfByYear,aes(x=year,y=total))+
  geom_line()
