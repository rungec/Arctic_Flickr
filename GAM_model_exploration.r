require(tidyverse)
require(mgcv)
require(modelr)


#gw <- readRDS(file.choose())
#plot(gw, all.terms = TRUE)

currdf <- readRDS(file.choose())
currdf <- currdf %>% filter(season=="winter") %>%
  select(-one_of("geometry")) %>%
  filter(country!="Russia") %>%
  mutate(dist2airports=if_else(dist2airports==0, 1, dist2airports), 
         dist2ports = if_else(dist2ports==0, 1, dist2ports),
         dist2populated_places = if_else(dist2populated_places==0, 1, dist2populated_places),
         dist2road = if_else(dist2road==0, 1, dist2road),
         dist2urban_areas = if_else(dist2urban_areas==0, 1, dist2urban_areas), 
         PA = as.factor(PA), 
         #PUDlog=log(PUD+0.00001),
         #PUDlog10=log10(PUD+0.00001),
         country=as.factor(country),
         year=as.integer(year))

subdf <- currdf %>% filter(PUD>0) %>% 
  #sample_n(100000) %>% 
  mutate(PUDlog = log(PUD), 
         PUDlog10 =log10(PUD))
rm(currdf)

#SET UP PLOT FUNCTIONS
plotmodel <- function(x, y, preds2){
  preds2 %>%  
    ggplot(aes(substitute(x), substitute(y))) +
    geom_point(aes(col=country)) +
    geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
    geom_line(aes(y=fit), col="black") +
    #facet_wrap(~Region) +
    #theme(legend.position="none") +
    theme_minimal()
} 

plotmodelbyregion <- function(x, y, preds2){
  preds2 %>% ggplot(aes(substitute(x), substitute(y))) +
    geom_point(col="darkslategray2") +
    geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
    geom_line(aes(y=fit), col="black", lwd=0.7) +
    facet_wrap(~country) +
    theme_light() +
    theme(legend.position="none", panel.grid=element_blank(), strip.text = element_text(colour = "black"))
}  






pairs(subdf[,c(2,7,8,10:21)])

ggplot(subdf) +
  geom_point(aes(x=log(dist2airports), y=log(PUD)))

pairs(sample_n(gridYearPUD_models[, 10:21], 10000))

g1 <- gamm(PUD ~ s(year) + s(dist2road), data=subdf, method="REML")
g2 <- gamm(PUD ~ s(year) + s(dist2road) + s(Latitude) + s(Longitude), data=subdf, method="REML")
g3 <- gamm(PUD ~ s(year) + s(dist2road) + country + dist2airports + dist2ports + dist2populated_places, data=subdf, method="REML")
g4 <- gamm(PUDlog10 ~ s(year) + s(dist2road) + country + s(dist2airports) + s(dist2ports) + s(dist2populated_places), data=subdf, method="REML")
g5 <- gamm(PUDlog10 ~ s(year) + s(log(dist2road)) + country + dist2airports + dist2ports + dist2populated_places, data=subdf, method="REML")
g6 <- gamm(PUDlog10 ~ s(year) + log(dist2road) + country + PA + dist2airports + dist2ports + dist2populated_places, data=subdf, method="REML")
g8 <- gamm(PUDlog ~ s(year) + log(dist2road) + country + PA + log(dist2airports) + log(dist2ports) , data=subdf, method="REML")
g9 <- gamm(PUDlog ~ s(year) + log(dist2road) + country + log(dist2airports) + log(dist2ports) + log(dist2populated_places), data=subdf, method="REML")
g10 <- gamm(PUDlog ~ s(year) + log(dist2road) + log(dist2airports) + log(dist2ports) + log(dist2populated_places), data=subdf, method="REML")
g11 <- gamm(PUDlog ~ s(year) + log(dist2road) + country + log(dist2airports) + log(dist2populated_places), data=subdf, method="REML")
anova(g7$lme, g8$lme, g9$lme, g10$lme, g11$lme)
anova(g7$lme, g9$lme) #no signif difference


#
g7 <- gamm(PUDlog ~ s(year) + log(dist2road) + log(dist2airports) + log(dist2ports) + log(dist2populated_places) + country + PA, data=subdf, method="REML")

#test autocorrelation structures
gar0 <- gamm(PUDlog ~  s(year) + log(dist2road) + log(dist2airports) + log(dist2ports) + log(dist2populated_places) + PA, random=list(country=~1), correlation=NULL, data=subdf, method="REML")
gar1 <- gamm(PUDlog ~  s(year) + log(dist2road) + log(dist2airports) + log(dist2ports) + log(dist2populated_places) + PA, random=list(country=~1), correlation = corARMA(form = ~ 1|year, p=1), data=subdf, method="REML")
gma2 <- gamm(PUDlog ~  s(year) + log(dist2road) + log(dist2airports) + log(dist2ports) + log(dist2populated_places) + PA, random=list(country=~1), correlation = corARMA(form = ~ 1|year, p=1, q=2), data=subdf, method="REML")

anova(gar0$lme, gar1$lme, gma2$lme)


AIC(g7b$lme)
summary(g7b$gam)
par(mfrow=c(1,1))
gam.check(g7b$gam)

acf(resid(gar0$lme, type="normalized"), main="ACF")
pacf(resid(gar0$lme, type="normalized"), main="PACF")

plot(g4$gam)

par(mfrow=c(1,2))
plot(g3$gam)

par(mfrow=c(1,2))
acf(resid(g1$lme, type="normalized"), lag=20, main="ACF")
pacf(resid(g1$lme, type="normalized"), lag=20, main="PACF")

newdf <- subdf %>% mutate(dist2road=seq(0, 1, length.out = 10000), 
                    Latitude=mean(Latitude), 
                    Longitude=mean(Longitude),
                    country=as.factor("NOR"),
                    roadlength=mean(roadlength), 
                    dist2airports=mean(dist2airports),
                    dist2ports=mean(dist2ports),
                    dist2populated_places=mean(dist2populated_places),
                    PA=as.factor(FALSE))
                    
newpreds <- newdf %>% add_predictions(g7$gam)

ggplot(newpreds, aes(x=dist2road, y=pred)) +
  geom_point(col="black")+ facet_wrap(~year)


####      
ggplot(subdf, aes(x=log(dist2airports+0.00001), y=PUD)) +
  geom_point(col="black")+ facet_wrap(~year)
p1 <- ggplot(newpreds, aes(x=dist2airports, y=pred)) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  geom_line(aes(y=fit), col="black", lwd=0.7)