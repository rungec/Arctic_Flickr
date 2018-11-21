require(tidyverse)
require(mgcv)
require(modelr)

setwd("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model/gam_10km")

#gw <- readRDS(file.choose())
#plot(gw, all.terms = TRUE)

currdf <- readRDS("D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model/input/gridYearPUD_models10000_m.Rdata")
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
  #subdf <- subdf %>%
  mutate(PUDlog = log(PUD), 
         PUDlog10 =log10(PUD), 
         logdist2road = log(dist2road),
         logdist2airports = log(dist2airports),
         logdist2ports = log(dist2ports),
         logdist2populated_places = log(dist2populated_places),
         roadkm = roadlength/1000
         )
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


###Check correlation between variables
pairs(subdf[,c(2,7,8,24:28)])

ggplot(subdf) +
  geom_point(aes(x=logdist2airports, y=logPUD))



# The full model
g0 <- gamm(PUDlog ~ s(year) + logdist2road + logdist2airports + logdist2ports + logdist2populated_places + roadkm + PA, 
           random=list(country=~1), 
           data=subdf, method="REML")

# The full model with smooths
g1 <- gamm(PUDlog ~  s(year) + s(logdist2road) + s(logdist2airports) + s(logdist2ports) + s(logdist2populated_places) + s(roadkm) + PA, 
           random=list(country=~1), 
           data=subdf, method="REML")

# The full model with only smoothed airport, roadlength and year
g2 <- gamm(PUDlog ~  s(year) + logdist2road + s(logdist2airports) + logdist2ports + logdist2populated_places + s(roadkm) + PA, 
                 random=list(country=~1), 
                 data=subdf, method="REML")


anova(g0$lme, g1$lme, g2$lme)


par(mfrow=c(1,2))
plot(g0$gam)
sink("GAM_model0_summary.txt")
print(summary(g0$gam))
par(mfrow=c(2,2))
print(gam.check(g0$gam))
sink()

par(mfrow=c(2,3))
plot(g1$gam)
sink("GAM_model1_summary.txt")
print(summary(g1$gam))
par(mfrow=c(2,2))
print(gam.check(g1$gam))
sink()

par(mfrow=c(2,2))
plot(g2$gam)
sink("GAM_model2_summary.txt")
print(summary(g2$gam))
par(mfrow=c(2,2))
print(gam.check(g2$gam))
sink()

par(mfrow=c(1,2))
acf(resid(g2$lme, type="normalized"), main="ACF")
pacf(resid(g2$lme, type="normalized"), main="PACF")


#test temporal autocorrelation structures
gar0 <- gamm(PUDlog ~  s(year) + logdist2road + s(roadkm) + s(logdist2airports) + logdist2ports + logdist2populated_places + PA, 
             random=list(country=~1), 
             correlation=NULL, 
             data=subdf, method="REML")
gar1 <- gamm(PUDlog ~  s(year) + logdist2road + s(roadkm) + s(logdist2airports) + logdist2ports + logdist2populated_places + PA, 
             random=list(country=~1), 
             correlation = corARMA(form = ~ 1|year, p=1), 
             data=subdf, method="REML")
gma2 <- gamm(PUDlog ~  s(year) + logdist2road + s(roadkm) + s(logdist2airports) + logdist2ports + logdist2populated_places + PA, 
             random=list(country=~1), 
             correlation = corARMA(form = ~ 1|year, p=1, q=2), 
             data=subdf, method="REML")

anova(gar0$lme, gar1$lme, gma2$lme)

par(mfrow=c(2,2))
plot(gma2$gam)
sink("GAM_model2_ma2_summary.txt")
print(summary(gma2$gam))
par(mfrow=c(2,2))
print(gam.check(gma2$gam))
sink()

par(mfrow=c(2,2))
acf(resid(g2$lme, type="normalized"), main="ACF")
pacf(resid(g2$lme, type="normalized"), main="PACF")
acf(resid(gma2$lme, type="normalized"), main="ACF")
pacf(resid(gma2$lme, type="normalized"), main="PACF")


#try more parsimonious models
# Drop PA
g3 <- gamm(PUDlog ~  s(year) + logdist2road + s(logdist2airports) + logdist2ports + logdist2populated_places + s(roadkm), 
           random=list(country=~1), 
           correlation = corARMA(form = ~ 1|year, p=1, q=2),
           data=subdf, method="REML")
# Drop Populated places and PA
g4 <- gamm(PUDlog ~  s(year) + logdist2road + s(logdist2airports) + logdist2ports + s(roadkm), 
           random=list(country=~1),
           correlation = corARMA(form = ~ 1|year, p=1, q=2),
           data=subdf, method="REML")
# Drop dist2roads and PA
g5 <- gamm(PUDlog ~  s(year) + s(logdist2airports) + logdist2ports + logdist2populated_places + s(roadkm), 
           random=list(country=~1), 
           correlation = corARMA(form = ~ 1|year, p=1, q=2),
           data=subdf, method="REML")
# Drop dist2roads
g6 <- gamm(PUDlog ~  s(year) + s(logdist2airports) + logdist2ports + logdist2populated_places + s(roadkm) + PA, 
           random=list(country=~1), 
           correlation = corARMA(form = ~ 1|year, p=1, q=2),
           data=subdf, method="REML")
# Drop dist2roads and PA and logdist2ports
g7 <- gamm(PUDlog ~  s(year) + s(logdist2airports) + logdist2populated_places + s(roadkm), 
           random=list(country=~1), 
           correlation = corARMA(form = ~ 1|year, p=1, q=2),
           data=subdf, method="REML")
anova(gma2$lme, g3$lme) #no significant difference between models
anova(gma2$lme, g4$lme) #model 2 significantly better than model 4
anova(gma2$lme, g5$lme) #small but not very significant difference between models
anova(gma2$lme, g6$lme) #small but not very significant difference between models
anova(gma2$lme, g7$lme) #small but not very significant difference between models

par(mfrow=c(2,2))
plot(g5$gam)
sink("GAM_model5_ma2_summary.txt")
print(summary(g5$gam))
par(mfrow=c(2,2))
print(gam.check(g5$gam))
sink()


#check spatial autocorrelation using 2016 data
gsc <- gamm(PUDlog ~ logdist2road + s(roadkm) + s(logdist2airports) + logdist2ports + logdist2populated_places + PA, random=list(country=~1), correlation = corRatio(form = ~ Latitude + Longitude, nugget=TRUE), data=subdf[subdf$year==2016,], method="REML")
V1<- Variogram(gsc$lme, form=~Latitude+Longitude, nugget=TRUE, data=subdf[subdf$year==2016,])
plot(V1, smooth=FALSE)

dists <- dist(subdf[,7:8])
summary(dists)
d0 <- which(dists==0) #some are zero because we have temporal replicates


#vis.gam


### Make marginal plots
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
