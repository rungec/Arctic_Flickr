#########################
##LIBRARIES and FILES
#########################
require(sf)
require(tidyverse)
require(lubridate)
require(mgcv)


#wd <- "D:/Box Sync/Arctic/Data"
#setwd(wd)
wd <- paste0(getwd(), "/Documents")
setwd(wd)

gridYearPUD_models <- readRDS("gridYearPUD_models10000_m.Rdata")

#########################
##SET UP DATA
#########################
gridYearPUD_models <- gridYearPUD_models %>%
  select(-one_of("geometry")) %>%
  filter(!country %in% c("RUS", "Ocean")) %>%
  mutate(dist2airports=dist2airports/1000,
         dist2ports = dist2ports/1000, #converts distances to km
         dist2populated_places = dist2populated_places/1000,
         dist2road = dist2road/1000,
         dist2urban_areas = dist2urban_areas/1000, 
         sqrtroadlength = sqrt(roadlength/1000),
         PA = as.factor(PA), 
         #PUDlog10=log10(PUD+1),
         country=as.factor(country),
         year=as.integer(year)) %>%
    mutate(logdist2airports=if_else(dist2airports<1, 0, log(dist2airports)), #avoids negative log
         logdist2ports = if_else(dist2ports<1, 0, log(dist2ports)),
         logdist2populated_places = if_else(dist2populated_places<1, 0, log(dist2populated_places)),
         logdist2road = if_else(dist2road<1, 0, log(dist2road))) 
#set norway as the reference level
gridYearPUD_models <- within(gridYearPUD_models, country <- relevel(country, ref = "NOR"))


#########################
##MODELS
#########################
gridYearsummermod <- gridYearPUD_models %>% 
  filter(season=="summer") %>% 
  mutate(PUD_pa = if_else(PUD>0, 1, 0) ) #reclassify as 1,0


gridYearwintermod <- gridYearPUD_models %>% 
  filter(season=="winter") %>% 
  mutate(PUD_pa = if_else(PUD>0, 1, 0) ) #reclassify as 1,0

###set up cluster
require(parallel)  
nc <- 8   ## cluster size
if (detectCores()>1) { 
  cl <- makeCluster(nc) 
} else cl <- NULL

#Summer
k=20
gs <- bam(PUD_pa ~ s(year, bs="cr", k=9) + 
            s(Latitude, bs="cr", k=k) + s(Longitude, bs="cr", k=k)+
            country +
            PA +
            sqrtroadlength +
            s(logdist2road, bs="cr", k=k) +
            s(logdist2airports, bs="cr", k=k) +
            s(logdist2ports, bs="cr", k=k) +
            s(logdist2populated_places, bs="cr", k=k),
          data = gridYearsummermod,  
          family = nb(link="sqrt"), cluster=cl)

sink("GAM_summer_model1_summary_sqrt_noocean_sqrtroadlength_logvars.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
par(mfrow=c(2,2))
gam.check(gs)
sink()

par(mfrow=c(3,3))
plot(gs)
#saveRDS(file="GAM_summer_model1_summary_log_noocean_sqrtroadlength_logvars.Rdata", gs)

#this model is not supported
gmms <-gamm(PUD ~ s(year) + 
            s(Latitude) + s(Longitude)+
            PA +
            s(sqrtroadlength) +
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places), 
            random=list(country=~1),
          data = gridYearsummermod,  
          family = nb(link="log"), cluster=cl)

sink("GAMM_summer_model1_summary_log_noocean_sqrtroadlength_logvars.txt")
print(anova(gmms$lme))
print(summary(gmms$gam))
print(paste0("AIC ", AIC(gmms$gam)))
par(mfrow=c(2,2))
gam.check(gmms$gam)
sink()

par(mfrow=c(1,3))
plot(gmms$gam)
#saveRDS(file="GAMM_summer_model1_summary_log_noocean_sqrtroadlength_logvars.Rdata", gs)

#####BINOMIAL

gs <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model1_summary_binomial_noocean_sqrtroadlength_logvars.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
par(mfrow=c(2,2))
gam.check(gs)
sink()

par(mfrow=c(3,3))
plot(gs)
#saveRDS(file="GAM_summer_model1_summary_binomial_noocean_sqrtroadlength_logvars.Rdata", gs)
#gs <- readRDS(file="GAM_summer_model1_summary_binomial_noocean_sqrtroadlength_logvars.Rdata")

gs <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            #dist2road +
            #dist2airports +
            #dist2ports +
            #dist2populated_places +
            sqrtroadlength,
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model0_summary_binomial_nodistvars.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
par(mfrow=c(1,3))
plot(gs)
par(mfrow=c(2,2))
gam.check(gs)
sink()



if (!is.null(cl)) stopCluster(cl)


#most parsimonious
gs0 <- bam(PUD_pa ~ s(year) + 
             #s(Latitude) + s(Longitude)+
             #country +
             #PA +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl)
anova(gs0)
sink("GAM_summer_model0_summary_binomial_noocean_sqrtroadlength.txt")
print(anova(gs0))
print(summary(gs0))
print(paste0("AIC ", AIC(gs0)))
par(mfrow=c(2,2))
gam.check(gs0)
sink()

par(mfrow=c(1,3))
plot(gs0)

#quite parsimonious
gs1 <- bam(PUD_pa ~ s(year) + 
             #s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl)
anova(gs1)
sink("GAM_summer_model1_summary_binomial_noocean_sqrtroadlength.txt")
print(anova(gs1))
print(summary(gs1))
print(paste0("AIC ", AIC(gs1)))
print(anova(gs0, gs1))
par(mfrow=c(2,2))
gam.check(gs1)
sink()

#tiff("GAM_summer_model1_smooths_binomial_noocean_sqrtroadlength.tiff")
par(mfrow=c(1,3))
plot(gs1)
#dev.off()



#add variables
gs2 <- bam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2road,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model2_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs2))
  print(summary(gs2))
  print(paste0("AIC ", AIC(gs2)))
  par(mfrow=c(2,2))
  gam.check(gs2)
  sink()
gs3 <- bam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2airports,
           data = gridYearsummermod,
           family = binomial, cluster=cl)
  sink("GAM_summer_model3_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs3))
  print(summary(gs3))
  print(paste0("AIC ", AIC(gs3)))
  par(mfrow=c(2,2))
  gam.check(gs3)
  sink()
gs4 <- bam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model4_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs4))
  print(summary(gs4))
  print(paste0("AIC ", AIC(gs4)))
  par(mfrow=c(2,2))
  gam.check(gs4)
  sink()
gs5 <- bam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2ports,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model5_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs1, gs2, gs3, gs4, gs5))
  print(anova(gs5))
  print(summary(gs5))
  print(paste0("AIC ", AIC(gs5)))
  par(mfrow=c(2,2))
  gam.check(gs5)
  sink()


#add 2 variables
gs6 <- gam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2airports + dist2road,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model6_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs6))
  print(summary(gs6))
  print(paste0("AIC ", AIC(gs6)))
  par(mfrow=c(2,2))
  gam.check(gs6)
  sink()
gs7 <- gam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2airports + dist2ports,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model7_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs7))
  print(summary(gs7))
  print(paste0("AIC ", AIC(gs7)))
  par(mfrow=c(2,2))
  gam.check(gs7)
  sink()
gs8 <- gam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             dist2airports + dist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model8_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs8))
  print(summary(gs8))
  print(paste0("AIC ", AIC(gs8)))
  par(mfrow=c(2,2))
  gam.check(gs8)
  sink()
gs9 <- gam(PUD_pa ~ s(year) + 
             s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength + 
             dist2ports + dist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model9_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs1, gs6, gs7, gs8, gs9))
  print(anova(gs9))
  print(summary(gs9))
  print(paste0("AIC ", AIC(gs9)))
  par(mfrow=c(2,2))
  gam.check(gs9)
  sink()
  
gs10 <- gam(PUD_pa ~ s(year) + 
               s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength + 
               dist2road + dist2populated_places,
             data = gridYearsummermod, 
             family = binomial, cluster=cl)
  sink("GAM_summer_model10_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs10))
  print(summary(gs10))
  print(paste0("AIC ", AIC(gs10)))
  par(mfrow=c(2,2))
  gam.check(gs10)
  sink()
gs11 <- gam(PUD_pa ~ s(year) + 
                s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength + 
                logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model11_summary_binomial_noocean_sqrtroadlength.txt")
  print(anova(gs11))
  print(summary(gs11))
  print(paste0("AIC ", AIC(gs11)))
  par(mfrow=c(2,2))
  gam.check(gs11)
  sink()
  
 gs12 <- gam(PUD_pa ~ s(year) + 
                s(Latitude) + s(Longitude)+
                country +
                PA +
                #sqrtroadlength + 
                dist2road + dist2airports + dist2ports + dist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model12_summary_binomial_noocean_nosqrtroadlength.txt")
  print(anova(gs12))
  print(summary(gs12))
  print(paste0("AIC ", AIC(gs12)))
  par(mfrow=c(2,2))
  gam.check(gs12)
  sink()
  
  gs13 <- gam(PUD_pa ~ s(year) + 
                s(Latitude) + s(Longitude)+
                country +
                #PA +
                sqrtroadlength + 
                dist2road + dist2airports + dist2ports + dist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model13_summary_binomial_noocean_sqrtroadlength_noPA.txt")
  print(anova(gs13))
  print(summary(gs13))
  print(paste0("AIC ", AIC(gs13)))
  par(mfrow=c(2,2))
  gam.check(gs13)
  sink()
  
gsb <- bam(PUD_pa ~ s(year, k=10) + 
              s(Latitude, k=30) + s(Longitude, k=30)+
              country +
              PA +
              dist2road +
              dist2airports +
              dist2ports +
              dist2populated_places +
              sqrtroadlength,
            data = gridYearsummermod,  
            family = binomial, cluster=cl) 
sink("GAM_summer_model1c_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gsb))
print(summary(gsb))
print(paste0("AIC ", AIC(gsb)))
par(mfrow=c(2,2))
gam.check(gsb)
sink() 


gs <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(dist2road) +
            s(dist2airports) +
            s(dist2ports) +
            s(dist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model1_summary_binomial_noocean_sqrtroadlength_vars.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
par(mfrow=c(2,2))
gam.check(gs)
sink()


if (!is.null(cl)) stopCluster(cl)

###SUMMER BINOMIAL plus plots

gs1 <- gam(PUD_pa ~ s(year, k=10) + 
               s(Latitude, k=20) + s(Longitude, k=20)+
               country +
               PA +
               sqrtroadlength + 
               dist2road + dist2airports + dist2ports + dist2populated_places,
             data = gridYearsummermod, 
             family = binomial, cluster=cl)
sink("GAM_summer_model1c_summary_binomial_noocean_sqrtroadlength.txt")
print(anova(gs1))
print(summary(gs1))
print(paste0("AIC ", AIC(gs1)))
tiff("GAM_summer_model1c_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs1)
dev.off()
sink()

tiff("GAM_summer_binomialmodel1c_plot_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearsummermod$sqrtroadlength,residuals(gs1), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearsummermod$dist2road,residuals(gs1), cex=0.5, pch=20, xlab="dist2road")
plot(gridYearsummermod$dist2airports,residuals(gs1), cex=0.5, pch=20,  xlab="dist2airports")
plot(gridYearsummermod$dist2ports,residuals(gs1), cex=0.5, pch=20, xlab="dist2ports")
plot(gridYearsummermod$dist2populated_places,residuals(gs1), cex=0.5, pch=20, xlab="dist2populated_places")
dev.off()

tiff("GAM_summer_binomialmodel1c_plot_residuals_vs_allsmoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gs1, residuals=TRUE)
dev.off()

tiff("GAM_summer_binomialmodel1c_plot_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gs1, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_summer_binomialmodel1c_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs1, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gs1s <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(dist2road) +
            s(dist2airports) +
            s(dist2ports) +
            s(dist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

tiff("GAM_summer_binomialmodel1s_plot_residuals_vs_distancevars.tiff",  width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearsummermod$sqrtroadlength,residuals(gs1s), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearsummermod$dist2road,residuals(gs1s), cex=0.5, pch=20, xlab="dist2road")
plot(gridYearsummermod$dist2airports,residuals(gs1s), cex=0.5, pch=20, xlab="dist2airports")
plot(gridYearsummermod$dist2ports,residuals(gs1s), cex=0.5, pch=20, xlab="dist2ports")
plot(gridYearsummermod$dist2populated_places,residuals(gs1s), cex=0.5, pch=20, xlab="dist2populated_places")
dev.off()

tiff("GAM_summer_binomialmodel1s_plot_residuals_vs_allsmoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gs1s, residuals=TRUE)
dev.off()

tiff("GAM_summer_binomialmodel1s_plot_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gs1s, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_summer_binomialmodel1s_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs1s, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()



gs11b <- gam(PUD_pa ~ s(year, k=10) + 
               s(Latitude, k=20) + s(Longitude, k=20)+
               country +
               PA +
               sqrtroadlength + 
               logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
             data = gridYearsummermod, 
             family = binomial, cluster=cl)
sink("GAM_summer_model11b_summary_binomial_noocean_sqrtroadlength.txt")
print(anova(gs11b))
print(summary(gs11b))
print(paste0("AIC ", AIC(gs11b)))
tiff("GAM_summer_model11b_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs11b)
dev.off()
sink()

tiff("GAM_summer_binomialmodel11_plot_residuals_vs_distancevars.tiff",  width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearsummermod$sqrtroadlength,residuals(gs11b), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearsummermod$logdist2road,residuals(gs11b), cex=0.5, pch=20, xlab="logdist2road")
plot(gridYearsummermod$logdist2airports,residuals(gs11b), cex=0.5, pch=20, xlab="logdist2airports")
plot(gridYearsummermod$logdist2ports,residuals(gs11b), cex=0.5, pch=20, xlab="logdist2ports")
plot(gridYearsummermod$logdist2populated_places,residuals(gs11b), cex=0.5, pch=20, xlab="logdist2populated_places")
dev.off()

tiff("GAM_summer_binomialmodel11_plot_residuals_vs_allsmoothedvars.tiff",  width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gs11b, residuals=TRUE)
dev.off()

tiff("GAM_summer_binomialmodel11_plot_variables_pluspredictions_sewithmean_smoothedvars.tiff",  width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gs11b, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_summer_binomialmodel11_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs11b, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gs <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model11s_summary_binomial_noocean_sqrtroadlength.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
tiff("GAM_summer_model11s_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs)
dev.off()
sink()

tiff("GAM_summer_binomialmodel11s_plot_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearsummermod$sqrtroadlength,residuals(gs), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearsummermod$logdist2road,residuals(gs), cex=0.5, pch=20, xlab="logdist2road")
plot(gridYearsummermod$logdist2airports,residuals(gs), cex=0.5, pch=20, xlab="logdist2airports")
plot(gridYearsummermod$logdist2ports,residuals(gs), cex=0.5, pch=20, xlab="logdist2ports")
plot(gridYearsummermod$logdist2populated_places,residuals(gs), cex=0.5, pch=20, xlab="logdist2populated_places")
dev.off()

tiff("GAM_summer_binomialmodel11s_plot_residuals_vs_allsmoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gs, residuals=TRUE)
dev.off()

tiff("GAM_summer_binomialmodel11s_plot_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gs, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_summer_binomialmodel11s_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()



####WINTER BINOMIAL

gw <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=30) + s(Longitude, k=30) +
             country +
             PA +
             dist2road +
             dist2airports +
             dist2ports +
             dist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model1_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw))
print(summary(gw))
print(paste0("AIC ", AIC(gw)))
tiff("GAM_winter_model1_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw)
dev.off()
sink() 

tiff("GAM_winter_model1_plot_binomial_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearwintermod$sqrtroadlength,residuals(gw), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearwintermod$dist2road,residuals(gw), cex=0.5, pch=20, xlab="dist2road")
plot(gridYearwintermod$dist2airports,residuals(gw), cex=0.5, pch=20, xlab="dist2airports")
plot(gridYearwintermod$dist2ports,residuals(gw), cex=0.5, pch=20, xlab="dist2ports")
plot(gridYearwintermod$dist2populated_places,residuals(gw), cex=0.5, pch=20, xlab="dist2populated_places")
dev.off()

tiff("GAM_winter_model1_plot_binomial_residuals_vs_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gw, residuals=TRUE)
dev.off()

tiff("GAM_winter_model1_plot_binomial_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gw, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_winter_model1_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gw1s <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(dist2road) +
            s(dist2airports) +
            s(dist2ports) +
            s(dist2populated_places),
          data = gridYearwintermod,  
          family = binomial, cluster=cl)
sink("GAM_winter_model1s_summary_binomial_noocean_sqrtroadlength_smoothedvars.txt")
print(anova(gw1s))
print(summary(gw1s))
print(paste0("AIC ", AIC(gw1s)))
tiff("GAM_winter_model1s_fit_binomial_noocean_sqrtroadlength_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw1s)
dev.off()
sink() 

tiff("GAM_winter_model1s_plot_binomial_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearwintermod$sqrtroadlength,residuals(gw1s), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearwintermod$dist2road,residuals(gw1s), cex=0.5, pch=20, xlab="dist2road")
plot(gridYearwintermod$dist2airports,residuals(gw1s), cex=0.5, pch=20, xlab="dist2airports")
plot(gridYearwintermod$dist2ports,residuals(gw1s), cex=0.5, pch=20, xlab="dist2ports")
plot(gridYearwintermod$dist2populated_places,residuals(gw1s), cex=0.5, pch=20, xlab="dist2populated_places")
dev.off()

tiff("GAM_winter_model1s_plot_binomial_residuals_vs_allsmoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gw1s, residuals=TRUE)
dev.off()

tiff("GAM_winter_model1s_plot_binomial_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gw1s, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_winter_model1s_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw1s, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gw11 <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=30) + s(Longitude, k=30) +
             country +
             PA +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model11_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw11))
print(summary(gw11))
print(paste0("AIC ", AIC(gw11)))
tiff("GAM_winter_model11_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw11)
dev.off()
sink() 

tiff("GAM_winter_model11_plot_binomial_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearwintermod$sqrtroadlength,residuals(gw11), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearwintermod$logdist2road,residuals(gw11), cex=0.5, pch=20, xlab="logdist2road")
plot(gridYearwintermod$logdist2airports,residuals(gw11), cex=0.5, pch=20, xlab="logdist2airports")
plot(gridYearwintermod$logdist2ports,residuals(gw11), cex=0.5, pch=20, xlab="logdist2ports")
plot(gridYearwintermod$logdist2populated_places,residuals(gw11), cex=0.5, pch=20, xlab="logdist2populated_places")
dev.off()

tiff("GAM_winter_model11_plot_binomial_residuals_vs_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gw11, residuals=TRUE)
dev.off()

tiff("GAM_winter_model11_plot_binomial_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(1,3))
plot(gw11, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_winter_model11_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw11, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()



gw11s <- bam(PUD_pa ~ s(year) + 
            s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places),
          data = gridYearwintermod,  
          family = binomial, cluster=cl)
sink("GAM_winter_model11s_summary_binomial_noocean_sqrtroadlength_smoothedvars.txt")
print(anova(gw11s))
print(summary(gw11s))
print(paste0("AIC ", AIC(gw11s)))
tiff("GAM_winter_model11s_fit_binomial_noocean_sqrtroadlength_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw11s)
dev.off()
sink() 

tiff("GAM_winter_model11s_plot_binomial_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearwintermod$sqrtroadlength,residuals(gw11s), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearwintermod$logdist2road,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2road")
plot(gridYearwintermod$logdist2airports,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2airports")
plot(gridYearwintermod$logdist2ports,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2ports")
plot(gridYearwintermod$logdist2populated_places,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2populated_places")
dev.off()

tiff("GAM_winter_model11s_plot_binomial_residuals_vs_allsmoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gw11s, residuals=TRUE)
dev.off()

tiff("GAM_winter_model11s_plot_binomial_variables_pluspredictions_sewithmean_smoothedvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,4))
plot(gw11s, shade=TRUE, seWithMean=TRUE, scale=0)
dev.off()

tiff("GAM_winter_model11s_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw11s, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gw11b <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=20) + s(Longitude, k=20) +
             country +
             PA +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model11b_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw11b))
print(summary(gw11b))
print(paste0("AIC ", AIC(gw11b)))
tiff("GAM_winter_model11_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw11b)
dev.off()
sink() 

####
#MODELS without PA

gw14 <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=20) + s(Longitude, k=20) +
             country +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model14_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw14))
print(summary(gw14))
print(paste0("AIC ", AIC(gw14)))
tiff("GAM_winter_model14_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw14)
dev.off()
sink() 

tiff("GAM_winter_model14_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw14, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gs14 <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=20) + s(Longitude, k=20) +
             country +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl) 
sink("GAM_summer_model14_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gs14))
print(summary(gs14))
print(paste0("AIC ", AIC(gs14)))
tiff("GAM_summer_model14_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs14)
dev.off()
sink() 

tiff("GAM_summer_model14_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs14, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

###WINTER MODEL without airport or port
gw10 <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=20) + s(Longitude, k=20) +
             country +
             logdist2road +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model10_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw10))
print(summary(gw10))
print(paste0("AIC ", AIC(gw10)))
tiff("GAM_winter_model10_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw10)
dev.off()
sink() 

tiff("GAM_winter_model10_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw10, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

gw9 <- bam(PUD_pa ~ s(year, k=10) + 
             s(Latitude, k=20) + s(Longitude, k=20) +
             country +
             logdist2road +
             logdist2airports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model9_summary_binomial_noocean_sqrtroadlength_unsmoothedvars.txt")
print(anova(gw9))
print(summary(gw9))
print(paste0("AIC ", AIC(gw9)))
tiff("GAM_winter_model9_fit_binomial_noocean_sqrtroadlength.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw9)
dev.off()
sink() 

tiff("GAM_winter_model9_plot_binomial_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gw9, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()




