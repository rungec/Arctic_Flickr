#########################
##LIBRARIES and FILES
#########################
require(sf)
require(tidyverse)
require(lubridate)
require(mgcv)


#wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model_and_footprint/input/"
#setwd(wd)
wd <- paste0(getwd(), "/Documents")
setwd(wd)

gridYearPUD_models <- readRDS("gridYearPUD_models10000_m.Rdata")

outdir <- "gam_10km_binomial_noyear" ##EDIT ME

#########################
##SET UP DATA
#########################
gridYearPUD_models <- gridYearPUD_models %>%
  select(-one_of("geometry")) %>%
  filter(!country %in% c("RUS", "Ocean")) %>%
  group_by(season, row.id) %>%
  summarise(PUD=max(PUD), 
          Latitude = mean(Latitude),
          Longitude = mean(Longitude),
          dist2airports=mean(dist2airports)/1000,
         dist2ports = mean(dist2ports)/1000, #converts distances to km
         dist2populated_places = mean(dist2populated_places)/1000,
         dist2road = mean(dist2road)/1000,
         dist2urban_areas = mean(dist2urban_areas)/1000, 
         sqrtroadlength = sqrt(mean(roadlength)/1000),
         PA = first(PA), 
         #PUDlog10=log10(PUD+1),
         country=first(country)) %>%
    mutate(PA=as.factor(PA),
           country=as.factor(country), 
           logdist2airports=if_else(dist2airports<1, 0, log(dist2airports)), #avoids negative log
         logdist2ports = if_else(dist2ports<1, 0, log(dist2ports)),
         logdist2populated_places = if_else(dist2populated_places<1, 0, log(dist2populated_places)),
         logdist2road = if_else(dist2road<1, 0, log(dist2road))) 
#set norway as the reference level
gridYearPUD_models <- within(gridYearPUD_models, country <- relevel(country, ref = "NOR"))


#########################
##MODELS
#########################
#set the output directory for files
setwd(paste(dirname(getwd()), outdir, sep="/"))

#set up summer and winter datasets
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

###################
###SUMMER BINOMIAL 
###################
#Model with all the variables
g1 <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
             country +
             PA +
             sqrtroadlength + 
             dist2road + dist2airports + dist2ports + dist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model1_summary_binomial_noocean.txt")
  print(anova(g1))
  print(summary(g1))
  print(paste0("AIC ", AIC(g1)))
  tiff("GAM_summer_model1_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(g1)
  dev.off()
  sink()
  
  tiff("GAM_summer_model1_plot_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearsummermod$sqrtroadlength,residuals(g1), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearsummermod$dist2road,residuals(g1), cex=0.5, pch=20, xlab="dist2road")
  plot(gridYearsummermod$dist2airports,residuals(g1), cex=0.5, pch=20,  xlab="dist2airports")
  plot(gridYearsummermod$dist2ports,residuals(g1), cex=0.5, pch=20, xlab="dist2ports")
  plot(gridYearsummermod$dist2populated_places,residuals(g1), cex=0.5, pch=20, xlab="dist2populated_places")
  dev.off()
  
  tiff("GAM_summer_model1_plot_residuals_vs_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(g1, residuals=TRUE)
  dev.off()
  
  tiff("GAM_summer_model1_plot_variables_pluspredictions_sewithmean_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(g1, shade=TRUE, seWithMean=TRUE, scale=0)
  dev.off()
  
  tiff("GAM_summer_model1_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(g1, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()

#Model with all the variables, logged
g11 <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
            country +
            PA +
            sqrtroadlength + 
            logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
          data = gridYearsummermod, 
          family = binomial, cluster=cl)
sink("GAM_summer_model11_summary_binomial_logvars.txt")
print(anova(g11))
print(summary(g11))
print(paste0("AIC ", AIC(g11)))
tiff("GAM_summer_model11_fit_binomial_logvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(g11)
sink()
  
  tiff("GAM_summer_model11_plot_residuals_vs_logdistancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearsummermod$sqrtroadlength,residuals(g11), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearsummermod$logdist2road,residuals(g11), cex=0.5, pch=20, xlab="logdist2road")
  plot(gridYearsummermod$logdist2airports,residuals(g11), cex=0.5, pch=20,  xlab="logdist2airports")
  plot(gridYearsummermod$logdist2ports,residuals(g11), cex=0.5, pch=20, xlab="logdist2ports")
  plot(gridYearsummermod$logdist2populated_places,residuals(g11), cex=0.5, pch=20, xlab="logdist2populated_places")
  dev.off()
  
  tiff("GAM_summer_model11_plot_residuals_vs_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(g11, residuals=TRUE)
  dev.off()
  
  tiff("GAM_summer_model11_plot_variables_pluspredictions_sewithmean_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(g11, shade=TRUE, seWithMean=TRUE, scale=0)
  dev.off()
  
  tiff("GAM_summer_model11_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(g11, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()


#Add a smoother to the accessibility variables
gs <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
            country +
            PA +
            s(sqrtroadlength) +
            s(dist2road) +
            s(dist2airports) +
            s(dist2ports) +
            s(dist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model1s_summary_binomial_noocean_smoothedvars.txt")
print(anova(gs))
print(summary(gs))
print(paste0("AIC ", AIC(gs)))
tiff("GAM_summer_model1s_fit_binomial_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs)
dev.off()
sink()

tiff("GAM_summer_model1s_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

#Add a smoother to the logged accessibility variables
gs1 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
            country +
            PA +
            s(sqrtroadlength) +
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places),
          data = gridYearsummermod,  
          family = binomial, cluster=cl)

sink("GAM_summer_model11s_summary_binomial_logvars_smoothedvars.txt")
print(anova(gs1))
print(summary(gs1))
print(paste0("AIC ", AIC(gs1)))
tiff("GAM_summer_model11s_fit_binomial_logvars_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs1)
dev.off()
sink()

tiff("GAM_summer_model11s_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,5))
plot(gs1, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
dev.off()

#increase the k value (adds more kinks in the smooth)  
gsb <- bam(PUD_pa ~ s(Latitude, k=30) + s(Longitude, k=30)+
             country +
             PA +
             dist2road +
             dist2airports +
             dist2ports +
             dist2populated_places +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl) 
  sink("GAM_summer_model1b_summary_binomial_unsmoothedvars_k30.txt")
  print(anova(gsb))
  print(summary(gsb))
  print(paste0("AIC ", AIC(gsb)))
  par(mfrow=c(2,2))
  gam.check(gsb)
  sink() 

gs11b <- bam(PUD_pa ~ s(Latitude, k=30) + s(Longitude, k=30)+
               country +
               PA +
               sqrtroadlength + 
               logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
             data = gridYearsummermod, 
             family = binomial, cluster=cl)
  sink("GAM_summer_model11b_summary_binomial_logvars_k30.txt")
  print(anova(gs11b))
  print(summary(gs11b))
  print(paste0("AIC ", AIC(gs11b)))
  tiff("GAM_summer_model11b_fit_binomial_logvars_k30.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gs11b)
  dev.off()
  sink()

#most parsimonious
gs0 <- bam(PUD_pa ~ sqrtroadlength,
            data = gridYearsummermod,  
            family = binomial, cluster=cl)
  anova(gs0)
  sink("GAM_summer_model0_summary_binomial.txt")
  print(anova(gs0))
  print(summary(gs0))
  print(paste0("AIC ", AIC(gs0)))
  tiff("GAM_summer_model0_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gs0)
  dev.off()
  sink()

#add in the smooths for lat and lon
gs0a <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             #country +
             #PA +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl)
  anova(gs0a)
  sink("GAM_summer_model0a_summary_binomial.txt")
  print(anova(gs0a))
  print(summary(gs0a))
  print(paste0("AIC ", AIC(gs0a)))
  tiff("GAM_summer_model0a_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gs0a)
  dev.off()
  sink()

#quite parsimonious
gs0b <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl)
  anova(gs0b)
  sink("GAM_summer_model0b_summary_binomial.txt")
  print(anova(gs0b))
  print(summary(gs0b))
  print(paste0("AIC ", AIC(gs0b)))
  print(anova(gs0a, gs0b))
  tiff("GAM_summer_model0b_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gs0b)
  dev.off()
  sink()

#Add assessibility variables, logged
gs2 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2road,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model2_summary_binomial.txt")
  print(anova(gs2))
  print(summary(gs2))
  print(paste0("AIC ", AIC(gs2)))
  par(mfrow=c(2,2))
  gam.check(gs2)
  sink()
gs3 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2airports,
           data = gridYearsummermod,
           family = binomial, cluster=cl)
  sink("GAM_summer_model3_summary_binomial.txt")
  print(anova(gs3))
  print(summary(gs3))
  print(paste0("AIC ", AIC(gs3)))
  par(mfrow=c(2,2))
  gam.check(gs3)
  sink()
gs4 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model4_summary_binomial.txt")
  print(anova(gs4))
  print(summary(gs4))
  print(paste0("AIC ", AIC(gs4)))
  par(mfrow=c(2,2))
  gam.check(gs4)
  sink()
gs5 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2ports,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model5_summary_binomial.txt")
  print(anova(gs1, gs2, gs3, gs4, gs5))
  print(anova(gs5))
  print(summary(gs5))
  print(paste0("AIC ", AIC(gs5)))
  par(mfrow=c(2,2))
  gam.check(gs5)
  sink()


#add 2 variables
gs6 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2airports + logdist2road,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model6_summary_binomial.txt")
  print(anova(gs6))
  print(summary(gs6))
  print(paste0("AIC ", AIC(gs6)))
  par(mfrow=c(2,2))
  gam.check(gs6)
  sink()
gs7 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2airports + logdist2ports,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model7_summary_binomial.txt")
  print(anova(gs7))
  print(summary(gs7))
  print(paste0("AIC ", AIC(gs7)))
  par(mfrow=c(2,2))
  gam.check(gs7)
  sink()
gs8 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength +
             logdist2airports + logdist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model8_summary_binomial.txt")
  print(anova(gs8))
  print(summary(gs8))
  print(paste0("AIC ", AIC(gs8)))
  par(mfrow=c(2,2))
  gam.check(gs8)
  sink()
gs9 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
             country +
             PA +
             sqrtroadlength + 
             logdist2ports + logdist2populated_places,
           data = gridYearsummermod, 
           family = binomial, cluster=cl)
  sink("GAM_summer_model9_summary_binomial_noocean.txt")
  print(anova(gs1, gs6, gs7, gs8, gs9))
  print(anova(gs9))
  print(summary(gs9))
  print(paste0("AIC ", AIC(gs9)))
  par(mfrow=c(2,2))
  gam.check(gs9)
  sink()
  
gs10 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength + 
               logdist2road + logdist2populated_places,
             data = gridYearsummermod, 
             family = binomial, cluster=cl)
  sink("GAM_summer_model10_summary_binomial.txt")
  print(anova(gs10))
  print(summary(gs10))
  print(paste0("AIC ", AIC(gs10)))
  par(mfrow=c(2,2))
  gam.check(gs10)
  sink()
  
#drop variable from model with logged accessibility variables
gs12 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                #sqrtroadlength + 
                logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model12_summary_binomial_logvars.txt")
  print(anova(gs12))
  print(summary(gs12))
  print(paste0("AIC ", AIC(gs12)))
  par(mfrow=c(2,2))
  gam.check(gs12)
  sink()
gs13 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength + 
                logdist2road + logdist2airports + logdist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model13_summary_binomial_logvars.txt")
  print(anova(gs13))
  print(summary(gs13))
  print(paste0("AIC ", AIC(gs13)))
  par(mfrow=c(2,2))
  gam.check(gs13)
  sink()
gs15 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength + 
                logdist2airports + logdist2ports + logdist2populated_places,
              data = gridYearsummermod, 
              family = binomial, cluster=cl)
  sink("GAM_summer_model15_summary_binomial_logvars.txt")
  print(anova(gs15))
  print(summary(gs15))
  print(paste0("AIC ", AIC(gs15)))
  par(mfrow=c(2,2))
  gam.check(gs15)
  sink()


  ###################
  ###WINTER BINOMIAL 
  ###################
  #Model with all the variables
  gw1 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
              country +
              PA +
              sqrtroadlength + 
              dist2road + dist2airports + dist2ports + dist2populated_places,
            data = gridYearwintermod, 
            family = binomial, cluster=cl)
  sink("GAM_winter_model1_summary_binomial_noocean.txt")
  print(anova(gw1))
  print(summary(gw1))
  print(paste0("AIC ", AIC(gw1)))
  tiff("GAM_winter_model1_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw1)
  dev.off()
  sink()
  
  tiff("GAM_winter_model1_plot_residuals_vs_distancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearwintermod$sqrtroadlength,residuals(gw1), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearwintermod$dist2road,residuals(gw1), cex=0.5, pch=20, xlab="dist2road")
  plot(gridYearwintermod$dist2airports,residuals(gw1), cex=0.5, pch=20,  xlab="dist2airports")
  plot(gridYearwintermod$dist2ports,residuals(gw1), cex=0.5, pch=20, xlab="dist2ports")
  plot(gridYearwintermod$dist2populated_places,residuals(gw1), cex=0.5, pch=20, xlab="dist2populated_places")
  dev.off()
  
  tiff("GAM_winter_model1_plot_residuals_vs_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw1, residuals=TRUE)
  dev.off()
  
  tiff("GAM_winter_model1_plot_variables_pluspredictions_sewithmean_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw1, shade=TRUE, seWithMean=TRUE, scale=0)
  dev.off()
  
  tiff("GAM_winter_model1_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(gw1, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()
  
  #Model with all the variables, logged
  gw11 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength + 
               logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model11_summary_binomial_logvars.txt")
  print(anova(gw11))
  print(summary(gw11))
  print(paste0("AIC ", AIC(gw11)))
  tiff("GAM_winter_model11_fit_binomial_logvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw11)
  dev.off()
  sink()
  
  tiff("GAM_winter_model11_plot_residuals_vs_logdistancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearwintermod$sqrtroadlength,residuals(gw11), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearwintermod$logdist2road,residuals(gw11), cex=0.5, pch=20, xlab="logdist2road")
  plot(gridYearwintermod$logdist2airports,residuals(gw11), cex=0.5, pch=20,  xlab="logdist2airports")
  plot(gridYearwintermod$logdist2ports,residuals(gw11), cex=0.5, pch=20, xlab="logdist2ports")
  plot(gridYearwintermod$logdist2populated_places,residuals(gw11), cex=0.5, pch=20, xlab="logdist2populated_places")
  dev.off()
  
  tiff("GAM_winter_model11_plot_residuals_vs_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw11, residuals=TRUE)
  dev.off()
  
  tiff("GAM_winter_model11_plot_variables_pluspredictions_sewithmean_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw11, shade=TRUE, seWithMean=TRUE, scale=0)
  dev.off()
  
  tiff("GAM_winter_model11_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(gw11, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()
  
  
  #Add a smoother to the accessibility variables
  gw1s <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
              country +
              PA +
              s(sqrtroadlength) +
              s(dist2road) +
              s(dist2airports) +
              s(dist2ports) +
              s(dist2populated_places),
            data = gridYearwintermod,  
            family = binomial, cluster=cl)
  
  sink("GAM_winter_model1s_summary_binomial_noocean_smoothedvars.txt")
  print(anova(gw1s))
  print(summary(gw1s))
  print(paste0("AIC ", AIC(gw1s)))
  tiff("GAM_winter_model1s_fit_binomial_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw1s)
  dev.off()
  sink()
  
  #Add a smoother to the logged accessibility variables
  gw11s <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               s(sqrtroadlength) +
               s(logdist2road) +
               s(logdist2airports) +
               s(logdist2ports) +
               s(logdist2populated_places),
             data = gridYearwintermod,  
             family = binomial, cluster=cl)
  
  sink("GAM_winter_model11s_summary_binomial_logvars_smoothedvars.txt")
  print(anova(gw11s))
  print(summary(gw11s))
  print(paste0("AIC ", AIC(gw11s)))
  tiff("GAM_winter_model11s_fit_binomial_logvars_smoothedvars.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw11s)
  dev.off()
  sink()
  
  tiff("GAM_winter_model11s_plot_residuals_vs_logdistancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearwintermod$sqrtroadlength,residuals(gw11s), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearwintermod$logdist2road,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2road")
  plot(gridYearwintermod$logdist2airports,residuals(gw11s), cex=0.5, pch=20,  xlab="logdist2airports")
  plot(gridYearwintermod$logdist2ports,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2ports")
  plot(gridYearwintermod$logdist2populated_places,residuals(gw11s), cex=0.5, pch=20, xlab="logdist2populated_places")
  dev.off()
  
  tiff("GAM_winter_model11s_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(gw11s, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()
  
  
  #increase the k value (adds more kinks in the smooth)  
  gw1b <- bam(PUD_pa ~ s(Latitude, k=30) + s(Longitude, k=30)+
               country +
               PA +
               dist2road +
               dist2airports +
               dist2ports +
               dist2populated_places +
               sqrtroadlength,
             data = gridYearwintermod,  
             family = binomial, cluster=cl) 
  sink("GAM_winter_model1b_summary_binomial_unsmoothedvars_k30.txt")
  print(anova(gw1b))
  print(summary(gw1b))
  print(paste0("AIC ", AIC(gw1b)))
  par(mfrow=c(2,2))
  gam.check(gw1b)
  sink() 
  
  gw11b <- bam(PUD_pa ~ s(Latitude, k=30) + s(Longitude, k=30)+
                 country +
                 PA +
                 sqrtroadlength + 
                 logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
               data = gridYearwintermod, 
               family = binomial, cluster=cl)
  sink("GAM_winter_model11b_summary_binomial_logvars_k30.txt")
  print(anova(gw11b))
  print(summary(gw11b))
  print(paste0("AIC ", AIC(gw11b)))
  tiff("GAM_winter_model11b_fit_binomial_logvars_k30.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw11b)
  dev.off()
  sink()
  
  gw11c <- bam(PUD_pa ~ s(Latitude, k=20) + s(Longitude, k=20)+
                 country +
                 PA +
                 sqrtroadlength + 
                 logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
               data = gridYearwintermod, 
               family = binomial, cluster=cl)
  sink("GAM_winter_model11c_summary_binomial_logvars_k20.txt")
  print(anova(gw11c))
  print(summary(gw11c))
  print(paste0("AIC ", AIC(gw11c)))
  tiff("GAM_winter_model11c_fit_binomial_logvars_k20.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw11c)
  dev.off()
  sink()
  
  tiff("GAM_winter_model11c_plot_residuals_vs_logdistancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,3))
  plot(gridYearwintermod$sqrtroadlength,residuals(gw11c), cex=0.5, pch=20, xlab="sqrtroadlength")
  plot(gridYearwintermod$logdist2road,residuals(gw11c), cex=0.5, pch=20, xlab="logdist2road")
  plot(gridYearwintermod$logdist2airports,residuals(gw11c), cex=0.5, pch=20,  xlab="logdist2airports")
  plot(gridYearwintermod$logdist2ports,residuals(gw11c), cex=0.5, pch=20, xlab="logdist2ports")
  plot(gridYearwintermod$logdist2populated_places,residuals(gw11c), cex=0.5, pch=20, xlab="logdist2populated_places")
  dev.off()
  
  tiff("GAM_winter_model11c_plot_residuals_vs_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw11c, residuals=TRUE)
  dev.off()
  
  tiff("GAM_winter_model11c_plot_variables_pluspredictions_sewithmean_latlon.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(1,2))
  plot(gw11c, shade=TRUE, seWithMean=TRUE, scale=0)
  dev.off()
  
  tiff("GAM_winter_model11c_plot_variables_pluspredictions_sewithmean_allvars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
  par(mfrow=c(2,5))
  plot(gw11c, shade=TRUE, seWithMean=TRUE, scale=0, all.terms=TRUE)
  dev.off()
  
  
  #most parsimonious
  gw0 <- bam(PUD_pa ~ sqrtroadlength,
             data = gridYearwintermod,  
             family = binomial, cluster=cl)
  anova(gw0)
  sink("GAM_winter_model0_summary_binomial.txt")
  print(anova(gw0))
  print(summary(gw0))
  print(paste0("AIC ", AIC(gw0)))
  tiff("GAM_winter_model0_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw0)
  sink()
  dev.off()

  #add in the smooths for lat and lon
  gw0a <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                #country +
                #PA +
                sqrtroadlength,
              data = gridYearwintermod,  
              family = binomial, cluster=cl)
  anova(gw0a)
  sink("GAM_winter_model0a_summary_binomial.txt")
  print(anova(gw0a))
  print(summary(gw0a))
  print(paste0("AIC ", AIC(gw0a)))
  tiff("GAM_winter_model0a_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw0a)
  dev.off()
  sink()
  
  #quite parsimonious
  gw0b <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength,
              data = gridYearwintermod,  
              family = binomial, cluster=cl)
  anova(gw0b)
  sink("GAM_winter_model0b_summary_binomial.txt")
  print(anova(gw0b))
  print(summary(gw0b))
  print(paste0("AIC ", AIC(gw0b)))
  print(anova(gw0a, gw0b))
  tiff("GAM_winter_model0b_fit_binomial.tiff", width=7, height=7, units='in', res=300, compression="lzw")
  par(mfrow=c(2,2))
  gam.check(gw0b)
  sink()
  dev.off()
  
#Add logged assessibility variables
gw2 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2road,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model2_summary_binomial.txt")
  print(anova(gw2))
  print(summary(gw2))
  print(paste0("AIC ", AIC(gw2)))
  par(mfrow=c(2,2))
  gam.check(gw2)
  sink()
gw3 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2airports,
             data = gridYearwintermod,
             family = binomial, cluster=cl)
  sink("GAM_winter_model3_summary_binomial.txt")
  print(anova(gw3))
  print(summary(gw3))
  print(paste0("AIC ", AIC(gw3)))
  par(mfrow=c(2,2))
  gam.check(gw3)
  sink()
gw4 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2populated_places,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model4_summary_binomial.txt")
  print(anova(gw4))
  print(summary(gw4))
  print(paste0("AIC ", AIC(gw4)))
  par(mfrow=c(2,2))
  gam.check(gw4)
  sink()
gw5 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2ports,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model5_summary_binomial.txt")
  print(anova(gw1, gw2, gw3, gw4, gw5))
  print(anova(gw5))
  print(summary(gw5))
  print(paste0("AIC ", AIC(gw5)))
  par(mfrow=c(2,2))
  gam.check(gw5)
  sink()
 
#add 2 variables
gw6 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2airports + logdist2road,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model6_summary_binomial.txt")
  print(anova(gw6))
  print(summary(gw6))
  print(paste0("AIC ", AIC(gw6)))
  par(mfrow=c(2,2))
  gam.check(gw6)
  sink()
gw7 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2airports + logdist2ports,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model7_summary_binomial.txt")
  print(anova(gw7))
  print(summary(gw7))
  print(paste0("AIC ", AIC(gw7)))
  par(mfrow=c(2,2))
  gam.check(gw7)
  sink()
 gw8 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength +
               logdist2airports + logdist2populated_places,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model8_summary_binomial.txt")
  print(anova(gw8))
  print(summary(gw8))
  print(paste0("AIC ", AIC(gw8)))
  par(mfrow=c(2,2))
  gam.check(gw8)
  sink()
gw9 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
               country +
               PA +
               sqrtroadlength + 
               logdist2ports + logdist2populated_places,
             data = gridYearwintermod, 
             family = binomial, cluster=cl)
  sink("GAM_winter_model9_summary_binomial_noocean.txt")
  print(anova(gw1, gw6, gw7, gw8, gw9))
  print(anova(gw9))
  print(summary(gw9))
  print(paste0("AIC ", AIC(gw9)))
  par(mfrow=c(2,2))
  gam.check(gw9)
  sink()
  
 gw10 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength + 
                logdist2road + logdist2populated_places,
              data = gridYearwintermod, 
              family = binomial, cluster=cl)
  sink("GAM_winter_model10_summary_binomial.txt")
  print(anova(gw10))
  print(summary(gw10))
  print(paste0("AIC ", AIC(gw10)))
  par(mfrow=c(2,2))
  gam.check(gw10)
  sink()
  
  #drop variables from model with logged accessibility variables
 gw12 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                #sqrtroadlength + 
                logdist2road + logdist2airports + logdist2ports + logdist2populated_places,
              data = gridYearwintermod, 
              family = binomial, cluster=cl)
  sink("GAM_winter_model12_summary_binomial_logvars.txt")
  print(anova(gw12))
  print(summary(gw12))
  print(paste0("AIC ", AIC(gw12)))
  par(mfrow=c(2,2))
  gam.check(gw12)
  sink()
 gw13 <- bam(PUD_pa ~ s(Latitude) + s(Longitude)+
                country +
                PA +
                sqrtroadlength + 
                logdist2road + logdist2airports + logdist2populated_places,
              data = gridYearwintermod, 
              family = binomial, cluster=cl)
  sink("GAM_winter_model13_summary_binomial_logvars.txt")
  print(anova(gw13))
  print(summary(gw13))
  print(paste0("AIC ", AIC(gw13)))
  par(mfrow=c(2,2))
  gam.check(gw13)
  sink()
 

###################
#MODELS without PA
###################

gw14 <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
             country +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearwintermod,  
           family = binomial, cluster=cl) 
sink("GAM_winter_model14_summary_binomial_logvars_noPA.txt")
print(anova(gw14))
print(summary(gw14))
print(paste0("AIC ", AIC(gw14)))
tiff("GAM_winter_model14_fit_binomial_logvarsnoPA.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw14)
dev.off()
sink() 


tiff("GAM_winter_model14_plot_residuals_vs_logdistancevars.tiff", width=14, height=7, units='in', res=400, compression="lzw")
par(mfrow=c(2,3))
plot(gridYearwintermod$sqrtroadlength,residuals(gw14), cex=0.5, pch=20, xlab="sqrtroadlength")
plot(gridYearwintermod$logdist2road,residuals(gw14), cex=0.5, pch=20, xlab="logdist2road")
plot(gridYearwintermod$logdist2airports,residuals(gw14), cex=0.5, pch=20,  xlab="logdist2airports")
plot(gridYearwintermod$logdist2ports,residuals(gw14), cex=0.5, pch=20, xlab="logdist2ports")
plot(gridYearwintermod$logdist2populated_places,residuals(gw14), cex=0.5, pch=20, xlab="logdist2populated_places")
dev.off()

gs14 <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
             country +
             logdist2road +
             logdist2airports +
             logdist2ports +
             logdist2populated_places +
             sqrtroadlength,
           data = gridYearsummermod,  
           family = binomial, cluster=cl) 
sink("GAM_summer_model14_summary_binomial_logvars_noPA.txt")
print(anova(gs14))
print(summary(gs14))
print(paste0("AIC ", AIC(gs14)))
tiff("GAM_summer_model14_fit_binomial_logvars_noPA.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gs14)
dev.off()
sink() 


###################
#MODELS without PA and ports
###################

gw15 <- bam(PUD_pa ~ s(Latitude) + s(Longitude) +
              country +
              logdist2road +
              logdist2airports +
              logdist2populated_places +
              sqrtroadlength,
            data = gridYearwintermod,  
            family = binomial, cluster=cl) 
sink("GAM_winter_model15_summary_binomial_logvars_noPA.txt")
print(anova(gw15))
print(summary(gw15))
print(paste0("AIC ", AIC(gw15)))
tiff("GAM_winter_model15_fit_binomial_logvarsnoPAnoports.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw15)
dev.off()
sink() 

###################
#MODELS with k20
###################
gw13c <- bam(PUD_pa ~ s(Latitude, k=20) + s(Longitude, k=20)+
              country +
              PA +
              sqrtroadlength + 
              logdist2road + logdist2airports + logdist2populated_places,
            data = gridYearwintermod, 
            family = binomial, cluster=cl)
sink("GAM_winter_model13c_summary_binomial_logvars_k20.txt")
print(anova(gw13c))
print(summary(gw13c))
print(paste0("AIC ", AIC(gw13c)))
par(mfrow=c(2,2))
gam.check(gw13c)
sink()

gw14c <- bam(PUD_pa ~ s(Latitude, k=20) + s(Longitude, k=20) +
              country +
              logdist2road +
              logdist2airports +
              logdist2ports +
              logdist2populated_places +
              sqrtroadlength,
            data = gridYearwintermod,  
            family = binomial, cluster=cl) 
sink("GAM_winter_model14c_summary_binomial_logvars_noPA_k20.txt")
print(anova(gw14c))
print(summary(gw14c))
print(paste0("AIC ", AIC(gw14c)))
tiff("GAM_winter_model14c_fit_binomial_logvarsnoPA_k20.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw14c)
dev.off()
sink() 

gw15c <- bam(PUD_pa ~ s(Latitude, k=20) + s(Longitude, k=20) +
              country +
              logdist2road +
              logdist2airports +
              logdist2populated_places +
              sqrtroadlength,
            data = gridYearwintermod,  
            family = binomial, cluster=cl) 
sink("GAM_winter_model15c_summary_binomial_logvars_noPA_k20.txt")
print(anova(gw15c))
print(summary(gw15c))
print(paste0("AIC ", AIC(gw15c)))
tiff("GAM_winter_model15c_fit_binomial_logvarsnoPAnoports_k20.tiff", width=7, height=7, units='in', res=300, compression="lzw")
par(mfrow=c(2,2))
gam.check(gw15c)
dev.off()
sink() 

##########################
#Compare models
sink("GAM_winter_model_comparison.txt")
print(anova(gw11, gw13, test="Chisq"))
print(anova(gw11, gw14, test="Chisq"))
print(anova(gw11, gw15, test="Chisq"))
print(anova(gw14, gw15, test="Chisq"))
sink() 

sink("GAM_summer_model_comparison.txt")
print(anova(g11, gs13, test="Chisq"))
print(anova(g11, gs14, test="Chisq"))
print(anova(g11, gs15, test="Chisq"))
sink() 

###END
if (!is.null(cl)) stopCluster(cl)
