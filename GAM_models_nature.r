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

gridYearPUD_models <- readRDS("gridYearPUD_models10000_m_bioticnature.Rdata")

outdir <- "gam_10km_binomial_noyear_nature" ##EDIT ME

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
setwd(paste(getwd(), outdir, sep="/"))

#set up summer and winter datasets
gridYearsummermod <- gridYearPUD_models %>% 
  filter(season=="summer") %>% 
  mutate(PUD_pa = if_else(PUD>0, 1, 0) ) #reclassify as 1,0
gridYearwintermod <- gridYearPUD_models %>% 
  filter(season=="winter") %>% 
  mutate(PUD_pa = if_else(PUD>0, 1, 0) ) #reclassify as 1,0

table(gridYearsummermod$PUD_pa)
table(gridYearwintermod$PUD_pa)

###set up cluster
require(parallel)  
nc <- 8   ## cluster size
if (detectCores()>1) { 
  cl <- makeCluster(nc) 
} else cl <- NULL

###################
###SUMMER BINOMIAL 
###################
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
            s(logdist2road) +
            s(logdist2airports) +
            s(logdist2ports) +
            s(logdist2populated_places),
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

  ###################
  ###WINTER BINOMIAL 
  ###################
 
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

##########################
#Compare models
sink("GAM_nature_model_comparison.txt")
print(anova(gw11, gw14, test="Chisq"))
print(anova(g11, gs14, test="Chisq"))
sink() 

###END
if (!is.null(cl)) stopCluster(cl)
