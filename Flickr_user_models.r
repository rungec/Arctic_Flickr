#Plot and model the accessibility (distance to road etc) for different users

#SETUP ----
require(sf)
require(tidyverse)
require(ggplot2)
require(modelr) #add_predictions
require(lme4) #lmer
require(DHARMa)
require(piecewiseSEM)
require(sjPlot) #lme plots and diagnostics
require(ggpubr) #ggarrange


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/model_and_footprint/model/lme_user_wildlifephotoprop"
#wd <- paste0(getwd(), "/Documents")
setwd(wd)
# flickr data
load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")
#load("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")

logitTransform <- function(p) { log(p/(1-p)) } #log of the odds ratio

#####################
###SET UP DATA
#####################
accessdf <- flickraccess %>% st_set_geometry(NULL) 
rm(flickraccess)

#convert units class to numeric
accessdf$dist2road <- units::drop_units(accessdf$dist2road)
accessdf$dist2airports <- units::drop_units(accessdf$dist2airports)
accessdf$dist2ports <- units::drop_units(accessdf$dist2ports)
accessdf$dist2populated_places <- units::drop_units(accessdf$dist2populated_places)
accessdf$dist2urban_areas <- units::drop_units(accessdf$dist2urban_areas)
accessdf$dist2PA <- units::drop_units(accessdf$dist2PA)

accessdf <- accessdf %>% filter(!NE_country %in% c("RUS", "Ocean")) %>%
                        mutate(logitwildphotoprop = logitTransform(wildlife_photo_prop),
                                logdist2road = log(dist2road+1),
                                logdist2airports = log(dist2airports+1),
                                logdist2ports = log(dist2ports+1),
                                logdist2populated_places = log(dist2populated_places+1),
                                logdist2PA = log(dist2PA+1))

#####################
###PLOT DATA
#####################


#check the covariance of the continous variables
access_cor <- cor(accessdf[, c("dist2road", "dist2airports", "dist2ports", "dist2populated_places", "dist2urban_areas", "dist2PA")], method="pearson")
png("Variable_correlation_plot.png", width=7, height=7, units="in", res=150)
corrplot::corrplot.mixed(access_cor, lower.col="black")
dev.off()
#We drop dist2urban_areas as it is highly correlated with dist2road


#do they need to be transformed?
#how far to infrastructure
png("Variable_dist2infrastructure_plot.png", width=7, height=6, units="in", res=150)
par(mfrow=c(2,3))
hist(accessdf$dist2road/1000, breaks=seq(0,2400,0.1), xlim=c(0,5), main="", xlab="Distance to road (km)")
hist(accessdf$dist2airports/1000, breaks=seq(0,10000,10), xlim=c(0,500), main="", xlab="Distance to airport (km)")
hist(accessdf$dist2ports/1000, breaks=seq(0,10000,10), xlim=c(0,500), main="", xlab="Distance to port (km)")
hist(accessdf$dist2populated_places/1000, breaks=seq(0,10000,10), xlim=c(0,500), main="", xlab="Distance to populated place (km)")
hist(accessdf$dist2urban_areas/1000, breaks=seq(0,10000,10), xlim=c(0,500), main="", xlab="Distance to urban area (km)")
hist(accessdf$dist2PA/1000, breaks=seq(0,10000,10), xlim=c(0,500), main="", xlab="Distance to protected area (m)")
dev.off()

#make some plots of the distance to road versus wildlife_photo_prop
p <- ggplot(accessdf, aes(logdist2road, logitwildphotoprop)) +
  geom_point() +
  xlab("Log distance to road (m)") +
  ylab("Logit photos taken of wildlife per user") +
  theme_minimal()
ggsave("Variable_logitwildlife_photo_vs_logdist2road.png", p)
p <- ggplot(accessdf, aes(dist2road, logitwildphotoprop)) +
  geom_point() +
  xlab("Distance to road (m)") +
  xlim(0,20000) +
  ylab("Logit photos taken of wildlife per user") +
  theme_minimal()
ggsave("Variable_logitwildlife_photo_vs_dist2road.png", p)
p <- ggplot(accessdf, aes(dist2road, wildlife_photo_prop)) +
  geom_point() +
  xlim(0,5000) +
  xlab("Distance to road (m)") +
  ylab("Proportion of photos taken of wildlife") +
  theme_minimal()
ggsave("Variable_wildlife_photo_prop_vs_dist2road.png", p)

#####################
###RUN MODELS
#####################

#Full model
g1 <- lmer(wildlife_photo_prop ~ 
             dist2road + dist2airports + dist2ports + dist2populated_places + dist2PA +
             (1|owner), data = accessdf)
#Full model, log transformed
g1b <- lmer(logitwildphotoprop ~ 
             logdist2road + logdist2airports + logdist2ports + logdist2populated_places + logdist2PA +
             (1|owner), data = accessdf)
#Full model, logit y
g1c <- lmer(logitwildphotoprop ~ 
              dist2road + dist2airports + dist2ports + dist2populated_places + dist2PA +
             (1|owner), data = accessdf)

plot_model(g1)
plot_model(g1b)
plot_model(g1c)
summary(g1)
summary(g1b)
summary(g1c)

g2 <- lmer(wildlife_photo_prop ~ dist2road + dist2airports + dist2populated_places + dist2urban_areas + dist2PA + (1|owner), data = accessdf)
g3 <- lmer(wildlife_photo_prop ~ dist2road + dist2airports + dist2ports + dist2urban_areas + dist2PA + (1|owner), data = accessdf)
g4 <- lmer(wildlife_photo_prop ~ dist2road + dist2airports + dist2ports + dist2populated_places + dist2PA + (1|owner), data = accessdf)
g5 <- lmer(wildlife_photo_prop ~ dist2road + dist2airports + dist2ports + dist2populated_places + dist2urban_areas + (1|owner), data = accessdf)
g6 <- lmer(wildlife_photo_prop ~ dist2road + dist2ports + dist2populated_places + dist2urban_areas + dist2PA + (1|owner), data = accessdf)

g7 <- lmer(wildlife_photo_prop ~ dist2road + dist2airports + dist2ports + dist2populated_places + dist2urban_areas + dist2PA + (1|owner), data = accessdf)


sink("Model_of_wildlifephotoprop_byaccess_lme.txt")
print("Estimate the models by ML and find the most parsimonious")
print(anova(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g12b, g12c, g13, g14))
print("Full model, estimated by REML")
print(summary(g1))
print(car::Anova(g1)) #p values for the fixed effects
print(" ")
print("Most parsimonious model, estimated by REML")
print(summary(g13))
print(car::Anova(g13)) 
sink()

x <-as.data.frame.table(summary(g13)$coefficients) %>% spread(key = Var2, value = Freq)
write.csv(x, "Model_of_wildlifephotoprop_byaccess_lme_bestmod_coefs.csv", row.names=FALSE)
x <-as.data.frame.table(summary(g1)$coefficients) %>% spread(key = Var2, value = Freq)
write.csv(x, "Model_of_wildlifephotoprop_byaccess_lme_fullmod_coefs.csv", row.names=FALSE)

# rsquared() is recommended function to use for rsquared calculations of mixed models in library piecewiseSEM, gives both the variance explained by the fixed effect alone and 
#the fixed and random component together - the whole model https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5970551/ 
#exp( ({AICc(g6)-AICc(g5)}/2) )
#e.g. model g6 is 0.0364 times as probable as mod g5 to minimise the information loss. Cant use to compare models run with REML

#plot the model diagnostics, bestmod
#plot diagnostic plots for lmer For linear (mixed) models, plots for multicollinearity-check (Variance In???ation Factors), 
#QQ-plots, checks for normal distribution of residuals and homoscedasticity (constant variance of residuals) are shown https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf.
p<-plot_model(g13, type = "diag")
pout <- ggarrange(p[[1]], p[[2]]$owner, p[[3]], p[[4]], ncol=2, nrow=2, labels = c("A", "B", "C", "D"))
ggsave("Model_of_wildlifephotoprop_byaccess_lme_diag_bestmod.png", pout, width=9, height=7, units="in", dpi=300)

#plot the model diagnostics, fullmod
p<-plot_model(g1, type = "diag")
pout <- ggarrange(p[[1]], p[[2]]$owner, p[[3]], p[[4]], ncol=2, nrow=2, labels = c("A", "B", "C", "D"))
ggsave("Model_of_wildlifephotoprop_byaccess_lme_diag_fullmod.png", pout, width=9, height=7, units="in", dpi=300)

# plot marginal effects 
p1 <- plot_model(g1, type = "pred", terms = "dist2road")
p2 <- plot_model(g1, type = "pred", terms = "dist2airports")
p3 <- plot_model(g1, type = "pred", terms = "dist2ports")
p4 <- plot_model(g1, type = "pred", terms = "dist2populated_places")
p5 <- plot_model(g1, type = "pred", terms = "dist2urban_areas")
p6 <- plot_model(g1, type = "pred", terms = "dist2PA")

pout <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow=2, labels = c("A", "B", "C", "D", "E", "F"))
  
ggsave("Model_of_wildlifephotoprop_byaccess_lme_marginaleffects_fullmod.png", pout, width=7, height=9, units="in", dpi=300)

#plot residuals assesses how well the predicted and the observed values fit across predictors. The actual (observed) values have a coloured ???ll, while the predicted values have a solid outline without ???lling.
#https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf
#plot_residuals(g1)

# plot random effects 
#plot_model(g1, type = "re")

# #plot the predictions from the two models
# ppgis_sub_preds <- ppgis_sub %>% 
#   add_predictions(g1) %>% 
#   add_residuals(g1) %>%
#   mutate(pred_modfull=pred,
#          resid_modfull=resid) %>%
#   add_predictions(g13) %>% 
#   add_residuals(g13) %>%
#   mutate(pred_bestmod=pred,
#          resid_bestmod=resid)
# 
# ggplot(ppgis_sub_preds, aes(y=exp(pred_modfull)/1000, x=activity))+
#   geom_boxplot(aes(col=gender)) + 
#   ylab("Mean distance to road (km)") + xlab("") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("Model_of_dist2road_bysocioecon_lme_fullmodel.png", width=7.77, height=4.34, units="in")
# ggplot(ppgis_sub_preds, aes(y=exp(pred_parsmod)/1000, x=activity))+
#   geom_boxplot(aes(col=gender)) + 
#   ylab("Mean distance to road (km)") + xlab("") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("Model_of_dist2road_bysocioecon_lme_parsmodel.png", width=7.77, height=4.34, units="in")

