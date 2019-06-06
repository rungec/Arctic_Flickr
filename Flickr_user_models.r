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


#wd <- "D:/Box Sync/Arctic/Data"
#setwd(wd)
wd <- paste0(getwd(), "/Documents")
setwd(wd)
# flickr data
#load("D:/Box Sync/Arctic/Data/Flickr/processed/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")
load("flickr/Flickr_Artic_60N_googlelabels_escodes_amap_plusaccessibility.Rdata")


#Then we run a model 
#wildlife_photo_prop ~ lm(accessibility + PAs + random(owner))

g1 <- lmer(logdist2road~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub)


sink("Model_of_dist2road_bysocioecon_lme.txt")
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
write.csv(x, "Model_of_dist2road_bysocioecon_lme_bestmod_coefs.csv", row.names=FALSE)
x <-as.data.frame.table(summary(g1)$coefficients) %>% spread(key = Var2, value = Freq)
write.csv(x, "Model_of_dist2road_bysocioecon_lme_fullmod_coefs.csv", row.names=FALSE)

# rsquared() is recommended function to use for rsquared calculations of mixed models in library piecewiseSEM, gives both the variance explained by the fixed effect alone and 
#the fixed and random component together - the whole model https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5970551/ 
#exp( ({AICc(g6)-AICc(g5)}/2) )
#e.g. model g6 is 0.0364 times as probable as mod g5 to minimise the information loss. Cant use to compare models run with REML

#plot the model diagnostics, bestmod
#plot diagnostic plots for lmer For linear (mixed) models, plots for multicollinearity-check (Variance In???ation Factors), 
#QQ-plots, checks for normal distribution of residuals and homoscedasticity (constant variance of residuals) are shown https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf.
p<-plot_model(g13, type = "diag")
pout <- ggarrange(p[[1]], p[[2]]$LogID, p[[3]], p[[4]], ncol=2, nrow=2, labels = c("A", "B", "C", "D"))
ggsave("Model_of_dist2road_bysocioecon_lme_diag_bestmod.png", pout, width=9, height=7, units="in", dpi=300)

#plot the model diagnostics, fullmod
p<-plot_model(g1, type = "diag")
pout <- ggarrange(p[[1]], p[[2]]$LogID, p[[3]], p[[4]], ncol=2, nrow=2, labels = c("A", "B", "C", "D"))
ggsave("Model_of_dist2road_bysocioecon_lme_diag_fullmod.png", pout, width=9, height=7, units="in", dpi=300)

# plot marginal effects 
p1 <- plot_model(g1, type = "pred", terms = "education")
p2 <- plot_model(g1, type = "pred", terms = "age")
p3 <- plot_model(g1, type = "pred", terms = "gender")
p4 <- plot_model(g1, type = "pred", terms = "income")
p5 <- plot_model(g1, type = "pred", terms = "activity")

pout <- ggarrange(p5, ggarrange(p1, p2, p3, p4, ncol = 2, nrow=2, labels = c("B", "C", "D", "E")), # Second row with box and dot plots
                  nrow = 2, labels = "A" )   
ggsave("Model_of_dist2road_bysocioecon_lme_marginaleffects_fullmod.png", pout, width=7, height=9, units="in", dpi=300)

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

