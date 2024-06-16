####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# 09-varprop-diagnostics.R
# Created May 2024
# Last Updated June 2024

####### Import Libraries and External Files #######

library(cmdstanr)
library(bayesplot)
library(ggpubr)
theme_set(theme_pubclean())
bayesplot::color_scheme_set("red")

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

varprop_model <- readRDS(paste0("output/model_runs/", sp, "-varprop.rds"))

####### Main Code #################################

avail_plot <- bayesplot::mcmc_areas(varprop_model$model_fit$draws("zeta")) 

percept_plot <- bayesplot::mcmc_areas(varprop_model$model_fit$draws("xi")) +
  scale_y_discrete(labels = c("Intercept","Forest","Road","RoadForest"))

####### Output ####################################

png(filename = paste0("output/plots/", sp, "-varprop-percept.png"),
    width = 6, height = 4, res = 300, units = "in")
print(percept_plot)
dev.off()
