####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <05-generate-indices.R>
# Created December 2023
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp <- "WOTH"

####### Read Data #################################

route <- readRDS(paste0("output/model_runs/", sp, "-route.rds"))
point <- readRDS(paste0("output/model_runs/", sp, "-point.rds"))
detectability <- readRDS(paste0("output/model_runs/", sp, "-detectability.rds"))

####### Calculate Indices and Output ##############

indices_route <- generate_indices(model_output = route)
saveRDS(object = indices_route,
        file = paste0("output/indices/", sp, "_route.RDS"))

indices_point <- generate_indices(model_output = point)
saveRDS(object = indices_point,
        file = paste0("output/indices/", sp, "_point.RDS"))

indices_detectability <- generate_indices(model_output = detectability)
saveRDS(object = indices_detectability,
        file = paste0("output/indices/", sp, "_detectability.RDS"))
