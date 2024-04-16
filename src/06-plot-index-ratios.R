####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <06-plot-index-ratios.R>
# Created March 2024
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

route_indices <- readRDS(file = "output/indices/route.RDS")
point_indices <- readRDS(file = "output/indices/point.RDS")
detect_indices <- readRDS(file = "output/indices/detectability.RDS")

####### Main Code #################################

route_cont <- indices_route$indices[which(indices_route$indices$region == "continent"), ]
point_cont <- indices_point$indices[which(indices_point$indices$region == "continent"), ]
detect_cont <- indices_detectability$indices[which(indices_detectability$indices$region == "continent"),]

ratio_indices <- (route_cont$index / point_cont$index) /50
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Route Index / Point Index")
abline(h = 1)

ratio_indices <- (route_cont$index / detect_cont$index) / 50
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Route Index / Detectability Index")
abline(h = 1)

ratio_indices <- (point_cont$index / detect_cont$index) 
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Point Index / Detectability Index")
abline(h = 1)