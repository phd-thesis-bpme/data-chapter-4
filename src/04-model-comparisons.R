####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <04-model-comparisons.R>
# Created December 2023
# Last Updated December 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Read Data #################################

route <- readRDS("data/generated/model_runs/Ovenbird-route.RDS")
point <- readRDS("data/generated/model_runs/Ovenbird-point.rds")

####### Indices and Trends ########################

indices_route <- generate_indices(model_output = route)
trends_route <- generate_trends(indices = indices_route)
(trend_map_route <- plot_map(trends_route))

indices_point <- generate_indices(model_output = point)
trends_point <- generate_trends(indices = indices_point)
(trend_map_point <- plot_map(trends_point))
