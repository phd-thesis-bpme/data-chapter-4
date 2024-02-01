####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <04-model-comparisons.R>
# Created December 2023
# Last Updated December 2023

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Read Data #################################

route <- readRDS("data/generated/model_runs/Ovenbird-route.rds")
point <- readRDS("data/generated/model_runs/Ovenbird-point.rds")

####### Indices and Trends ########################

indices_route <- generate_indices(model_output = route)
trends_route <- generate_trends(indices = indices_route)
(trend_map_route <- plot_map(trends_route))
indices_plot_route <- plot_indices(indices = indices_route)

indices_point <- generate_indices(model_output = point)
trends_point <- generate_trends(indices = indices_point)
(trend_map_point <- plot_map(trends_point))
indices_plot_point <- plot_indices(indices = indices_point)

####### Output ####################################

png(filename = "output/plots/oven-map.png",
    width = 10, height = 6, units = "in", res = 300)
ggarrange(trend_map_route, trend_map_point, nrow = 1,
          labels = c("Route", "Point"))
dev.off()

png(filename = "output/plots/oven-trajectory.png",
    width = 10, height = 6, units = "in", res = 300)
ggarrange(indices_plot_route$continent, indices_plot_point$continent, nrow = 1,
          labels = c("Route", "Point"))
dev.off()
