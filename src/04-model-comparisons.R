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
detectability <- readRDS("data/generated/model_runs/Ovenbird-detectability.rds")

####### Indices and Trends ########################

indices_route <- generate_indices(model_output = route)
trends_route <- generate_trends(indices = indices_route)
(trend_map_route <- plot_map(trends_route))
indices_plot_route <- plot_indices(indices = indices_route)

indices_point <- generate_indices(model_output = point)
trends_point <- generate_trends(indices = indices_point)
(trend_map_point <- plot_map(trends_point))
indices_plot_point <- plot_indices(indices = indices_point)

indices_detectability <- generate_indices(model_output = detectability)
trends_detectability <- generate_trends(indices = indices_detectability)
(trend_map_detectability <- plot_map(trends_detectability))
indices_plot_detectability <- plot_indices(indices = indices_detectability)

# Generate a new list of ggarranged trajectory plots comparing all three


####### Output ####################################

png(filename = "output/plots/oven-map.png",
    width = 10, height = 6, units = "in", res = 300)
ggarrange(trend_map_route, trend_map_point, trend_map_detectability, nrow = 1,
          labels = c("Route", "Point", "Detectability"))
dev.off()

png(filename = "output/plots/oven-trajectory.png",
    width = 10, height = 6, units = "in", res = 300)
ggarrange(indices_plot_route$continent, indices_plot_point$continent, 
          indices_plot_detectability$continent, nrow = 1,
          labels = c("Route", "Point", "Detectability"))
dev.off()
