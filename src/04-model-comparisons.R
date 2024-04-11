####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <04-model-comparisons.R>
# Created December 2023
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp <- "OVEN"

####### Main Code #################################
route <- readRDS(paste0("output/model_runs/", sp, "-route.rds"))
point <- readRDS(paste0("output/model_runs/", sp, "-point.rds"))
detectability <- readRDS(paste0("output/model_runs/", sp, "-detectability.rds"))

indices_route <- generate_indices(model_output = route)
trends_route <- generate_trends(indices = indices_route)
trend_map_route <- plot_map(trends_route, title = FALSE)
indices_plot_route <- plot_indices(indices = indices_route, title = FALSE, 
                                   axis_text_size = 12, axis_title_size = 12)

indices_point <- generate_indices(model_output = point)
trends_point <- generate_trends(indices = indices_point)
trend_map_point <- plot_map(trends_point, title = FALSE)
indices_plot_point <- plot_indices(indices = indices_point, title = FALSE, 
                                   axis_text_size = 12, axis_title_size = 12)

indices_detectability <- generate_indices(model_output = detectability)
trends_detectability <- generate_trends(indices = indices_detectability)
trend_map_detectability <- plot_map(trends_detectability, title = FALSE)
indices_plot_detectability <- plot_indices(indices = indices_detectability, title = FALSE, 
                                           axis_text_size = 12, axis_title_size = 12)

route_cont <- indices_route$indices[which(indices_route$indices$region == "continent"), ]
point_cont <- indices_point$indices[which(indices_point$indices$region == "continent"), ]
detect_cont <- indices_detectability$indices[which(indices_detectability$indices$region == "continent"),]

# png(filename = paste0("output/plots/trends-and-trajectories.png"),
#     width = 6, height = 4, units = "in", res = 600)
# ggarrange(trend_map_route, trend_map_point, trend_map_detectability,
#           indices_plot_route$continent, indices_plot_point$continent, 
#           indices_plot_detectability$continent,
#           nrow = 2, ncol = 3,
#           common.legend = TRUE)
# dev.off()

png(filename = paste0("output/plots/", sp, "-map.png"),
    width = 20, height = 6, units = "in", res = 300)
ggarrange(trend_map_route, trend_map_point, trend_map_detectability, nrow = 1,
          labels = c("Route", "Point", "Detectability"))
dev.off()

png(filename = paste0("output/plots/", sp, "-trajectory.png"),
    width = 20, height = 6, units = "in", res = 300)
ggarrange(indices_plot_route$continent, indices_plot_point$continent,
          indices_plot_detectability$continent, nrow = 1,
          labels = c("Route", "Point", "Detectability"))
dev.off()
