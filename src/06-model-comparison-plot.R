####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <06-model-comparison-plot.R>
# Created April 2024
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp_list <- c("WOTH", "PIWO")

####### Main Code #################################

for (sp in sp_list)
{
  route_indices <- readRDS(file = paste0("output/indices/", sp, "_route.RDS"))
  point_indices <- readRDS(file = paste0("output/indices/", sp, "_point.RDS"))
  detect_indices <- readRDS(file = paste0("output/indices/", sp, "_detectability.RDS"))
  
  route_trends <- generate_trends(indices = route_indices)
  point_trends <- generate_trends(indices = point_indices)
  detectability_trends <- generate_trends(indices = detect_indices)
  
  indices_plot_route <- plot_indices(indices = route_indices, title = FALSE, 
                                     axis_text_size = 12, axis_title_size = 12)
  
  indices_plot_point <- plot_indices(indices = point_indices, title = FALSE, 
                                     axis_text_size = 12, axis_title_size = 12)
  
  indices_plot_detectability <- plot_indices(indices = detect_indices, title = FALSE, 
                                             axis_text_size = 12, axis_title_size = 12)
  
  trend_map_route <- plot_map(route_trends, title = FALSE)
  trend_map_point <- plot_map(point_trends, title = FALSE)
  trend_map_detectability <- plot_map(detectability_trends, title = FALSE)
  
  ####### Output ####################################
  
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
}
