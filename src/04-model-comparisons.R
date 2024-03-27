####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <04-model-comparisons.R>
# Created December 2023
# Last Updated March 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp <- "OVEN"#sp_list <- c("OVEN", "SWTH", "AMCR", "BHVI", "EAPH", "TEWA", "YEWA")

####### Main Code #################################

#for (sp in sp_list)
#{
  route <- readRDS(paste0("data/generated/model_runs/", sp, "-route.rds"))
  point <- readRDS(paste0("data/generated/model_runs/", sp, "-point.rds"))
  detectability <- readRDS(paste0("data/generated/model_runs/", sp, "-detectability.rds"))
  
  indices_route <- generate_indices(model_output = route)
  trends_route <- generate_trends(indices = indices_route)
  trend_map_route <- plot_map(trends_route)
  indices_plot_route <- plot_indices(indices = indices_route)
  
  indices_point <- generate_indices(model_output = point)
  trends_point <- generate_trends(indices = indices_point)
  trend_map_point <- plot_map(trends_point)
  indices_plot_point <- plot_indices(indices = indices_point)
  
  indices_detectability <- generate_indices(model_output = detectability)
  trends_detectability <- generate_trends(indices = indices_detectability)
  trend_map_detectability <- plot_map(trends_detectability)
  indices_plot_detectability <- plot_indices(indices = indices_detectability)
  
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
  
#}
