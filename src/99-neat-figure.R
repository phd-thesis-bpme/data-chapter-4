####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <04-model-comparisons.R>
# Created March 2024
# Last Updated March 2024

####### Import Libraries and External Files #######

library(terra)
library(napops)

####### Set Constants #############################

sp_list <- c("OVEN")

####### Read Data #################################

model <- readRDS(paste0("data/generated/model_runs/", sp_list, "-detectability.rds"))

nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

####### Main Code #################################

# Exploratory part to find some points with lots of forest cover change
data <- model$raw_data

points <- unique(data$route)

fc_change <- data.frame(Point = points,
                        Change = NA,
                        N_Years = NA)

for (i in 1:nrow(fc_change))
{
  pt <- fc_change$Point[i]
  temp <- data[which(data$route == pt), ]
  
  n_year <- max(temp$year) - min(temp$year) + 1
  fc_min_year <- as.numeric(temp[which(temp$year == min(temp$year)), "forest_coverage"])
  fc_max_year <- as.numeric(temp[which(temp$year == max(temp$year)), "forest_coverage"])
  
  fc_change$Change[i] <- fc_max_year - fc_min_year
  fc_change$N_Years[i] <- n_year
}

point <- "68-68-255-stop_27"
point_df <- data[which(data$route == point), ]

point_df$edr <- edr(species = "OVEN",
                            model = "best",
                            road = rep(TRUE, times = nrow(point_df)),
                            forest = point_df$forest_coverage,
                            pairwise = TRUE)[, "EDR_est"]

maps <- list(nalcms_2010, nalcms_2015, nalcms_2020)
years <- c(2010, 2015, 2020)

for (i in 1:3)
{
  y <- years[i]
  #png(filename = paste0("output/plots/fc-vs-edr/", years[i], ".png"),
   #   width = 8, height = 4, res = 300, units = "in")
  #par(mfrow = c(1, 2))
  map <- maps[[i]]
  pt_coords <- project(vect(matrix(c(point_df$longitude[1], point_df$latitude[1]), ncol = 2),
                            crs = "+proj=longlat"),
                       crs(map))
  bbs_radius <- buffer(pt_coords, width = 400)
  map_cropped <- crop(map, bbs_radius)
  plot(map_cropped)
  lines(bbs_radius)
  # 
  # plot(point_df$edr~point_df$year,
  #      col = ifelse(point_df$year == y, "red", "black"),
  #      pch=ifelse(point_df$year == y, 19, 1), cex=ifelse(point_df$year == y, 2, 1))
  
  dev.off()
}









####### Output ####################################