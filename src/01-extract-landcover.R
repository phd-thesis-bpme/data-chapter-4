####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# 00c-extract-nalcms-landcover.R
# Created March 2024
# Last Updated June 2024

####### Import Libraries and External Files #######

library(terra)

####### Read Data #################################

bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

####### Main Code #################################

bbs_sites$forest_coverage <- NA

indices<- which(bbs_sites$year >= 2010 &
                     bbs_sites$latitude < 1000)

bbs_reduced <- bbs_sites[indices, ]

routes <- unique(bbs_reduced$route)

for (r in routes)
{
  temp <- bbs_reduced[which(bbs_reduced$route == r), ]
  
  forest_df <- data.frame(year = seq(2010, max(2020, max(temp$year))))
  
  forest_df <- merge(forest_df, temp[, c("year", "latitude", "longitude")],
                     by = "year", all = TRUE)
  
  #' Check that all missing latitude and longitudes can safely just be copied 
  #' based on latitudes and longitudes that are availabile. I.e., if the route
  #' location had changed overtime, we have to account for this
  
  #if (var(forest_df$latitude, na.rm = TRUE) == 0)
 # {
    forest_df[which(is.na(forest_df$latitude)), "latitude"] <- median(forest_df$latitude, na.rm = TRUE)
 # }
  
#  if (var(forest_df$longitude, na.rm = TRUE) == 0)
  #{
    forest_df[which(is.na(forest_df$longitude)), "longitude"] <- median(forest_df$longitude, na.rm = TRUE)
  #}
  
  # landcover in 2010
  lat <- forest_df[which(forest_df$year == 2010), "latitude"]
  lon <- forest_df[which(forest_df$year == 2010), "longitude"]
  
  bbs_point <- project(vect(matrix(c(lon, lat), ncol = 2),
                            crs = "+proj=longlat"),
                       crs(nalcms_2010))
  bbs_radius <- buffer(bbs_point, width = 400)
  landcover_types <- extract(nalcms_2010, bbs_radius)
  n_forest <- nrow(landcover_types[which(grepl("forest", 
                                               landcover_types$Class_Name)),])
  fc_2010 <- n_forest / nrow(landcover_types)
  
  # landcover in 2015
  lat <- forest_df[which(forest_df$year == 2015), "latitude"]
  lon <- forest_df[which(forest_df$year == 2015), "longitude"]
  
  bbs_point <- project(vect(matrix(c(lon, lat), ncol = 2),
                            crs = "+proj=longlat"),
                       crs(nalcms_2015))
  bbs_radius <- buffer(bbs_point, width = 400)
  landcover_types <- extract(nalcms_2015, bbs_radius)
  n_forest <- nrow(landcover_types[which(landcover_types$CAN_NALCMS_landcover_2015v3_30m <= 6),])
  fc_2015 <- n_forest / nrow(landcover_types)
  
  # landcover in 2020
  lat <- forest_df[which(forest_df$year == 2020), "latitude"]
  lon <- forest_df[which(forest_df$year == 2020), "longitude"]
  
  bbs_point <- project(vect(matrix(c(lon, lat), ncol = 2),
                            crs = "+proj=longlat"),
                       crs(nalcms_2020))
  bbs_radius <- buffer(bbs_point, width = 400)
  landcover_types <- extract(nalcms_2020, bbs_radius)
  n_forest <- nrow(landcover_types[which(landcover_types$CAN_NALCMS_landcover_2020_30m <= 6),])
  fc_2020 <- n_forest / nrow(landcover_types)
  
  # Interpolate missing values
  # 2010 - 2015
  values_tp1 <- seq(from = fc_2010,
                to = fc_2015,
                length.out = 6)
  
  # 2015 - 2020
  values_tp2 <- seq(from = fc_2015,
                    to = fc_2020,
                    length.out = 6)
  
  #' Since we don't know the current rate of landcover change for any years past 2020,
  #' we will just copy the 2020 values for the remaining years
  if (nrow(forest_df[which(forest_df$year > 2020), ]) > 0)
  {
    values_tp3 <- rep(fc_2020,
                      times = nrow(forest_df[which(forest_df$year > 2020), ])) 
  }else {
    values_tp3 <- NULL
  }
  
  fc_vals <- c(values_tp1, values_tp2[2:length(values_tp2)], values_tp3)
  forest_df$forest_coverage <- fc_vals
  
  forest_df_red <- forest_df[which(forest_df$year %in% temp$year), ]
  
  bbs_reduced[which(bbs_reduced$route == r), "forest_coverage"] <- forest_df_red$forest_coverage

}
  
#' Now we just have to actually put these forest coverage values into the main bbs_sites df
#' Easiest way is to probably remove all the ones that previously had NA values, then add this
#' new DF back in. Saves possible merging issues.

bbs_sites_red <- bbs_sites[-indices, ]
bbs_sites_fc <- rbind(bbs_sites_red, bbs_reduced)
bbs_sites <- bbs_sites_fc

####### Output ####################################

saveRDS(object = bbs_sites, file = "data/generated/bbs_sites_fc.RDS")
