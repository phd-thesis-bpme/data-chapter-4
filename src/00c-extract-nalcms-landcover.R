####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# 00c-extract-nalcms-landcover.R
# Created March 2024
# Last Updated March 2024

####### Import Libraries and External Files #######

library(terra)

####### Read Data #################################

bbs_sites <- readRDS(file = "data/generated/bbs_sites_aci.RDS"); bbs_sites$rt_st <- NULL
nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

####### Main Code #################################

#' We need to go back and check which Ontario (with year > 2011) did NOT receive
#' landcover values. For those ones, we need to use the NALCMS data and interpolate.

# First, get the indices that didn't work
na_indices<- which(bbs_sites$year >= 2011 &
                     bbs_sites$st_abrev == "ON" &
                     bbs_sites$latitude < 1000 &
                     is.na(bbs_sites$forest_coverage))

bbs_reduced <- bbs_sites[na_indices, ]

routes <- unique(bbs_reduced$route)

for (r in routes)
{
  temp <- bbs_reduced[which(bbs_reduced$route == r), ]
  
  forest_df <- data.frame(year = seq(2010, max(2020, max(temp$year))),
                            forest_coverage = NA)
  
  

  
  forest_df <- merge(forest_df, temp[, c("year", "latitude", "longitude")],
                     by = "year", all = TRUE)
  
  
}
  

####### Output ####################################