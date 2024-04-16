####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <06-fc-change-hotspot.R>
# Created March 2024
# Last Updated March 2024

####### Import Libraries and External Files #######

library(terra)
library(geodata)
library(bbsBayes2)

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

model <- readRDS(paste0("data/generated/model_runs/", sp, "-detectability.rds"))

nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

latlong <- load_map("latlong")
####### Main Code #################################

# Exploratory part to find some points with lots of forest cover change
data <- model$raw_data

points <- unique(data$route)

fc_change <- data.frame(Point = points,
                        Change = NA,
                        N_Years = NA,
                        Longitude = NA,
                        Latitude = NA)

for (i in 1:nrow(fc_change))
{
  pt <- fc_change$Point[i]
  temp <- data[which(data$route == pt), ]
  
  n_year <- max(temp$year) - min(temp$year) + 1
  fc_min_year <- as.numeric(temp[which(temp$year == min(temp$year)), "forest_coverage"])
  fc_max_year <- as.numeric(temp[which(temp$year == max(temp$year)), "forest_coverage"])
  
  fc_change$Change[i] <- fc_max_year - fc_min_year
  fc_change$N_Years[i] <- n_year
  fc_change$Longitude[i] <- temp$longitude[1]
  fc_change$Latitude[i] <- temp$latitude[1]
}

rbPal <- colorRampPalette(c('red','blue'))
fc_change$Col <- rbPal(10)[as.numeric(cut(fc_change$Change,breaks = 10))]

point_rast <- vect(matrix(c(fc_change$Longitude, fc_change$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change$Change,
                                     Colour = fc_change$Col),
                   crs = "+proj=longlat")

plot(point_rast, col = fc_change$Col)

####### Output ####################################