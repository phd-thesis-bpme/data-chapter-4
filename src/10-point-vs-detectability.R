####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <10-point-vs-detectability.R>
# Created March 2024
# Last Updated May 2024

####### Import Libraries and External Files #######

library(terra)
library(geodata)
library(bbsBayes2)

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

point_model <- readRDS(paste0("output/model_runs/", sp, "-point.rds"))
detectability_model <- readRDS(paste0("output/model_runs/", sp, "-detectability.rds"))

point_indices <- readRDS(paste0("output/indices/", sp, "_point.RDS"))
detect_indices <- readRDS(paste0("output/indices/", sp, "_detectability.RDS"))

nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

latlong <- load_map("latlong")
####### Main Code #################################

# Exploratory part to find some points with lots of forest cover change
data <- detectability_model$raw_data

points <- unique(data$route)

fc_change <- data.frame(Point = points,
                        Stratum = NA,
                        Change = NA,
                        N_Years = NA,
                        Longitude = NA,
                        Latitude = NA)

for (i in 1:nrow(fc_change))
{
  pt <- fc_change$Point[i]
  temp <- data[which(data$route == pt), ]
  
  stratum_list <- data[which(data$route == pt), ]
  fc_change$Stratum[i] <- unique(stratum_list$strata_name)
  
  n_year <- max(temp$year) - min(temp$year) + 1
  fc_min_year <- as.numeric(temp[which(temp$year == min(temp$year)), "forest_coverage"])
  fc_max_year <- as.numeric(temp[which(temp$year == max(temp$year)), "forest_coverage"])
  
  fc_change$Change[i] <- fc_max_year - fc_min_year
  fc_change$N_Years[i] <- n_year
  fc_change$Longitude[i] <- temp$longitude[1]
  fc_change$Latitude[i] <- temp$latitude[1]
}

# Generate Canada-wide map of forest change for each point
rbPal <- colorRampPalette(c('red','blue'))
fc_change$Col <- rbPal(10)[as.numeric(cut(fc_change$Change,breaks = 10))]

point_rast <- vect(matrix(c(fc_change$Longitude, fc_change$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change$Change,
                                     Colour = fc_change$Col),
                   crs = "+proj=longlat")

plot(point_rast, col = fc_change$Col, cex = 0.5)

# Landcover change at a specific route

mean_fc_change_stratum <- aggregate(fc_change$Change, list(fc_change$Stratum), mean)
names(mean_fc_change_stratum) <- c("Stratum", "Mean_Change")

pt_trends <- generate_trends(point_indices)
detect_trends <- generate_trends(detect_indices)

mean_fc_change_stratum$Point_Trend <- NA
mean_fc_change_stratum$Detectability_Trend <- NA
for (s in mean_fc_change_stratum$Stratum)
{
  det_trend <- detect_trends$trends[which(detect_trends$trends$region == s), ]$trend
  point_trend <- pt_trends$trends[which(pt_trends$trends$region == s),]$trend
  
  mean_fc_change_stratum[which(mean_fc_change_stratum$Stratum == s), "Point_Trend"] <- point_trend
  mean_fc_change_stratum[which(mean_fc_change_stratum$Stratum == s), "Detectability_Trend"] <- det_trend
}

mean_fc_change_stratum$Difference <- mean_fc_change_stratum$Detectability_Trend - mean_fc_change_stratum$Point_Trend

mod <- lm(Difference ~ Mean_Change, data = mean_fc_change_stratum)
plot(mean_fc_change_stratum$Mean_Change, mean_fc_change_stratum$Difference,
     xlab = "Change in Forest Coverage",
     ylab = "Trend Difference (Detectability - Point)")
abline(a = coef(mod)[1], b = coef(mod)[2])






ind_plot <- plot_indices(indices = point_indices)
det_plot <- plot_indices(indices = detect_indices)

st_min <- mean_fc_change_stratum[which(mean_fc_change_stratum$Mean_Change == min(mean_fc_change_stratum$Mean_Change)), "Stratum"]
st_max <- mean_fc_change_stratum[which(mean_fc_change_stratum$Mean_Change == max(mean_fc_change_stratum$Mean_Change)), "Stratum"]

fc_change_stratum <- fc_change[which(fc_change$Stratum == st_min), ]
strat_rast <- vect(matrix(c(fc_change_stratum$Longitude, fc_change_stratum$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change_stratum$Change,
                                     Colour = fc_change_stratum$Col),
                   crs = "+proj=longlat")
plot(strat_rast, col = fc_change_stratum$Col, cex = 0.5)
print(ind_plot[which(names(ind_plot) == gsub("-", "", st_min))])
print(det_plot[which(names(ind_plot) == gsub("-", "", st_min))])
pt_trends$trends[which(pt_trends$trends$region == st_min), ]
detect_trends$trends[which(detect_trends$trends$region == st_min), ]

fc_change_stratum <- fc_change[which(fc_change$Stratum == st_max), ]
strat_rast <- vect(matrix(c(fc_change_stratum$Longitude, fc_change_stratum$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change_stratum$Change,
                                     Colour = fc_change_stratum$Col),
                   crs = "+proj=longlat")
plot(strat_rast, col = fc_change_stratum$Col, cex = 0.5)
print(ind_plot[which(names(ind_plot) == gsub("-", "", st_max))])
print(det_plot[which(names(ind_plot) == gsub("-", "", st_max))])
pt_trends$trends[which(pt_trends$trends$region == st_max), ]
detect_trends$trends[which(detect_trends$trends$region == st_max), ]




# Landcover Change at a specific point
pt <- "76-76-71-stop_25"
lat <- fc_change[which(fc_change$Point == pt), "Latitude"]
lon <- fc_change[which(fc_change$Point == pt), "Longitude"]

lc_2010 <- project(vect(matrix(c(lon, lat), ncol = 2),
                    crs = "+proj=longlat"),
                    crs(nalcms_2010)) |>
  buffer(width = 400)
plot(crop(nalcms_2010, lc_2010))
lines(lc_2010)

lc_2015 <- project(vect(matrix(c(lon, lat), ncol = 2),
                        crs = "+proj=longlat"),
                   crs(nalcms_2015)) |>
  buffer(width = 400)
plot(crop(nalcms_2015, lc_2015))
lines(lc_2015)

lc_2020 <- project(vect(matrix(c(lon, lat), ncol = 2),
                        crs = "+proj=longlat"),
                   crs(nalcms_2015)) |>
  buffer(width = 400)
plot(crop(nalcms_2020, lc_2020))
lines(lc_2020)




####### Output ####################################















