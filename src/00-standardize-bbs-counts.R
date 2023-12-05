####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <00-standardize-bbs-counts.R>
# Created June 2023
# Last Updated June 2023

####### Import Libraries and External Files #######

library(bbsBayes2)
library(reshape2)
library(sf)

####### Set Constants #############################

sf::sf_use_s2(FALSE)

####### Read Data #################################

# Read in BBS stop-level data
bbs_counts <- load_bbs_data(level = "stop")$birds
bbs_route <- load_bbs_data(level = "stop")$route
bbs_species <- load_bbs_data(level = "stop")$species

stop_location <- read_sf("data/raw/bbs-routes-stops",
                         layer = "CURRENT_STOPS_2022")

####### Main Code #################################
# filter down BBS data to Canada only (country code 124)
bbs_counts <- bbs_counts[which(bbs_counts$country_num == 124), ]
bbs_route <- bbs_route[which(bbs_route$country_num == 124), ]

# add province codes to stop location data
stop_location$st_abrev <- NULL
stop_location[which(stop_location$Province == 4), "st_abrev"] <- "AB"
stop_location[which(stop_location$Province == 11), "st_abrev"] <- "BC"
stop_location[which(stop_location$Province == 43), "st_abrev"] <- "NT"
stop_location[which(stop_location$Province == 45), "st_abrev"] <- "MB"
stop_location[which(stop_location$Province == 56), "st_abrev"] <- "NB"
stop_location[which(stop_location$Province == 57), "st_abrev"] <- "NL"
stop_location[which(stop_location$Province == 65), "st_abrev"] <- "NS"
stop_location[which(stop_location$Province == 68), "st_abrev"] <- "ON"
stop_location[which(stop_location$Province == 75), "st_abrev"] <- "PE"
stop_location[which(stop_location$Province == 76), "st_abrev"] <- "QC"
stop_location[which(stop_location$Province == 79), "st_abrev"] <- "SK"
stop_location[which(stop_location$Province == 93), "st_abrev"] <- "YT"

stop_location$rt_st <- paste0(stop_location$route, "-", stop_location$st_abrev)
bbs_route$rt_st <- paste0(bbs_route$route, "-", bbs_route$st_abrev)

bbs_route_merged <- dplyr::left_join(stop_location[, c("rt_st",
                                                       "POINT_X",
                                                       "POINT_Y",
                                                       "Stop")],
                                     bbs_route, 
                                      by = "rt_st")

bbs_route_merged <- bbs_route_merged[-which(is.na(bbs_route_merged$route)), ]
# sort by routexstate combo, then year, then by stop number
# i don't think this is actually necessary,but keeps bookkeeping a bit easier
bbs_route_sorted <- bbs_route_merged[order(bbs_route_merged$rt_st,
                                           bbs_route_merged$year,
                                           bbs_route_merged$Stop), ]
# keep track of index
bbs_route_sorted$index <- seq(1, nrow(bbs_route_sorted))
bbs_route_sorted$stop_start_time <- NA

# Infer start times for each stop in a route
for (rs in unique(bbs_route_sorted$rt_st))
{
  temp1 <- bbs_route_sorted[which(bbs_route_sorted$rt_st == rs), ]
  for (y in unique(temp1$year))
  {
    temp2 <- temp1[which(temp1$year == y), ]
    n_stops <- nrow(temp2)
    start_time <- as.POSIXct(sprintf("%04d",temp2$start_time[1]),
                             format = "%H%M")
    #subtract 3 minutes off the "end time" because we actually want the
    # start time of the final stop
    end_time <- as.POSIXct(sprintf("%04d",temp2$end_time[1]),
                           format = "%H%M") - (3 * 60)
    start_time_vector <- seq(from = start_time, to = end_time,
                             length = n_stops)
    indices <- temp2$index
    
    bbs_route_sorted[which(bbs_route_sorted$index %in% indices), "stop_start_time"] <- 
      format(start_time_vector, "%H%M")
  }
}

bbs_route_sorted$route <- paste0(bbs_route_sorted$route,
                                 "-stop_",
                                 bbs_route_sorted$Stop)

# Now reorder this routes dataframe to match the BBS
bbs_route_stops <- as.data.frame(bbs_route_sorted[, c("country_num",
                                        "state_num",
                                        "route",
                                        "route_name",
                                        "active",
                                        "POINT_Y",
                                        "POINT_X",
                                        "bcr",
                                        "route_type_id",
                                        "route_type_detail_id",
                                        "route_data_id",
                                        "rpid",
                                        "year",
                                        "month",
                                        "day",
                                        "obs_n",
                                        "total_spp",
                                        "start_temp",
                                        "end_temp",
                                        "temp_scale",
                                        "start_wind",
                                        "end_wind",
                                        "start_sky",
                                        "end_sky",
                                        "stop_start_time",
                                        "end_time",
                                        "assistant",
                                        "quality_current_id",
                                        "run_type",
                                        "state",
                                        "st_abrev",
                                        "country")])
names(bbs_route_stops) <- names(bbs_route)

# Create long format so that each stop is its own row
bbs_counts_long <- melt(bbs_counts,
                        id.vars = c("route_data_id", "country_num", "state_num",
                                    "route", "rpid", "year", "aou",
                                    "bcr", "unid_combined"),
                        variable.name = "stop",
                        value.name = "species_total")

bbs_counts_long$route <- paste0(bbs_counts_long$route,
                                "-",
                                bbs_counts_long$stop)
bbs_counts_long <- as.data.frame(bbs_counts_long[, c("route_data_id",
                                       "country_num",
                                       "state_num",
                                       "route",
                                       "rpid",
                                       "year",
                                       "aou",
                                       "species_total",
                                       "bcr",
                                       "unid_combined")])

####### Output ####################################

saveRDS(bbs_route_stops, file = "data/generated/bbs_sites.RDS")
saveRDS(bbs_counts_long, file = "data/generated/bbs_counts.RDS")
