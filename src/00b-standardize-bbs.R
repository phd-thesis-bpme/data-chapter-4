####### Script Information ########################
# Brandon P.M. Edwards
# BBS Data Integration
# <00b-standardize-bbs.R>
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

# Create long format so that each stop is its own row
bbs_counts_long <- melt(bbs_counts,
                        id.vars = c("route_data_id", "country_num", "state_num",
                                    "route", "rpid", "year", "aou",
                                    "bcr", "unid_combined"),
                        variable.name = "stop",
                        value.name = "species_total")


####### Output ####################################