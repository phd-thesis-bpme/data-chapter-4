####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <03-run-point-detectability-model.R>
# Created July 2023
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(napops)
library(suntools)
library(lutz)

####### Set Constants #############################

species_list <- c("Ovenbird", "Wood Thrush", "White-throated Sparrow", 
                  "Black-throated Green Warbler", "Winter Wren",
                  "Pileated Woodpecker", "Blue-headed Vireo", 
                  "American Goldfinch", "Eastern Wood-pewee")
sp_code_list <- c("OVEN", "WOTH", "WTSP", "BTNW", "WIWR", "PIWO",
             "BHVI", "AMGO", "EAWP")
st <- "latlong"

####### Read Data #################################

bbs_counts <- readRDS(file = "data/generated/bbs_counts.RDS")
bbs_sites <- readRDS(file = "data/generated/bbs_sites_fc.RDS"); bbs_sites$rt_st <- NULL
bbs_species <- load_bbs_data(level = "stop")$species

####### Main Code #################################

bbs_data <- list(birds = bbs_counts,
                 routes = bbs_sites,
                 species = bbs_species)
rm(bbs_counts); rm(bbs_sites); rm(bbs_species); gc()

for (i in 1:length(species_list))
{
  sp <- species_list[i]
  sp_code <- sp_code_list[i]
  
  bbs_stratified <- stratify(by = st, level = "stop", species = sp, data_custom = bbs_data)
  
  # Limit analysis to only Canada
  bbs_stratified$routes_strata <- 
    bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$country == "CA"), ]
  
  prepared_data <- prepare_data(strata_data = bbs_stratified,
                                min_year = 2010,
                                min_n_routes = 1) %>%
    prepare_spatial(strata_map = load_map(st))
  
  mod_prepped <- prepare_model(prepared_data = prepared_data,
                               model_file = "models/gamye_spatial_detectability.stan", 
                               model = "gamye", 
                               model_variant = "spatial")
  
  
  #' Consider doing this stuff below (i.e. extracting the ordinal day and other
  #' detectability variables) within bbsBayes. There might be opportunity to add
  #' that functionality into bbsBayes so that others can have detectability offsets
  #' as well. Plus I think that might be the easiest way to then subset the offsets
  #' properly based on the BBS data that are going to be included in the model.
  
  subsetted_data <- mod_prepped$raw_data
  
  # Ordinal Day
  od <- as.POSIXlt(sprintf("%04d-%02d-%02d", 
                           subsetted_data$year,
                           subsetted_data$month,
                           subsetted_data$day),
                   format = "%Y-%m-%d")$yday
  
  # Time since local sunrise
  coords <- matrix(c(subsetted_data$longitude, subsetted_data$latitude),
                   nrow = nrow(subsetted_data))
  
  date_time <- as.POSIXct(sprintf("%04d-%02d-%02d %s", 
                                  subsetted_data$year,
                                  subsetted_data$month,
                                  subsetted_data$day,
                                  subsetted_data$start_time),
                          format = "%Y-%m-%d %H%M")
  
  time_zone <- tz_lookup_coords(lat = subsetted_data$latitude,
                                lon = subsetted_data$longitude)
  
  utc_offset <- numeric(length = length(time_zone))
  for (i in seq_along(time_zone))
  {
    utc_offset[i] <- lutz::tz_offset(date_time[i], 
                                     tz = time_zone[i])$utc_offset_h
  }
  
  utc_time <- lubridate::force_tz(date_time  + (utc_offset*-1 * 60 * 60),
                                  tzone = "UTC")
  
  sunrise_times <- sunriset(crds = coords,
                            dateTime = utc_time,
                            direction = "sunrise",
                            POSIXct.out = TRUE)
  
  tssr <- as.numeric(difftime(utc_time, sunrise_times$time, units = "hours"))
  
  #' Determine which detectability model to use for each species.
  #' I may eventually decide to choose the "fullest best model", where if there is
  #' a tied (or within 5 delta AIC) model, I hcoose the fuller one.
  p_best_model_df <- coef_removal(species = sp_code)[,c("Model", "AIC")]
  p_best_model <- p_best_model_df[which(p_best_model_df$AIC == min(p_best_model_df$AIC)), "Model"]
  
  q_best_model_df <- coef_distance(species = sp_code)[, c("Model", "AIC")]
  q_best_model <- q_best_model_df[which(q_best_model_df$AIC == min(q_best_model_df$AIC)), "Model"]
  
  kappa_p <- avail_fd(species = sp_code,
                      model = p_best_model,
                      od = od,
                      tssr = tssr,
                      pairwise = TRUE,
                      time = rep(3, times = length(od)))
  
  kappa_q <- percept_fd(species = sp_code,
                        model = q_best_model,
                        road = rep(TRUE, times = nrow(subsetted_data)),
                        forest = subsetted_data$forest_coverage,
                        pairwise = TRUE,
                        distance = rep(400, times = length(od)))
  
  p <- avail(species = sp_code,
             model = p_best_model,
             od = od,
             tssr = tssr,
             pairwise = TRUE,
             time = rep(3, times = length(od)))$p
  
  q <- percept(species = sp_code,
               model = q_best_model,
               road = rep(TRUE, times = nrow(subsetted_data)),
               forest = subsetted_data$forest_coverage,
               pairwise = TRUE,
               distance = rep(400, times = length(od)))$q
  
  p_vcv <- get_vcv(species = sp_code,
                   model_type = "rem",
                   model_num = p_best_model)
  q_vcv <- get_vcv(species = sp_code, 
                   model_type = "dis", 
                   model_num = q_best_model)
  
  detectability_data_list <- list(n_avail_covs = ncol(kappa_p),
                                  n_percept_covs = ncol(kappa_q),
                                  kappa_p = kappa_p,
                                  kappa_q = kappa_q,
                                  vcv_p = p_vcv,
                                  vcv_q = q_vcv,
                                  p = p,
                                  q = q)
  
  mod_prepped$model_data <- c(mod_prepped$model_data, detectability_data_list)
  
  model_run <- run_model(model_data = mod_prepped,
                         output_basename = paste0(sp_code, "-detectability"),
                         output_dir = "output/model_runs",
                         overwrite = TRUE)
}
