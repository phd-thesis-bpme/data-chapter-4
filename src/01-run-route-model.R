####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <01-run-route-model.R>
# Created July 2023
# Last Updated December 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

sp <- "Ovenbird"
st <- "latlong"

####### Read Data #################################

####### Main Code #################################

bbs_stratified <- stratify(by = st, species = sp)

# Limit analysis to only Ontario, Canada
bbs_stratified$routes_strata <- 
  bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$st_abrev == "ON"), ]

mod_prepped <- prepare_data(strata_data = bbs_stratified,
                            min_year = 2000,
                            min_n_routes = 1) %>%
  prepare_spatial(strata_map = load_map(st)) %>%
  prepare_model(model = "gamye", model_variant = "spatial")

model_run <- run_model(model_data = mod_prepped,
                       output_basename = paste0(sp, "-route"),
                       output_dir = "data/generated/model_runs",
                       overwrite = TRUE)
