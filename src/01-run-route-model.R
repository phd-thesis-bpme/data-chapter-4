####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <01-run-route-model.R>
# Created July 2023
# Last Updated March 2024

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

species_list <- c("Ovenbird", "Swainson's Thrush", "American Crow",
        "Blue-headed Vireo", "Eastern Phoebe", "Tennessee Warbler",
        "Yellow Warbler")
sp_code <- c("OVEN", "SWTH", "AMCR", "BHVI", "EAPH", 
             "TEWA", "YEWA")

st <- "latlong"

####### Read Data #################################

####### Main Code #################################

for (i in 1:length(species_list))
{
  sp <- species_list[i]
  sp_out <- sp_code[i]
  
  print(sp)
  bbs_stratified <- stratify(by = st, species = sp)
  
  # Limit analysis to only Ontario, Canada
  bbs_stratified$routes_strata <- 
    bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$st_abrev == "ON"), ]
  bbs_stratified$routes_strata$forest_coverage <- NA # bbsBayes2 needs this, doesn't exist in route modelling
  
  mod_prepped <- prepare_data(strata_data = bbs_stratified,
                              min_year = 2010,
                              min_n_routes = 1) %>%
    prepare_spatial(strata_map = load_map(st)) %>%
    prepare_model(model = "gamye", model_variant = "spatial")
  
  model_run <- run_model(model_data = mod_prepped,
                         output_basename = paste0(sp_out, "-route"),
                         output_dir = "data/generated/model_runs",
                         overwrite = TRUE)
}
