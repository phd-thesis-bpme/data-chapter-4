####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <02-run-point-model.R>
# Created June 2023
# Last Updated July 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

sp <- "Ovenbird"
st <- "latlong"

####### Read Data #################################

bbs_counts <- readRDS(file = "data/generated/bbs_counts.RDS")
bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
bbs_species <- load_bbs_data(level = "stop")$species

####### Main Code #################################

bbs_data <- list(birds = bbs_counts,
                 routes = bbs_sites,
                 species = bbs_species)

bbs_stratified <- stratify(by = st, level = "stop", species = sp, data_custom = bbs_data)

# Limit analysis to only Ontario, Canada
bbs_stratified$routes_strata <- 
  bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$st_abrev == "ON"), ]

mod_prepped <- prepare_data(strata_data = bbs_stratified,
                            min_year = 1990,
                            min_n_routes = 1) %>%
  prepare_spatial(strata_map = load_map(st)) %>%
  prepare_model(model = "gamye", model_variant = "spatial")

model_run <- run_model(model_data = mod_prepped,
                       output_basename = paste0(sp, "-point"),
                       output_dir = "data/generated/model_runs",
                       overwrite = TRUE)

# This will likely be moved to its own analysis script at some point, easier to do all in one right now
indices <- generate_indices(model_output = model_run)
p <- plot_indices(indices = indices,
                  add_observed_means = TRUE) # optional argument to show raw observed mean counts

trends <- generate_trends(indices = indices)
trend_map <- plot_map(trends)

####### Output ####################################

pdf(file = paste0("output/plots/indices_point_", sp, ".pdf"))
print(p)
dev.off()

pdf(file = paste0("output/plots/trends_point_", sp, ".pdf"))
print(trend_map)
dev.off()
