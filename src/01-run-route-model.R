####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <01-run-route-model.R>
# Created July 2023
# Last Updated July 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

sp <- "Ovenbird"
st <- "bbs_cws"

####### Read Data #################################

####### Main Code #################################

bbs_stratified <- stratify(by = st, species = sp)

# Limit analysis to only Canada
bbs_stratified$routes_strata <- 
  bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$country == "CA"), ]

mod_prepped <- prepare_data(strata_data = bbs_stratified,
                            min_year = 1990) %>%
  prepare_spatial(strata_map = load_map(st)) %>%
  prepare_model(model = "gamye", model_variant = "spatial")

model_run <- run_model(model_data = mod_prepped,
                       output_basename = paste0(sp, "-route"),
                       output_dir = "data/generated/model_runs")

# This will likely be moved to its own analysis script at some point, easier to do all in one right now
indices <- generate_indices(model_output = model_run)
p <- plot_indices(indices = indices,
                  add_observed_means = TRUE) # optional argument to show raw observed mean counts

trends <- generate_trends(indices = indices)
trend_map <- plot_map(trends)

####### Output ####################################

pdf(file = paste0("output/plots/indices_route_", sp, ".pdf"))
print(p)
dev.off()

pdf(file = paste0("output/plots/trends_route_", sp, ".pdf"))
print(trend_map)
dev.off()