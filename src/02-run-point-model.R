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
st <- "bbs_cws"

####### Read Data #################################

bbs_counts <- readRDS(file = "data/generated/bbs_counts.RDS")
bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
bbs_species <- load_bbs_data(level = "stop")$species

####### Main Code #################################

bbs_data <- list(birds = bbs_counts,
                 routes = bbs_sites,
                 species = bbs_species)

bbs_stratified <- stratify(by = st, species = sp, data_custom = bbs_data)

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







trends_bbs <- stratify(by = "latlong", species = sp,
                       level = "stop",
                       data_custom = bbs_data) %>%
  prepare_data() %>%
  prepare_model(model = "gamye") %>%
  run_model(iter_warmup = 20, iter_sampling = 20, chains = 2, save_model = FALSE) %>%
  generate_indices() %>%
  generate_trends()
saveRDS(trends_bbs, file = paste0("output/", sp, "_trends_bbs.RDS"))
png(filename = paste0("output/", sp, "_bbs.png"), width = 6, height = 6, units = "in", res = 300)
plot_map(trends_bbs)
dev.off()

message("Combined Model Starting...\n")
# Package up bam data (will likely be done earlier)
combined_data <- list(birds = rbind(bbs_counts, bam_counts),
                 routes = rbind(bbs_sites, bam_sites),
                 species = bbs_species)
trends_combined <- stratify(by = "latlong", species = sp,
                            level = "stop",
                            data_custom = combined_data) %>%
  prepare_data() %>%
  prepare_model(model = "gamye") %>%
  run_model(iter_warmup = 20, iter_sampling = 20, chains = 2, save_model = FALSE) %>%
  generate_indices() %>%
  generate_trends()
saveRDS(trends_combined, file = paste0("output/", sp, "_trends_combined.RDS"))
png(filename = paste0("output/", sp, "_combined.png"), width = 6, height = 6, units = "in", res = 300)
plot_map(trends_combined)
dev.off()

####### Output ####################################