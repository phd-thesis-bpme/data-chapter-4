####### Script Information ########################
# Brandon P.M. Edwards
# BBS Data Integration
# <02-run-integrated-model.R>
# Created June 2023
# Last Updated June 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

sp <- "Connecticut Warbler"

####### Read Data #################################

bbs_counts <- readRDS(file = "data/generated/bbs_counts.RDS")
bam_counts <- readRDS(file = "data/generated/bam_counts.RDS")
bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
bam_sites <- readRDS(file = "data/generated/bam_sites.RDS")
bbs_species <- load_bbs_data(level = "stop")$species

####### Main Code #################################
message("BBS Model Starting...\n")
# Package up bbs data (will likely be done earlier)
bbs_data <- list(birds = bbs_counts,
                 routes = bbs_sites,
                 species = bbs_species)

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