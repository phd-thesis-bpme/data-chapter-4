####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <03-run-point-detectability-model.R>
# Created July 2023
# Last Updated February 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(napops)

####### Set Constants #############################

sp <- "Ovenbird"
sp_code <- "OVEN"
st <- "latlong"

####### Read Data #################################

bbs_counts <- readRDS(file = "data/generated/bbs_counts.RDS")
bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
bbs_species <- load_bbs_data(level = "stop")$species

####### Main Code #################################

bbs_data <- list(birds = bbs_counts,
                 routes = bbs_sites,
                 species = bbs_species)
rm(bbs_counts); rm(bbs_sites); rm(bbs_species); gc()

bbs_stratified <- stratify(by = st, level = "stop", species = sp, data_custom = bbs_data)

# Limit analysis to only Ontario, Canada
bbs_stratified$routes_strata <- 
  bbs_stratified$routes_strata[which(bbs_stratified$routes_strata$st_abrev == "ON"), ]

prepared_data <- prepare_data(strata_data = bbs_stratified,
                          min_year = 2000,
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
# Need to actually figure out information on Time zone. Ugh...

kappa_p <- avail_fd(species = sp_code,
                    model = "best",
                    od = od,
                    tssr = rep(0, times = length(od)),
                    pairwise = TRUE,
                    time = rep(3, times = length(od)))

kappa_q <- percept_fd(species = sp_code,
                      model = 2,
                      road = rep(TRUE, times= length(od)),
                      forest = rep(1, times = length(od)),
                      pairwise = TRUE,
                      distance = rep(400, times = length(od)))

p <- avail(species = sp_code,
           model = "best",
           od = od,
           tssr = rep(0, times = length(od)),
           pairwise = TRUE,
           time = rep(3, times = length(od)))$p

q <- percept(species = sp_code,
             model = 2,
             road = rep(TRUE, times = length(od)),
             forest = rep(1, times = length(od)),
             pairwise = TRUE,
             distance = rep(400, times = length(od)))$q

p_vcv <- get_vcv(species = sp_code, model_type = "rem", model_num = 3)
q_vcv <- get_vcv(species = sp_code, model_type = "dis", model_num = 2)

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
                       chains = 1,
                       iter_warmup = 10,
                       iter_sampling = 10,
                       output_basename = paste0(sp, "-detectability"),
                       output_dir = "data/generated/model_runs",
                       overwrite = TRUE)
