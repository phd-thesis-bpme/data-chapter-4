####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <06-route-vs-point.R>
# Created April 2024
# Last Updated April 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)
library(cmdstanr)
library(bayesplot)
theme_set(theme_pubclean())
bayesplot::color_scheme_set("red")

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

route_indices <- readRDS(file = paste0("output/indices/", sp, "_route.RDS"))
point_indices <- readRDS(file = paste0("output/indices/", sp, "_point.RDS"))

####### Compare Indices ###########################

regions_both <- intersect(route_indices$indices$region, point_indices$indices$region)

route_df <- route_indices$indices[which(route_indices$indices$region %in% regions_both &
                                          route_indices$indices$region_type == "stratum"), ]
route_df$yr_region <- paste0(route_df$year, "-", route_df$region)

point_df <- point_indices$indices[which(point_indices$indices$region %in% regions_both &
                                          point_indices$indices$region_type == "stratum"), ]
point_df$yr_region <- paste0(point_df$year, "-", point_df$region)

# Make sure order of indices align
region_order <- match(route_df$yr_region, point_df$yr_region)
point_df <- point_df[region_order, ]

model_data <- list(N = nrow(route_df),
                   n_strata = length(unique(route_df$region)),
                   route_index = route_df$index / 50,
                   point_index = point_df$index,
                   stratum = as.numeric(factor(route_df$region)))

comp_model <- cmdstan_model(stan_file = "models/route-vs-point.stan")

comp_model_run <- comp_model$sample(
  data = model_data,
  iter_warmup = 1000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 2,
  refresh = 10
)

mod_summary <- comp_model_run$summary()

indices_slope_plot <- bayesplot::mcmc_areas(comp_model_run$draws("BETA"), prob = 0.95)

####### Compare Trends ############################

route_indices$indices <- route_indices$indices[which(route_indices$indices$region %in% regions_both), ]
point_indices$indices <- point_indices$indices[which(point_indices$indices$region %in% regions_both), ]

route_trends <- generate_trends(route_indices)
point_trends <- generate_trends(point_indices)

summary(lm(point_trends$trends$trend ~ route_trends$trends$trend))
