####### Script Information ########################
# Brandon P.M. Edwards
# BBS Data Integration
# <00-bbs-base.R>
# Created June 2023
# Last Updated June 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Set Constants #############################

species <- c("Pine Warbler", "Blackpoll Warbler", "Grey-cheeked Thrush")
strat <- "latlong"
map <- load_map(stratify_by = strat)

####### Main Code #################################

for (sp in species)
{
  strat_sp <- stratify(by = strat, species = sp)
  
  data_prepped <- prepare_data(strat_sp) %>%
    prepare_spatial(strata_map = map)
  
  model_sp <- prepare_model(prepared_data = data_prepped,
                            model = "gamye",
                            model_variant = "spatial")
  
  m <- run_model(model_sp, iter_sampling = 20, iter_warmup = 20,
                 refresh = 5, chains = 2)
}

####### Output ####################################