####### Script Information ########################
# Brandon P.M. Edwards
# BBS Data Integration
# <01-combine-counts.R>
# Created June 2023
# Last Updated June 2023

####### Import Libraries and External Files #######

library(bbsBayes2)
library(reshape2)

####### Read Data #################################

# Uncomment following line if BBS data has not yet been downloaded.
# fetch_bbs_data(level = "stop")
bbs_data <- load_bbs_data(level = "stop")

bam_files <- list.files("data/raw/bam")
bam_data <- vector(mode = "list", length = length(bam_files))
i <- 1
for (f in bam_files)
{
  temp <- 
  bam_data[[i]] <- read.csv(paste0("data/raw/bam/",f))
  i <- i + 1
}
bam_data <- do.call(rbind, bam_data)

####### Main Code #################################

piwa_strat <- stratify(by = "latlong", species = "Pine Warbler", level = "stop")

# subset to pine warbler for now
bbs_bird <- bbs_data$birds[which(bbs_data$birds$aou == 6710), ]

# Melt BBS data into one site x year per line
bbs_long <- melt(bbs_bird, id = c("route_data_id",
                                        "country_num",
                                        "state_num",
                                        "route",
                                        "rpid",
                                        "year",
                                        "aou",
                                        "bcr",
                                        "unid_combined"))

bbs_route <- bbs_data$routes

####### Output ####################################