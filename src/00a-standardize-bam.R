####### Script Information ########################
# Brandon P.M. Edwards
# BBS Data Integration
# <00a-standardize-bam.R>
# Created June 2023
# Last Updated June 2023

####### Import Libraries and External Files #######

library(bbsBayes2)

####### Read Data #################################

# Read in BAM data
bam_files <- list.files("data/raw/bam")
bam_data <- vector(mode = "list", length = length(bam_files))
i <- 1
for (f in bam_files)
{
  bam_data[[i]] <- read.csv(paste0("data/raw/bam/",f))
  i <- i + 1
}
bam_data <- do.call(rbind, bam_data)

# Read in BBS data to use headers
bbs_counts <- load_bbs_data()$birds
bbs_route <- load_bbs_data()$route
bbs_species <- load_bbs_data()$species

####### Main Code #################################

# Create unique identifier to the location x date
bam_data$site_date <- paste0(bam_data$location, ":", bam_data$date)

# Create year, month, day columns
dates <- as.POSIXct(bam_data$date, format = "%Y-%m-%d %H:%M:%S")
bam_data$year <- format(dates, "%Y")
bam_data$month <- format(dates, "%m")
bam_data$day <- format(dates, "%d")
bam_data$start_time <- format(dates, "%H%M")

# Create a dataframe with one entry per site x date
bam_sites <- bam_data[!duplicated(bam_data$site_date), ]

# Create an empty "bbs-like" sites/routes DF to now populate
bam_sites_bbs <- data.frame(matrix(nrow = nrow(bam_sites), ncol = ncol(bbs_route)))
names(bam_sites_bbs) <- names(bbs_route)

# Populate this BBS-like routes DF with bam site-level data (as much as possible)
bam_sites_bbs$latitude <- bam_sites$latitude
bam_sites_bbs$longitude <- bam_sites$longitude
bam_sites_bbs$route <- bam_sites$location
bam_sites_bbs$obs_n <- bam_sites$observer
bam_sites_bbs$year <- bam_sites$year #format(dates, "%Y")
bam_sites_bbs$month <- bam_sites$month #format(dates, "%m")
bam_sites_bbs$day <- bam_sites$day# format(dates, "%d")
bam_sites_bbs$start_time <- bam_sites$start_time # format(dates, "%H%M")
# Infer end time based on durationMethod
dur_method <- strsplit(bam_sites$durationMethod, "-")
duration <- unlist(lapply(dur_method, tail, n = 1L))
duration_num <- as.numeric(gsub("([0-9]+).*$", "\\1", duration))
bam_sites_bbs$end_time <- format(as.POSIXct(bam_sites$date, format = "%Y-%m-%d %H:%M:%S") + 
                                   (duration_num * 60), 
                                 "%H%M")

# Create a dataframe that sums counts for each species for a given site x date
counts_by_sitedate <- aggregate(abundance ~ site_date + speciesCommonName + location + year,
                                data = bam_data,
                                FUN = sum)

# Now Create an empty "bbs-like" counts DF to populate
bam_counts <- data.frame(matrix(nrow = nrow(counts_by_sitedate), ncol = ncol(bbs_counts)))
names(bam_counts) <- names(bbs_counts)

#populate this data frame
bam_counts$species_total <- counts_by_sitedate$abundance
bam_counts$route <- counts_by_sitedate$location
bam_counts$year <- counts_by_sitedate$year
species_not_found <- NULL
for (i in 1:nrow(bam_counts))
{
  aou <- search_species(species = counts_by_sitedate$speciesCommonName[i])$aou
  if (length(aou) > 1)
  {
    if (counts_by_sitedate$speciesCommonName[i] %in% species_not_found == FALSE)
    {
      species_not_found <- c(species_not_found, counts_by_sitedate$speciesCommonName[i])
    }
  }else if (length(aou) == 0){
    bam_counts$aou[i] <- NA
  }else{
    bam_counts$aou[i] <- aou
  }
}

####### Output ####################################