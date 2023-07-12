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

# Remove all species that are not birds of North America, unids, sps, slashes, etc
to_remove <- c("Unidentified Warbler", "Unidentified Woodpecker", "Unidentified bird",
               "Unidentified Thrush", "Unidentified Black-backed / American Three-toed Woodpecker",
               "Unidentified Goldeneye", "Unidentified Vireo", "Unidentified Sapsucker",
               "Unidentified Flycatcher", "Unidentified Duck", "Unidentified Gull",
               "Unidentified Swallow", "Unidentified Yellowlegs", "Unidentified Blackbird",
               "Unidentified Sparrow", "Unidentified Tern", "NONE", "Unidentified Dabbling Duck",
               "Unidentified Passerine", "Unidentified Redpoll", "Hawaiian Duck", "Bahama Woodstar",
               "Wattled Jacana", "Unidentified Owl", "Chinese Pond-Heron", "Unidentified Raptor",
               "Red Squirrel", "Unidentified signal", "Arctic Ground Squirrel", "Unidentified Grebe",
               "Unidentified Kingbird", "Unidentified Shorebird", "Unidentified goose",
               "Wedge-tailed Sabrewing", "Unidentified Blue-winged / Golden-winged Warbler",
               "Boreal Chorus Frog", "Unidentified Hawk", "Solitary Vireo (Blue-headed / Cassin's / Plumbeous)",
               "Unidentified Trill", "Canadian Toad", "Unidentified Wren","Unidentified Finch",
               "Unidentified Loon","Wood Frog", "Unidentified Accipter Hawk","Unidentified Mammal",
               "Wolf", "Common Snipe", "Greater Ani", "Common Swift", "Yellow-browed Warbler",
               "Western Spindalis", "Narcissus Flycatcher", "Nutting's Flycatcher", 
               "Rufous-winged Tanager", "Worthen's Sparrow", "Blue-crowned Chlorophonia",
               "Colombian Crake", "Bachman's Warbler","Olivaceous Woodcreeper")

bam_data <- bam_data[-which(bam_data$speciesCommonName %in% to_remove), ]
# Fix Bald eagle to Bald Eagle
bam_data[which(bam_data$speciesCommonName == "Bald eagle"), "speciesCommonName"] <- "Bald Eagle"

# Change Lesser Snow Goose White-morph to Snow Goose
bam_data[which(bam_data$speciesCommonName == "Lesser Snow Goose White-morph"), "speciesCommonName"] <- "Snow Goose"

# Change Lesser Snow Goose White-morph to Snow Goose
bam_data[which(bam_data$speciesCommonName == "Lesser Snow Goose White-morph"), "speciesCommonName"] <- "Snow Goose"

# change myrtle warbler to yellow-rumped warbler
bam_data[which(bam_data$speciesCommonName == "Myrtle Warbler"), "speciesCommonName"] <- "Yellow-rumped Warbler"

# change Gray Jay to Canada Jay
bam_data[which(bam_data$speciesCommonName == "Gray Jay"), "speciesCommonName"] <- "Canada Jay"

# change Short-billed Gull to Mew Gull
bam_data[which(bam_data$speciesCommonName == "Short-billed Gull"), "speciesCommonName"] <- "Mew Gull"

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
bam_sites_bbs$year <- as.numeric(bam_sites$year) #format(dates, "%Y")
bam_sites_bbs$month <- as.numeric(bam_sites$month) #format(dates, "%m")
bam_sites_bbs$day <- as.numeric(bam_sites$day) # format(dates, "%d")
bam_sites_bbs$start_time <- as.numeric(bam_sites$start_time) # format(dates, "%H%M")
# Infer end time based on durationMethod
dur_method <- strsplit(bam_sites$durationMethod, "-")
duration <- unlist(lapply(dur_method, tail, n = 1L))
duration_num <- as.numeric(gsub("([0-9]+).*$", "\\1", duration))
bam_sites_bbs$end_time <- as.numeric(format(as.POSIXct(bam_sites$date, format = "%Y-%m-%d %H:%M:%S") + 
                                   (duration_num * 60), 
                                 "%H%M"))

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
bam_counts$year <- as.numeric(counts_by_sitedate$year)

#' Species common name to AOU number. A bit tricky since AOU numbers account
#' for a number of different subspecies and forms, some of which may not necessarily
#' be specified with BAM data. What we can do is first make a dataframe that 
#' relates species to AOU number. If a species does not have a 1-to-1 relationship
#' with an AOU number right away, then may have to reiterate on those species
#' and update their common name to match 1-to-1

aou_num <- data.frame(speciesCommonName = unique(counts_by_sitedate$speciesCommonName),
                      aou = NA)
for (i in 1:nrow(aou_num))
{
    aou <- search_species(species = aou_num$speciesCommonName[i])$aou
    if (length(aou) == 1)
    {
      aou_num$aou[i] <- aou
    }else
    {
      next
    }
}

counts_by_sitedate <- dplyr::left_join(counts_by_sitedate, aou_num,
                                       by = "speciesCommonName")
bam_counts$aou <- counts_by_sitedate$aou
bam_counts <- bam_counts[, c("route_data_id",
                             "country_num",
                             "state_num",
                             "route",
                             "rpid",
                             "year",
                             "aou",
                             "species_total",
                             "bcr",
                             "unid_combined")]

####### Output ####################################

saveRDS(bam_sites_bbs, file = "data/generated/bam_sites.RDS")
saveRDS(bam_counts, file = "data/generated/bam_counts.RDS")
