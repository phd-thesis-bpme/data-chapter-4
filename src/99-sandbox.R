library(sf)
library(ggplot2)
library(ggpubr)
library(magrittr)
theme_set(theme_pubclean())

load("data/raw/samples.rda")

laea <- 4326 # sf::st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area coord reference system
sf::sf_use_s2(FALSE)

strata <- read_sf("data/raw/latlong_strata.gpkg") %>%
  st_transform(crs = laea)
stops <- read_sf("data/raw/bbs-routes-stops/",
                 layer = "CURRENT_STOPS_2022") %>%
  st_transform(crs = laea)
routes <- read_sf("data/raw/bbs-routes-stops/",
                  layer = "ALLROUTES_2022") %>%
  st_transform(crs = laea)

bam_coords <- project_samples[, c("Latitude", "Longitude")]
bam_coords <- bam_coords[!is.na(bam_coords$Latitude), ]
bam_coords <- bam_coords[!is.na(bam_coords$Longitude), ]
bam_coords <- st_as_sf(bam_coords,
                       coords = c("Longitude","Latitude"), crs = laea)

ggplot() + 
  xlim(-100,-75) + ylim(45,60) +
  geom_sf(data = strata) +
  geom_sf(data = stops, size = 0.3) +
  geom_sf(data = bam_coords, color = "blue", size = 0.3)

################################################################

library(bbsBayes2)

