####### Script Information ########################
# Brandon P.M. Edwards
# <Project Name>
# <r-file.R>
# Created MONTH YEAR
# Last Updated MONTH YEAR

####### Import Libraries and External Files #######

library(terra)

####### Read Data #################################

####### Main Code #################################

lc <- rast("data/raw/spatial/aci/aci_2015_on.tif")
plot(lc)

pts <- matrix(c(-81.245790, 43.725063), ncol = 2)
spts <- vect(pts, crs = "+proj=longlat")
points(spts, cex = 100)
  
extract(lc, spts)

####### Output ####################################
