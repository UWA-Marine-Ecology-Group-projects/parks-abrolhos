
###
# Project: Parks - Abrolhos Post-Survey
# Data:    GA Coarse Bathy/elevation data
# Task:    Trim down huge GA raster
# author:  Kingsley Griffin
# date:    Jul 2021
##

library(raster)
library(sf)
library(sp)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/raster", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 5 & cbathy$X < 117, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

# aggregate raster to reduce size and plotting time etc
aggbath  <- aggregate(bath_r, 10)
abath_df <- as.data.frame(aggbath, xy = TRUE, fun = max, na.rm = TRUE)
saveRDS(abath_df, 'output/ga_bathy_trim.rds')

# trim down coastal waters line
cwatr <- st_read("data/spatial/shp/amb_coastal_waters_limit.shp")
cwatr <- st_crop(cwatr, c(xmin = 107, xmax = 117, ymin = -31, ymax = -21))      # crop to general project area
saveRDS(cwatr, 'output/coastal_waters_limit_trimmed.rds')

# retain finer bathy near survey area
fbath    <- crop(bath_r, extent(c(111, 114.5, -29, -26)))
fbath_df <- as.data.frame(fbath, xy = TRUE)
saveRDS(fbath_df, 'output/ga_bathy_fine.rds')

# transform bathy to projected coords for modelling
wgscrs  <- CRS("+proj=longlat +datum=WGS84")
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
proj4string(fbath) <- wgscrs
fbath_t <- projectRaster(fbath, crs = sppcrs)

# calculate terrain on fine bathy
preds <- terrain(fbath_t, neighbors = 8,
                 opt = c("slope", "aspect", "TPI", "TRI", "roughness"))
preds <- stack(fbath_t, preds)
saveRDS(preds, "data/spatial/spatial_covariates.rds")

# clear workspace of large rasters etc
rm(ls())