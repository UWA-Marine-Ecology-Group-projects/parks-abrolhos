
###
# Project: Parks - Abrolhos Post-Survey
# Data:    GA Coarse Bathy/elevation data
# Task:    Trim down huge GA raster
# author:  Kingsley Griffin
# date:    Jul 2021
##

library(raster)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/raster", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= -1 & cbathy$X < 117, ]
# cbathy <- cbathy[, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

# aggregate raster to reduce size and plotting time etc
aggbath <- aggregate(bath_r, 10)

abath_df <- as.data.frame(aggbath, xy = TRUE)

saveRDS(abath_df, 'output/spatial/raster/ga_bathy_trim.rds')
