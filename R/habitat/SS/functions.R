###
# Project: Parks - Abrolhos Post-Survey
# Task:    Utility functions
##



## added requirement for crs, to clear up issues with misprojected rasters
# Author: Kingsley Griffin
Blank.Raster <- function(extent, crsobj, resolution){
  require(raster)
  rast         <- raster( )
  crs(rast)    <- crsobj
  extent(rast) <- extent(extent)
  res(rast)    <- resolution
  return(rast)
}