rm(list = ls())

library(terra)
library(tidyverse)

spreddf <- readRDS("output/site_habitat_predictions.rds") %>%
  dplyr::select(x, y, dom_tag) %>%
  dplyr::mutate(dom_tag = as.integer(factor(dom_tag))) %>%
  # glimpse()
  rast(crs = "EPSG:32750")
plot(spreddf)

writeRaster(spreddf, filename = "output/fssgam/abrolhos_habitat.tif", overwrite = T)
