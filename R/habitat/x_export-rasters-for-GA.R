library(tidyverse)
library(raster)
library(terra)
library(tidyterra)

# Habitat
site_habitat_predictions <- readRDS("output/site_habitat_predictions.rds") %>%
  dplyr::select(x, y, pbiogenic.fit, pbiogenic.se.fit) %>%
  rast(crs = "epsg:32750")
plot(site_habitat_predictions)

writeRaster(site_habitat_predictions, paste0("output/fssgam/abrolhos_", names(site_habitat_predictions), ".tif"), overwrite = TRUE)

# Fish
site_fish_predictions <- readRDS("output/fssgam - fish/abrolhos-fish-spatial_UTM50.rds") %>%
  rast() %>%
  select(p_legal6.fit, p_legal6.se.fit)
plot(site_fish_predictions)

writeRaster(site_fish_predictions, paste0("output/fssgam - fish/abrolhos_", names(site_fish_predictions), ".tif"), overwrite = TRUE)
