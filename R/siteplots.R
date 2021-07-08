
###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Kingsley Griffin
# date:    Jun 2021
##

library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(viridis)
library(googlesheets4)
library(patchwork)
library(raster)
library(ggnewscale)

# get and sort spatial boundaries
aus <- ne_countries(country = "Australia", scale = "medium", 
                    returnclass = "sf")                                         # all aus coast
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Abrolhos Islands", "Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby
ab_mpa$waname <- gsub("( \\().+(\\))", "", ab_mpa$ZONE_TYPE)
ab_mpa$waname <- gsub(" [1-4]", "", ab_mpa$waname)
ab_mpa$waname[ab_mpa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
sw_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- sw_mpa[sw_mpa$ResName %in% c("Abrolhos", "Jurien", "Shark Bay"), ]    # just nat parks nearby
bathdf <- readRDS("output/spatial/raster/ga_bathy_trim.rds")                    # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"



# assign mpa colours
mpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fbff85",
                                         "Fish Habitat Protection Area" = "#fbff85",
                                         # "Habitat Protection Zone (Lord Howe)" = "#fb9a99",
                                         # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                         "National Park Zone" = "#b2df8a",
                                         "Multiple Use Zone" = "#a6cee3",
                                         "General Use" = "#a6cee3",
                                         "National Park Zone" = "#b2df8a",
                                         "Recreational Use Zone" = "#ffb36b",
                                         "Recreational Zone" = "#ffb36b",
                                         "Recreation Area" = "#ffb36b",
                                         "Sanctuary Zone" = "#b2df8a",
                                         # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                         # "Special Purpose Zone (Norfolk)" = "#21ea17",
                                         # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                         "Special Purpose Zone" = "#368ac1",
                                         "Unassigned" = "#368ac1"
))

# build basic plot elements
p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "grey85", colour = "grey80") +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), 
          alpha = 3/5, colour = "grey90", size = 0.1) +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), 
          alpha = 4/5, colour = "grey90", size = 0.15) +
  mpa_cols +
  annotate("rect", xmin = 112.5, xmax = 114.5, ymin = -28.3, ymax = -27, 
           colour = "grey25", alpha = 1/5, size = 0.2) +
  coord_sf(xlim = c(108, 116), ylim = c(-30, -22)) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank())) +
  theme_minimal()
p1

  # inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "grey95", colour = "grey90") +
  geom_sf(data = sw_mpa, alpha = 5/6, colour = "grey85", size = 0.05) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 108, xmax = 116, ymin = -30, ymax = -22, 
           colour = "grey25", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("figures/locplot.png", dpi = 200)


# site zoom plots

# get sampling data
bruvd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
                                  sheet = "2021-05_Abrolhos_stereo-BRUVs"))
bossd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
                                  sheet = "2021-05_Abrolhos_BOSS"))

# assign mpa colours
smpa_cols <- scale_fill_manual(values = c(#"Habitat Protection Zone" = "#fbff85",
                                         # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                         "Fish Habitat Protection Area" = "#fbff85",
                                         "National Park Zone" = "#b2df8a",
                                         "Multiple Use Zone" = "#a6cee3",
                                         "National Park Zone" = "#b2df8a",
                                         "General Use" = "#a6cee3",
                                         # "Recreational Use Zone" = "#ffb36b",
                                         # "Sanctuary Zone" = "#b2df8a",
                                         # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                         # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                         "Special Purpose Zone" = "#368ac1",
                                         "Unassigned" = "#368ac1"
))

lbathdf <- bathdf[bathdf$Depth > -2500, ]
p3 <- ggplot() +
  geom_raster(data = lbathdf, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  guides(fill = guide_legend(override.aes = list(alpha = 4/5))) +
  geom_contour(data = lbathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "grey90", colour = "grey80") +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), 
          alpha = 3/5, colour = "grey90", size = 0.1) +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), 
          alpha = 4/5, colour = "grey90", size = 0.15) +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), shape = 3) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "BOSS"), shape = 3) +
  coord_sf(xlim = c(112.5, 114.5), ylim = c(-28.2, -27)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank(), 
                             override.aes = list(alpha = 4/5))) +
  theme_minimal() +
  smpa_cols
p3

ggsave("figures/siteplot.png", dpi = 200)

