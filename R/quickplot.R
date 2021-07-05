
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
sw_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- sw_mpa[sw_mpa$ResName %in% c("Abrolhos", "Jurien", "Shark Bay"), ]    # just nat parks nearby
bathy  <- raster("data/spatial/raster/WA_500m_bathy.tif")                       # bathymetry
bathdf <- as.data.frame(bathy, xy = TRUE, na.rm = TRUE)
colnames(bathdf)[3] <- "Depth"

# assign mpa colours
mpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fbff85",
                                         # "Habitat Protection Zone (Lord Howe)" = "#fb9a99",
                                         # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                         "National Park Zone" = "#b2df8a",
                                         "Multiple Use Zone" = "#a6cee3",
                                         "National Park Zone" = "#b2df8a",
                                         "Recreational Use Zone" = "#ffb36b",
                                         "Sanctuary Zone" = "#b2df8a",
                                         # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                         # "Special Purpose Zone (Norfolk)" = "#21ea17",
                                         # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                         "Special Purpose Zone" = "#368ac1"
))

# build basic plot elements
p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey95", colour = "grey90") +
  geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 3/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 100, colour = "white", alpha = 2/5, weight = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 4/5, colour = "grey90") +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 4/5, colour = "grey90") +
  # scale_fill_discrete() +
  coord_sf(xlim = c(108, 116), ylim = c(-30, -22)) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank())) +
  theme_minimal() +
  mpa_cols
p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "grey95", colour = "grey90") +
  geom_sf(data = sw_mpa, alpha = 5/6, colour = "grey85") +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 108, xmax = 116, ymin = -30, ymax = -22, 
           colour = "black", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p2

# plot both 
p2 + p1 + plot_layout(widths = c(1, 2))


ggsave("figures/locplot.png", dpi = 200)


# site zoom plots

# # get sampling data
# bruvd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592", 
#                                   sheet = "2021-05_Abrolhos_stereo-BRUVs"))
# bossd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592", 
#                                   sheet = "2021-05_Abrolhos_BOSS"))


p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_raster(data = bathdf, aes(x, y, fill = Depth)) +
  scale_fill_viridis(begin = 0, end = 0.5) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 4/5, colour = "grey90") +
  geom_sf(data = sw_mpa, aes(fill = ZoneName), alpha = 4/5, colour = "grey90") +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), shape = 3) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "BOSS"), shape = 3) +
  scale_fill_discrete() +
  coord_sf(xlim = c(108, 116), ylim = c(-30, -22)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank())) +
  theme_minimal()
p1
