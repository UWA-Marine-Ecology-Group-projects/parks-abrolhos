
###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS
# Task:    Quick plots
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
ab_mpa$waname <- paste("(WA)", ab_mpa$NAME)
sw_mpa <- aumpa[aumpa$NetName == "South-west", ]                                # just nat sw
ab_nmp <- sw_mpa[sw_mpa$ResName %in% c("Abrolhos", "Jurien"), ]                 # just nat parks nearby
bathy  <- raster("data/spatial/raster/WA_500m_bathy.tif")                       # bathymetry
bathdf <- as.data.frame(bathy, xy = TRUE, na.rm = TRUE)
colnames(bathdf)[3] <- "Depth"

# build basic plot elements
p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_raster(data = bathdf, aes(x, y, fill = Depth)) +
  scale_fill_viridis(begin = 0, end = 0.5) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 4/5, colour = "grey90") +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 4/5, colour = "grey90") +
  scale_fill_discrete() +
  coord_sf(xlim = c(108, 116), ylim = c(-30, -22)) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank())) +
  theme_minimal()
p1

## need to fix mpa colour scheming
## eg script for customising parks colours
# scale_fill_manual(values = c("A" = "yellow"), 
#                   guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
#   scale_colour_manual(values = c("B" = "pink", "C" = "purple"),
#                       guide = guide_legend(override.aes = list(linetype = c("blank", "solid"), 
#                                                                shape = c(16, NA)))) +


# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_sf(data = sw_mpa, fill = "grey80", alpha = 4/5, colour = "white")+
  geom_sf(data = ab_mpa, fill = "grey80", alpha = 4/5, colour = "white") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -10)) +
  annotate("rect", xmin = 108, xmax = 116, ymin = -30, ymax = -22, 
           colour = "black", alpha = 1/5, size = 0.2) +
  theme_minimal() +
  theme(axis.text = element_blank())
p2

# plot both 
p2 + p1 + plot_layout(widths = c(1, 2))


ggsave("figures/quickplot.png", dpi = 200)


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
