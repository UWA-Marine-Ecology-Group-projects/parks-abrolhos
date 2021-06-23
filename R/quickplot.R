
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
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(googlesheets4)
library(patchwork)

# get and sort spatial boundaries
aus <- ne_countries(country = "Australia", scale = "medium", 
                    returnclass = "sf")                                         # all aus coast
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME == "Abrolhos Islands", ]                                 # wa abrolhos
sw_mpa <- aumpa[aumpa$NetName == "South-west", ]                                # nat sw

# get sampling data
bruvd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592", 
                                  sheet = "2021-05_Abrolhos_stereo-BRUVs"))
bossd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592", 
                                  sheet = "2021-05_Abrolhos_BOSS"))

# fix mpa colour scheming

# build basic plot elements
p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_sf(data = ab_mpa, aes(fill = "WA Fish Habitat Protection Zone"), alpha = 4/5, colour = "grey90") +
  geom_sf(data = sw_mpa, aes(fill = ZoneName), alpha = 4/5, colour = "grey90") +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), shape = 3) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "BOSS"), shape = 3) +
  coord_sf(xlim = c(108, 116), ylim = c(-30, -20)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  guides(fill = guide_legend(element_blank())) +
  theme_minimal()

# customise 
# scale_fill_manual(values = c("A" = "yellow"), 
#                   guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
#   scale_colour_manual(values = c("B" = "pink", "C" = "purple"),
#                       guide = guide_legend(override.aes = list(linetype = c("blank", "solid"), 
#                                                                shape = c(16, NA)))) +


# inset map

p2 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  coord_sf(xlim = c(108, 130), ylim = c(-37, -10)) +
  annotate("rect", xmin = 108, xmax = 116, ymin = -30, ymax = -20, colour = "black", alpha = 1/5, size = 0.2) +
  theme_minimal() +
  theme(axis.text = element_blank())
p2

# plot both
p2 + p1 + plot_layout(widths = c(1, 2))


ggsave("figures/quickplot.png", dpi = 150)
