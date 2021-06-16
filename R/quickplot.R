
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


# get and sort spatial boundaries
aus <- ne_countries(country = "Australia", scale = "medium", 
                    returnclass = "sf")                                         # all aus coast
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- mpa[mpa$NAME == "Abrolhos Islands", ]                                 # wa abrolhos
sw_mpa <- aumpa[aumpa$NetName == "South-west", ]                                # nat sw

# build basic plot
p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_sf(data = ab_mpa, fill = "yellow", alpha = 3/5, colour = "grey80") +
  geom_sf(data = sw_mpa, aes(fill = ZoneName), alpha = 3/5, colour = "grey80") +
  coord_sf(xlim = c(108, 116), ylim = c(-40, -20), expand = FALSE)

p1

# tweak theme
themer <- theme_bw()

# final plot and save
p1 + themer

ggsave("figures/quickplot.png", dpi = 150)

