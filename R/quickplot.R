
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
mpa <- st_read("data/spatial/shp/WA_MPA_2018.shp")                              # all wa mpas
ab_mpa <- mpa[mpa$NAME == "Abrolhos Islands", ]

# build basic plot
p1 <- ggplot(data = aus) +
  geom_sf(fill = "grey90", colour = "grey80") +
  geom_sf(data = ab_mpa, fill = "yellow", alpha = 3/5, colour = "grey80") +
  coord_sf(xlim = c(112, 116), ylim = c(-30, -26), expand = FALSE)

p1

# tweak theme
themer <- theme_bw()

# final plot and save
p1 + themer

ggsave("figures/quickplot.png", dpi = 150)

