
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
library(googlesheets4)
library(patchwork)
library(raster)
library(ggnewscale)

# get and sort spatial boundaries
aus    <- st_read("data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Abrolhos Islands", #"Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby
sw_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- sw_mpa[sw_mpa$ResName %in% c("Abrolhos", "Jurien", "Shark Bay"), ]    # just nat parks nearby
cwatr  <- readRDS('output/coastal_waters_limit_trimmed.rds')                    # coastal waters line trimmed in 'R/GA_coast_trim.R'
bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)

# simplify state parks names
ab_mpa$waname <- gsub("( \\().+(\\))", "", ab_mpa$ZONE_TYPE)
ab_mpa$waname <- gsub(" [1-4]", "", ab_mpa$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <- 
#   c("Special Purpose Zone\n(Habitat Protection)")

ab_mpa$waname[ab_mpa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
ab_mpa$waname[ab_mpa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"

ab_mpa$waname <- dplyr::recode(ab_mpa$waname, 
                               "General Use" = "General Use Zone",
                               # "Unassigned" = "Fish Habitat Protection Area",
                               # "MMA" = "Marine Management Area",
                               "Recreation Area" = "Recreation Zone",
                               # "Conservation Area" = "Sanctuary Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                                 "Special Purpose Zone\n(Shore Based Activities)")


# assign mpa colours
nmpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
                                          # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                          "National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Recreational Use Zone" = "#ffb36b",
                                          "Sanctuary Zone" = "#f7c0d8",
                                          # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                          "Special Purpose Zone" = "#6daff4"
))

wampa_cols <- scale_fill_manual(values = c(# "Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                           "Fish Habitat Protection Area" = "#fbff85",
                                           # "National Park Zone" = "#a4d194",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Sanctuary Zone" = "#bfd054",
                                           "Marine Nature Reserve" = "#bfd054",
                                           "Conservation Area" = "#b3a63d",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
                                           # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
                                           "Unassigned/Unclassified" = "#ddccff",
                                           "Marine Management Area" = "#b7cfe1"
))

# build basic plot elements
p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 3/5, colour = NA) +
  wampa_cols + 
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  nmpa_cols + 
  labs(x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 112.5, xmax = 114.5, ymin = -28.3, ymax = -27, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  coord_sf(xlim = c(108.5, 115.5), ylim = c(-29.5, -24.5)) +
  theme_minimal()
# p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = sw_mpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 108.5, xmax = 115.5, ymin = -29.5, ymax = -24, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("figures/locplot.png", dpi = 200, width = 12, height = 9)


# site zoom plots

# get sampling data
bossd <- readRDS('data/2105_abrolhos_boss.rds')
bruvd <- readRDS('data/2105_abrolhos_bruv.rds')

# reduce mpa levels for these plots

snmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone" = "#6daff4"
))

swampa_cols <- scale_fill_manual(values = c(
  "Fish Habitat Protection Area" = "#fbff85"
))

# closer plot
sitebathy <- readRDS('output/ga_bathy_fine.rds')                                # finer bathy
colnames(sitebathy)[3] <- "Depth"
sitebathy <- sitebathy[sitebathy$Depth > -3100, ]                               # trim to reduce legend

p3 <- ggplot() +
  geom_raster(data = sitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 3/5, colour = NA) +
  swampa_cols + labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  snmpa_cols + labs(x = NULL, y = NULL, fill = "Commonwealth") +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  annotate("rect", xmin = 112.95, xmax = 113.4, ymin = -27.2, ymax = -27.0, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.15, y = -27.05, size = 3, 
           colour = "grey20", label = "Fig. 3.1. swabrnpz09") +
  annotate("rect", xmin = 113.2, xmax = 113.7, ymin = -28.2, ymax = -27.9,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.45, y = -27.95, size = 3,
           colour = "grey20", label = "Fig. 3.2. swabrnpz06") +
  coord_sf(xlim = c(112.5, 114.5), ylim = c(-28.2, -27)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  theme_minimal()
p3

ggsave("figures/siteplot.png", dpi = 200, width = 10, height = 8)

## single site zoom plots

snmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb"
))

ssitebathy <- sitebathy[sitebathy$Depth > -200, ]                               # trim to reduce legend

p4 <- ggplot() +
  geom_raster(data = ssitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = ssitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 10, colour = "white", alpha = 4/5, size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 1/5, colour = NA) +
  snmpa_cols + labs(x = NULL, y = NULL, fill = "Commonwealth") +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "navyblue")) +
  coord_sf(xlim = c(113.02, 113.28), ylim = c(-27.2, -27.05)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  theme_minimal()
p4
ggsave("figures/nthsite.png", dpi = 200, width = 10, height = 8)

snmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone" = "#6daff4"
))

ssitebathy <- sitebathy[sitebathy$Depth > -400, ]                               # trim to reduce legend

p5 <- ggplot() +
  geom_raster(data = ssitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = ssitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 10, colour = "white", alpha = 4/5, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 1/5, colour = NA) +
  snmpa_cols + labs(x = NULL, y = NULL, fill = "Commonwealth") +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "navyblue")) +
  coord_sf(xlim = c(113.24, 113.58), ylim = c(-28.13, -28.02)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  theme_minimal()
p5

ggsave("figures/sthsite.png", dpi = 200, width = 10, height = 8)


