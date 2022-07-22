
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
library(metR)
library(googlesheets4)
library(patchwork)
library(raster)
library(ggnewscale)
library(maptools)

# get and sort spatial boundaries
aus    <- st_read("data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                        # just dirk hartog island
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
terrnp <- st_read("data/spatial/shp/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
st_crs(aus)         <- st_crs(aumpa)
st_crs(dirkh)       <- st_crs(aumpa) 

roas <- st_read("data/spatial/shp/Abrolhos_ROAs.shp")                           # from matt's state reserve shapefile 

# simplify state parks names
ab_mpa$waname <- gsub("( \\().+(\\))", "", ab_mpa$ZONE_TYPE)
ab_mpa$waname <- gsub(" [1-4]", "", ab_mpa$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <- 
#   c("Special Purpose Zone\n(Habitat Protection)")

ab_mpa$waname[ab_mpa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
ab_mpa$waname[ab_mpa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"

ab_mpa$waname <- dplyr::recode(ab_mpa$waname, 
                               "General Use" = "General Use Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                                 "Special Purpose Zone\n(Shore Based Activities)",
                               "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = "Special Purpose Zone",
                               )

# reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 113, ymin = -30, xmax = 116, ymax = -26)       # just abrolhos area
# plot(terrnp["leg_catego"])

#Key Ecological Features
kef <- st_read("data/spatial/shp/AU_DOEE_KEF_2015.shp")
sf_use_s2(F)                                                              
kef <- st_crop(kef, c(xmin = 109, xmax = 116, ymin = -30, ymax = -24))  
kef$NAME <- dplyr::recode(kef$NAME,"Perth Canyon and adjacent shelf break, and other west coast canyons" = "West coast canyons",                 
                          "Commonwealth marine environment within and adjacent to the west coast inshore lagoons" = "West coast lagoons",                
                          "Ancient coastline at 90-120m depth" = "Ancient coastline",                                                   
                          "Western demersal slope and associated fish communities" = "Western demersal fish",                               
                          "Western rock lobster" = "Western rock lobster",
                          "Commonwealth marine environment surrounding the Houtman Abrolhos Islands" = "Abrolhos Islands")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone" = "#6daff4",
                                          "Habitat Protection Zone" = "#fff8a3"
))

wampa_cols <- scale_fill_manual(values = c("Fish Habitat Protection Area" = "#fac86b",
                                           "Reef Observation Area" = "#ddccff",
                                           "Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           "Marine Nature Reserve" = "#bfd054"
))

# WA terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"))

# build basic plot elements
p1 <- ggplot() +
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, - 7000)) +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
  breaks = c(-30, -70, -200, - 700, - 7000), colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = dirkh, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ab_mpa, aes(fill = waname), alpha = 2/5, colour = NA) +
  geom_sf(data = roas, aes(fill = Type), alpha = 2/5, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 112.8, xmax = 114.2, ymin = -28.1, ymax = -27.05,
           colour = "grey15", fill = "white", alpha = 0.1, size = 0.1) +
  annotate("point", y = c(-28.7761, - 27.7115), x = c(114.6113, 114.1714), size = 0.75) +
  annotate("text", y = c(-28.7761, - 27.7115), x = c(115, 114.48),
           label = c("Geraldton", "Kalbarri"), size = 3) +
  coord_sf(xlim = c(109.4, 115.0607), ylim = c(-29.25, -24.4)) +
  theme_minimal()
p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = sw_mpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 108.9, xmax = 115.0607, ymin = -29.4, ymax = -24.2, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("plots/spatial/locplot.png", dpi = 200, width = 10, height = 6)


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
sitebathy <- sitebathy[sitebathy$Depth > -1000, ]                               # trim to reduce legend
sitebathy <- sitebathy[sitebathy$x > 112.5 & sitebathy$x < 114.4, ]
sitebathy <- sitebathy[sitebathy$y > -28.4 & sitebathy$y < -26.8, ]

p3 <- ggplot() +
  geom_raster(data = sitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70", guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = ab_nmp, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  snmpa_cols + labs(x = NULL, y = NULL, fill = "Australian Marine Park") +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 50, colour = "white", alpha = 4/5, size = 0.1) +
  geom_text_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
                    binwidth = 100, size = 2.5, label.placer = label_placer_n(1)) +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  annotate("rect", xmin = 113.02, xmax = 113.29, ymin = -27.19, ymax = -27.08,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.15, y = -27.05, size = 3, 
           colour = "grey20", label = "swabrnpz09") +
  annotate("rect", xmin = 113.24, xmax = 113.58, ymin = -28.13, ymax = -28.02,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.42, y = -27.99, size = 3,
           colour = "grey20", label = "swabrnpz06") +
  coord_sf(xlim = c(112.8, 114.2), ylim = c(-28.1, -27.05)) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  theme_minimal()
p3

ggsave("plots/siteplot.png", dpi = 200, width = 8, height = 6)

## single site zoom plots
snmpa_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63"))

nsitebathy <- sitebathy[sitebathy$Depth > -160, ]                               # trim to reduce legend
nsitebathy <- nsitebathy[nsitebathy$x > 113 & nsitebathy$x < 113.3, ]
nsitebathy <- nsitebathy[nsitebathy$y > -27.2 & nsitebathy$y < -27.05, ]


p4 <- ggplot() +
  geom_raster(data = nsitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70", guide = "none") +
  geom_contour(data = nsitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 10, colour = "white", alpha = 1, size = 0.1) +
  geom_text_contour(data = nsitebathy, aes(x = x, y = y, z = Depth), 
                    binwidth = 10, size = 2.5,
                    label.placer = label_placer_n(1)) +
  geom_sf(data = ab_nmp, aes(colour = ZoneName), alpha = 4/5, fill = NA) +
  snmpa_cols + 
  labs(x = NULL, y = NULL, colour = NULL) +
  new_scale_colour() +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "navyblue")) +
  coord_sf(xlim = c(113.02, 113.28), ylim = c(-27.18, -27.08)) +
  labs(colour = NULL, x = NULL, y = NULL) +
  theme_minimal()
p4
ggsave("plots/nthsite.png", dpi = 200, width = 7, height = 4)


snmpa_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63"
                                          #  "Multiple Use Zone" = "#b9e6fb",
                                          # "Special Purpose Zone" = "#6daff4"
))
sab_nmp <- ab_nmp[ab_nmp$ZoneName == "National Park Zone", ]

ssitebathy <- sitebathy[sitebathy$Depth < -10 & sitebathy$Depth > -380, ]                               # trim to reduce legend
ssitebathy <- ssitebathy[ssitebathy$x > 113.23 & ssitebathy$x < 113.6, ]
ssitebathy <- ssitebathy[ssitebathy$y > -28.15 & ssitebathy$y < -28, ]

p5 <- ggplot() +
  geom_raster(data = ssitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70", guide = "none") +
  geom_contour(data = ssitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 20, colour = "white", alpha = 4/5, size = 0.1) +
  geom_text_contour(data = ssitebathy, aes(x = x, y = y, z = Depth), 
                    binwidth = 20, size = 2.5, label.placer = label_placer_n(1)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = sab_nmp, aes(colour = ZoneName), alpha = 1, fill = NA) +
  snmpa_cols + 
  labs(x = NULL, y = NULL, colour = NULL) +
  new_scale_colour() +
  geom_point(data = bruvd, aes(Longitude, Latitude, colour = "BRUV"), 
             alpha = 3/5, shape = 10) +
  geom_point(data = bossd, aes(Longitude, Latitude, colour = "Drop Camera"), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "navyblue")) +
  coord_sf(xlim = c(113.24, 113.58), ylim = c(-28.125, -28.03)) +
  labs(colour = NULL, x = NULL, y = NULL) +
  theme_minimal()
p5

ggsave("plots/sthsite.png", dpi = 200, width = 7, height = 4)

# Key Ecological Features plot
# KEF colours
kef_cols <- scale_fill_manual(values = c("Ancient coastline" = "#ff6db6",                             
                                         "Western rock lobster" = "#6db6ff",
                                         "West coast canyons" = "#21828b",
                                         "West coast lagoons" = "#188e8e",
                                         "Abrolhos Islands" = "#2bf446",
                                         "Western demersal fish" = "#016dda",
                                         "Wallaby Saddle" = "#940000"))

# Reorder KEFs so they plot in a sensible order
kef$NAME <- factor(kef$NAME, levels = c("Western rock lobster", "Western demersal fish", "Wallaby Saddle", 
                                        "Abrolhos Islands", "Ancient coastline", 
                                        "West coast canyons", "West coast lagoons"))

# Merge the zones into one polygon for each park
ab_nmpspat <- as_Spatial(ab_nmp)
ab_nmpmerge <- unionSpatialPolygons(ab_nmpspat, ab_nmpspat$ResName)
ab_nmpmerge <- as(ab_nmpmerge, "sf")

# build basic plot elements
p7 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = kef, aes(fill = NAME), alpha = 0.7, color = NA) +
  kef_cols+
  # geom_sf(data = ab_mpa, fill = NA, alpha = 2/5, colour = "black", show.legend = F, size = 0.2) +
  geom_sf(data = ab_nmpmerge, fill = NA, alpha = 2/5, color = "black", show.legend = F, size = 0.2) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
  guides(fill = guide_legend(order = 1)) +
  coord_sf(xlim = c(109.4, 115.0607), ylim = c(-29.25, -24.4)) + 
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(filename = "plots/spatial/key-ecological-features.png", height = 4, width = 6,
    units = "in", res = 200)
p7
dev.off()


## saving all colours for later

# 
# # assign mpa colours
# nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
#                                           "Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
#                                           # "Habitat Protection Zone (Reefs)" = "#fbff85",
#                                           "Multiple Use Zone" = "#b9e6fb",
#                                           # "Recreational Use Zone" = "#ffb36b",
#                                           # "Sanctuary Zone" = "#f7c0d8",
#                                           # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
#                                           # "Special Purpose Zone (Trawl)" = "#3e8ec4",
#                                           "Special Purpose Zone" = "#6daff4"
# ))
# 
# wampa_cols <- scale_fill_manual(values = c("Fish Habitat Protection Area" = "#fbff85",
#                                            # "Sanctuary Zone" = "#bfd054",
#                                            # "Marine Nature Reserve" = "#bfd054",
#                                            # "Conservation Area" = "#b3a63d",
#                                            # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
#                                            # "National Park Zone" = "#a4d194",
#                                            # "General Use Zone" = "#bddde1",
#                                            # "Recreation Zone" = "#f4e952",
#                                            # "Special Purpose Zone" = "#c5bcc9",
#                                            # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
#                                            # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
#                                            # "Marine Management Area" = "#b7cfe1",
#                                            "Reef Observation Area" = "#ddccff"
# ))
