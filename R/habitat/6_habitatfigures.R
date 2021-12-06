###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat figures
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("Abrolhos"), ]                             # just Abrolhos Aus MP
ab_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
ab_npz$parkid <- c(1:3)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
ab_npz <- st_transform(ab_npz, sppcrs)
habi   <- readRDS("data/tidy/merged_habitat.rds")
habi$ns <- ifelse(habi$Latitude.1 > 6940000, 1, 0)
habi$method <- dplyr::recode(habi$method,
                             BOSS = "Drop Camera")

# read in outputs from 'R/4_habitat_model.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/site_habitat_predictions.rds")                       # site predictions only
spreddf$dom_tag <- as.factor(spreddf$dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                          pkelps = "Kelp",
                          pmacroalg = "Macroalgae",
                          prock = "Rock",
                          psand = "Sand",
                          pbiogenic = "Biogenic Reef")
  
spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Kelp" = "goldenrod1",# Commonwealth MPA colours
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Biogenic Reef" = "plum"
))

p1 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 1, ], aes(Longitude.1, Latitude.1, colour = method), 
             shape = 10, size = 1, alpha = 3/5) +
  scale_colour_manual(values = c("BRUV" = "indianred4", "Drop Camera" = "navyblue")) +
  labs(x = NULL, y = NULL) +
  guides(fill = "none", colour = "none") +
  theme_minimal() + theme(axis.text = element_blank())

p11 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 0, ], aes(Longitude.1, Latitude.1, colour = method), 
             shape = 10, size = 1, alpha = 3/5) +
  scale_colour_manual(values = c("BRUV" = "indianred4", "Drop Camera" = "navyblue")) +
  labs(x = NULL, y = NULL, fill = "Habitat", colour = NULL) +
  theme_minimal() + theme(axis.text = element_blank())

p1 + p11
ggsave("plots/site_dominant_habitat.png", width = 12, height = 8, dpi = 160)

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- melt(spreddf, measure.vars = c(12:16))
widehabit$variable <- dplyr::recode(widehabit$variable,
                                    pkelps = "Kelp",
                                    pmacroalg = "Macroalgae",
                                    prock = "Rock",
                                    psand = "Sand",
                                    pbiogenic = "Biogenic Reef")

p2 <- ggplot() +
  geom_tile(data = widehabit[widehabit$sitens == 1, ], aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  guides(fill = "none") +
  facet_wrap(~variable, ncol = 1) + 
  theme(axis.text = element_blank())

p22 <- ggplot() +
  geom_tile(data = widehabit[widehabit$sitens == 0, ], aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL, fill = "Habitat (p)") +
  theme_minimal() +
  facet_wrap(~variable, ncol = 1) + 
  theme(axis.text = element_blank())

p2 + p22 + plot_layout(widths = c(0.84, 1))
ggsave("plots/site_habitat_predicted.png", width = 8, height = 14, dpi = 160)

# # fig 3: biogenic reef
# p3 <- ggplot(spreddf[widehabit$sitens == 1, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL) +
#   coord_equal() +
#   guides(fill = "none") +
#   theme_minimal()
# 
# p32 <- ggplot(spreddf[widehabit$sitens == 0, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL, fill = "Biogenic\nReef (p)") +
#   coord_equal() +
#   theme_minimal()
# 
# p3 + p32 + plot_layout(widths = c(0.46, 0.54))
# ggsave("plots/site_biogenicreef_p.png", width = 10, height = 6, dpi = 160)

# fig 4: predicted relief
pcelldf <- readRDS('output/predicted_relief_site.rds')
pcelldf$sitens <- ifelse(pcelldf$y > 6940000, 1, 0)
pcelldf$prelief[pcelldf$prelief < 0] <- 0

p4 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 1, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  guides(fill = "none") +
  theme_minimal()

p42 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 0, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  theme_minimal()

p4 + p42 + plot_layout(widths = c(0.44, 0.56))
ggsave("plots/site_relief_p.png", width = 10, height = 6, dpi = 160)

