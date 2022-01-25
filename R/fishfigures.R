###
# Project: Parks - Abrolhos Post-Survey
# Data:    BOSS Fish data
# Task:    Fish figures - predictions
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

rm(list=ls())

# library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(cowplot)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("Abrolhos"), ]                             # just Abrolhos Aus MP
ab_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
ab_npz$parkid <- c(1:3)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
ab_npz <- st_transform(ab_npz, sppcrs)


# read in outputs from 'R/habitat_fish_model_predict.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/site_fish_predictions.rds")                       # site predictions only
spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# plotting broad maps
p1 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$p_totabund))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+
  guides(fill = "none") +
  labs(x = NULL, y = NULL, fill = "Total Abundance")
p1

p11 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Total Abundance")

p1 + p11 + plot_layout(widths = c(0.78,1), nrow = 1)

# ggsave("plots/site_total_fishabund.png", width = 10, height = 8, dpi = 160)

p2 <- ggplot() +
  geom_raster(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$p_richness))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Species Richness") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+
  guides(fill = "none")

p21 <- ggplot() +
  geom_raster(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Species Richness")

p2 + p21 + plot_layout(widths = c(0.78,1), nrow = 1)

# ggsave("plots/site_total_fishrich.png", width = 10, height = 8, dpi = 160)

# greater than legal size
p3 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$p_legal))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Greater than legal size") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+
  guides(fill = "none")

p31 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Greater than legal size")

p3 + p31 + plot_layout(widths = c(0.78,1), nrow = 1)

# ggsave("plots/site_total_legal.png", width = 10, height = 8, dpi = 160)

#smaller than legal size
p4 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$p_sublegal))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Smaller than legal size") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+
  guides(fill = "none")

p41 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Smaller than legal size")

p4 + p41 + plot_layout(widths = c(0.78,1), nrow = 1)

# ggsave("plots/site_total_sublegal.png", width = 10, height = 8, dpi = 160)


gg.predictions <- (p1+p11)/(p2+p21)/(p3+p31)/(p4+p41)
gg.predictions
ggsave("plots/site_fish_predictions.png", gg.predictions,width = 12, height = 8, dpi = 160)


# 
# p5 <- ggplot() +
#   geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_lminatus)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$p_lminatus))) +
#   geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
#   theme_minimal() +
#   labs(x = NULL, y = NULL, fill = "L. minatus\n(MaxN)") +
#   guides(fill = "none")
# 
# p51 <- ggplot() +
#   geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_lminatus)) +
#   scale_fill_viridis(direction = -1) +
#   geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
#   theme_minimal() +
#   labs(x = NULL, y = NULL, fill = "L. minatus\n(MaxN)")
# 
# (p3 + p31) / (p4 + p41) / (p5 + p51) + 
#   plot_layout(widths = c(0.78,1))

# ggsave("plots/site_maxn_topsp.png", width = 10, height = 8, dpi = 160)
