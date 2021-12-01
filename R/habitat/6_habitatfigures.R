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
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Abrolhos Islands", #"Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby


# read in outputs from 'R/4_habitat_model.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/site_habitat_predictions.rds")                       # site predictions only
spreddf$dom_tag <- as.factor(spreddf$dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                          pkelps = "Kelp",
                          pmacroalg = "Macroalgae",
                          prock = "Rock",
                          psand = "Sand",
                          psponge = "Sponge")
  
spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Kelp" = "goldenrod1",# Commonwealth MPA colours
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Sponge" = "plum"
))

p1 <- ggplot(spreddf[spreddf$sitens == 1, ], aes(x, y)) +
  geom_tile(aes(fill = dom_tag)) +
  hab_cols +
  # geom_sf(data = ab_mpa, aes(x, y), alpha = 3/5) +
  # wampa_cols + 
  # labs(fill = "State") 
  labs(x = NULL, y = NULL) +
  coord_equal() +
  guides(fill = "none") +
  theme_minimal()

p11 <- ggplot(spreddf[spreddf$sitens == 0, ], aes(x, y)) +
  geom_tile(aes(fill = dom_tag)) +
  hab_cols +
  labs(x = NULL, y = NULL, fill = "Habitat") +
  coord_equal() +
  theme_minimal()

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
                                    psponge = "Sponge")

p2 <- ggplot(widehabit[widehabit$sitens == 1, ], aes(x, y)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL) +
  coord_equal() +
  theme_minimal() +
  guides(fill = "none") +
  facet_wrap(~variable, ncol = 1) + 
  theme(axis.text = element_blank())

p22 <- ggplot(widehabit[widehabit$sitens == 0, ], aes(x, y)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Habitat (p)") +
  coord_equal() +
  theme_minimal() +
  facet_wrap(~variable, ncol = 1) + 
  theme(axis.text = element_blank())

p2 + p22 + plot_layout(widths = c(0.48, 0.52))
ggsave("plots/site_habitat_predicted.png", width = 8, height = 14, dpi = 160)

# fig 3: biogenic reef
p3 <- ggplot(spreddf[widehabit$sitens == 1, ], aes(x, y)) +
  geom_tile(aes(fill = pbiogenic)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL) +
  coord_equal() +
  guides(fill = "none") +
  theme_minimal()

p32 <- ggplot(spreddf[widehabit$sitens == 0, ], aes(x, y)) +
  geom_tile(aes(fill = pbiogenic)) +
  scale_fill_viridis(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Biogenic\nReef (p)") +
  coord_equal() +
  theme_minimal()

p3 + p32
ggsave("plots/site_biogenicreef_p.png", width = 10, height = 6, dpi = 160)

# fig 4: predicted relief
pcelldf <- readRDS('output/predicted_relief.rds')
pcelldf$sitens <- ifelse(pcelldf$y > 6940000, 1, 0)
pcelldf$prelief[pcelldf$prelief < 0] <- 0

p4 <- ggplot(pcelldf[pcelldf$sitens == 1, ], aes(x, y)) +
  geom_tile(aes(fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1) +
  coord_equal() +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  theme_minimal()

p42 <- ggplot(pcelldf[pcelldf$sitens == 0, ], aes(x, y)) +
  geom_tile(aes(fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1) +
  coord_equal() +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  theme_minimal()

p4 + p42
ggsave("plots/site_relief_p.png", width = 10, height = 6, dpi = 160)

