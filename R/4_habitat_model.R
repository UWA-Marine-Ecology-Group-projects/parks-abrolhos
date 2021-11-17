###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Basic habitat models
# author:  Kingsley Griffin
# date:    Sept-Oct 2021
##

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
habi   <- readRDS("data/tidy/merged_habitat.rds")                               # merged data from 'R/1_mergedata.R'
preds  <- readRDS("data/spatial/spatial_covariates.rds")                        # spatial covs from 'R/1_mergedata.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$Depth <- preddf$Z * -1

# reduce predictor space to fit survey area
preddf <- preddf[preddf$Depth > min(habi$Depth), ]
preddf <- preddf[preddf$Depth < 200, ]
spredf <- preddf[(preddf$y > 6880000 & preddf$y < 6900000 & preddf$x > 130000 & preddf$x < 165000) | 
                   (preddf$y > 6986000 & preddf$y < 7000000 & preddf$x > 105000 & preddf$x < 130000), ]

# summarise classes ----
brfexcl <- c(1:10, 30, 38:ncol(habi), 
             grep("Unconsolidated", colnames(habi)),
             grep("Consolidated", colnames(habi)))  # collect biogenic reef colnames
brfc <- colnames(habi[ , -brfexcl])

habi$biog   <- rowSums(habi[ , colnames(habi) %in% brfc])
habi$macroalgae <- rowSums(habi[ , grep("Macroalgae", colnames(habi))])         # sum all tags with macroalgae in col name
habi$kelps      <- habi$Macroalgae_Large.canopy.forming
habi$sponge     <- rowSums(habi[ , grep("Sponge", colnames(habi))])
habi$sand       <- rowSums(habi[ , grep("Unconsolidated", colnames(habi))])
habi$rock       <- rowSums(habi[ , grep("Consolidated", colnames(habi))])

# visualise patterns
covs <- c("Depth", "slope", "roughness", "tpi", "tri", "detrended")             # all covariates
habs <- c("kelps", "macroalgae", "sponge", "sand", "rock", "biogenic")          # all habitats
habl <- habi[, colnames(habi) %in% c(covs, habs, "totalpts")]
habl <- melt(habl, measure.vars = covs)
habl <- melt(habl, measure.vars = habs)
head(habl)
colnames(habl) <- c("totalpts", "covariate", "value", "habitat", "count")
ggplot(habl, aes(value, (count/totalpts) * 100)) + 
  geom_point() + geom_smooth() + 
  facet_grid(habitat ~ covariate, scales = "free")

# use formula from top model from '2_modelselect.R'
m_kelps <- gam(cbind(kelps, totalpts - kelps) ~ 
                 s(Depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(tpi, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_kelps)
gam.check(m_kelps)
vis.gam(m_kelps)

m_macro <- gam(cbind(macroalgae, totalpts - macroalgae) ~ 
                 s(Depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

m_sponge <- gam(cbind(sponge, totalpts - sponge) ~ 
            s(Depth,     k = 5, bs = "cr") + 
            s(detrended, k = 5, bs = "cr") + 
            s(tpi,       k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_sponge)
gam.check(m_sponge)
vis.gam(m_sponge)

m_sand <- gam(cbind(sand, totalpts - sand) ~ 
                s(Depth,     k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") + 
                s(tpi,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)


m_rock <- gam(cbind(sand, totalpts - sand) ~ 
                s(aspect,  k = 5, bs = "cr") + 
                s(Depth, k = 5, bs = "cr") + 
                s(tpi,    k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
vis.gam(m_rock)

m_biogenic <- gam(cbind(biog, totalpts - biog) ~
                    s(Depth,  k = 5, bs = "cr") + 
                    s(roughness, k = 5, bs = "cr") + 
                    s(tpi,    k = 5, bs = "cr"), 
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_biogenic)
gam.check(m_biogenic)
vis.gam(m_biogenic)

# predict and plot
bpreds <- cbind(preddf, 
                "pkelps" = predict(m_kelps, preddf, type = "response"),
                "pmacroalg" = predict(m_macro, preddf, type = "response"),
               "psponge" = predict(m_sponge, preddf, type = "response"),
               "psand" = predict(m_sand, preddf, type = "response"),
               "prock" = predict(m_rock, preddf, type = "response"),
               "pbiogenic" = predict(m_biogenic, preddf, type = "response"))

ggplot(bpreds, aes(x, y, fill = pkelps)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Kelps (p)") + 
  coord_equal()

ggsave("plots/broad_kelp.png", width = 10, height = 8, dpi = 160)

ggplot(bpreds, aes(x, y, fill = pmacroalg)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Macroalgae (p)") + 
  coord_equal()

ggsave("plots/broad_macroalgae.png", width = 10, height = 8, dpi = 160)

ggplot(bpreds, aes(x, y, fill = psponge)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Sponge (p)") + 
  coord_equal()

ggsave("plots/broad_sponge.png", width = 10, height = 8, dpi = 160)

ggplot(bpreds, aes(x, y, fill = psand)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Sand (p)") + 
  coord_equal()

ggsave("plots/broad_sand.png", width = 10, height = 8, dpi = 160)


ggplot(bpreds, aes(x, y, fill = prock)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Rock (p)") + 
  coord_equal()

ggsave("plots/broad_rock.png", width = 10, height = 8, dpi = 160)

ggplot(bpreds, aes(x, y, fill = pbiogenic)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Biogenic Reef (p)") + 
  coord_equal()

ggsave("plots/broad_biogenicreef.png", width = 10, height = 8, dpi = 160)

# or, predict only to survey areas

spreds <- cbind(spredf, 
                "pkelps" = predict(m_kelps, spredf, type = "response"),
                "pmacroalg" = predict(m_macro, spredf, type = "response"),
                "psponge" = predict(m_sponge, spredf, type = "response"),
                "psand" = predict(m_sand, spredf, type = "response"),
                "prock" = predict(m_rock, spredf, type = "response"),
                "pbiogenic" = predict(m_biogenic, spredf, type = "response"))

ggplot(spreds, aes(x, y)) +
  geom_tile(aes(fill = ps)) +
  scale_fill_viridis(option = "E") +
  geom_point(data = habi, aes(Longitude.1, Latitude.1, colour = (sponge/totalpts))) +
  scale_colour_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Sponge (p)", colour = NULL) +
  coord_equal()

ggplot(spreds, aes(x, y)) +
  geom_tile(aes(fill = pma)) +
  scale_fill_viridis(option = "E") +
  geom_point(data = habi, aes(Longitude.1, Latitude.1, colour = (macroalgae/totalpts))) +
  scale_colour_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Macroalgae (p)", colour = NULL) +
  coord_equal()

# categorise by dominant tag
bpreds$dom_tag <- apply(bpreds[12:16], 1,
                        FUN = function(x){names(which.max(x))})
bpreds$dom_tag <- sub('.', '', bpreds$dom_tag)
head(bpreds)

ggplot(bpreds, aes(x, y, fill = dom_tag)) +
  geom_raster() + 
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Dominant Habitat Type") + 
  coord_equal()

ggsave("plots/broad_dominant_habitat.png", width = 10, height = 8, dpi = 160)


# rasterise predictions and stack (mainly for predicting for fish modelling)

