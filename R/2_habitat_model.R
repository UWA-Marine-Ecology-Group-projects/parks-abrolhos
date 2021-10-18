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

# read in
habi   <- readRDS("data/tidy/merged_habitat.rds")                               # merged data from 'R/1_mergedata.R'
preds  <- readRDS("data/spatial/spatial_covariates.rds")                        # spatial covs from 'R/1_mergedata.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$Depth <- preddf$Z * -1

# reduce predictor space to fit survey area
preddf <- preddf[preddf$Depth > min(habi$Depth), ]
preddf <- preddf[preddf$Depth < 200, ]
spredf <- preddf[(preddf$y > 6880000 & preddf$y < 6900000 & preddf$x > 130000 & preddf$x < 165000) | 
                   (preddf$y > 6986000 & preddf$y < 7000000 & preddf$x > 110000 & preddf$x < 125000), ]

# broad macroalgae, sponge classification ----
habi$macroalgae <- rowSums(habi[ , grep("Macroalgae", colnames(habi))])         # sum all macroalgae tags
habi$sponge     <- rowSums(habi[ , grep("Sponge", colnames(habi))])

# something up with one point - needs investigation
habi <- habi[!(habi$totalpts - habi$macroalgae < 0), ]

# visualise
covs <- c("Depth", "slope", "roughness", "tpi", "tri")
habl <- melt(habi, measure.vars = covs)
ggplot(habl, aes(value, macroalgae/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~ variable, scales = "free_x")

ggplot(habl, aes(value, sponge/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~ variable, scales = "free_x")

# top model from '2_modelselect.R'
m1 <- gam(cbind(macroalgae, totalpts - macroalgae) ~ 
            s(Depth, k = 5, bs = "cr") + s(roughness, k = 5, bs = "cr") + s(tpi, k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m1)

gam.check(m1)
vis.gam(m1)

m2 <- gam(cbind(sponge, totalpts - sponge) ~ 
            s(Depth, k = 5, bs = "cr") + s(roughness, k = 5, bs = "cr") + s(tpi, k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m2)
gam.check(m2)
vis.gam(m2)

# predict and plot
bpreds <- cbind(preddf, "pma" = predict(m1, preddf, type = "response"),
               "ps" = predict(m2, preddf, type = "response"))

ggplot(bpreds, aes(x, y, fill = pma)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Macroalgae (p)") + 
  coord_equal()

ggsave("figures/broad_macroalgae.png", width = 10, height = 8, dpi = 160)

ggplot(bpreds, aes(x, y, fill = ps)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Sponge (p)") + 
  coord_equal()

# predict only to survey areas

spreds <- cbind(spredf, "pma" = predict(m1, spredf, type = "response"),
                "ps" = predict(m2, spredf, type = "response"))

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



# ### NOTES ----
# ## visualise all relationships
# colnames(habi)
# habl <- melt(habi, measure.vars = c(8:34))
# colnames(habl)[19:20] <- c("Tag", "Count")
# 
# ggplot(habl, aes(Depth, Count/totalpts)) +
#   geom_point() + geom_smooth() +
#   facet_wrap (~ Tag, scales = "free_y")

# ggplot(habl, aes(Z, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
# ggplot(habl, aes(relief, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
# ggplot(habl, aes(roughness, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
# ggplot(habl, aes(slope, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
# ggplot(habl, aes(tri, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 



