###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Basic habitat models
# author:  Kingsley Griffin
# date:    July-Oct 2021
##

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)

# read in
habi   <- readRDS("data/tidy/merged_habitat.rds")                             # merged data from 'R/1_mergedata.R'
preds  <- readRDS("data/spatial/spatial_covariates.rds")                      # stack of spatial covs from 'R/1_mergedata.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$Depth <- preddf$Z * -1

# reduce predictor space to fit survey area
preddf <- preddf[preddf$Depth > min(habi$Depth), ]
preddf <- preddf[preddf$Depth < 200, ]

# broad macroalgae classification ----
habi$macroalgae <- rowSums(habi[, grep("Macroalgae", colnames(habi))])        # sum all macroalgae tags

# visualise
covs <- c("Depth", "slope", "roughness", "tpi", "tri")
habl <- melt(habi, measure.vars = covs)
ggplot(habl, aes(value, macroalgae/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~ variable, scales = "free_x")

# something up with one point - needs investigation
habi <- habi[!(habi$totalpts - habi$macroalgae < 0), ]

# quick model
m1 <- gam(cbind(macroalgae, totalpts - macroalgae) ~ 
            s(log(Depth), bs = "cr") + s(tri, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m1)

gam.check(m1)
vis.gam(m1)

# predict and plot
preds <- cbind(preddf, "ps" = predict(m1, preddf, type = "response"))

ggplot(preds, aes(x, y, fill = ps)) +
  geom_raster() + 
  scale_fill_viridis(option = "E") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Macroalgae (p)")

ggsave("figures/broad_macroalgae.png", width = 10, height = 8, dpi = 160)

# ### NOTES ----
# ## visualise all relationships
# colnames(habi)
# habl <- melt(habi, measure.vars = c(8:34))
# colnames(habl)[19:20] <- c("Tag", "Count")
# 
# ggplot(habl, aes(Depth, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
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



