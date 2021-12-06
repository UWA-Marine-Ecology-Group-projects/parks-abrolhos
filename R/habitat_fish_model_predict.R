###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
fabund <- readRDS("output/fish_abundance_fssgamdat.rds")                        # merged fish data used for fssgam script
preds  <- readRDS("output/broad_habitat_predictions.rds")                       # spatial and habitat covs
prel   <- readRDS("output/predicted_relief_raster.rds")                         # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
predsp$relief <- extract(prel, predsp)
preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE)
preddf$depth  <- preddf$Z * -1
preddf$rock   <- preddf$prock
preddf$biog   <- preddf$pbiogenic
preddf$macroalgae   <- preddf$pmacroalg
head(preddf)

# reduce predictor space to fit survey area
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
                                                fabund$latitude.1), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)
unique(fabund$scientific)

# use formula from top model from FSSGam model selection
m_totabund <- gam(maxn ~ s(relief, k = 3, bs = "cr"), 
               data = fabund[fabund$scientific == "total.abundance", ], 
               method = "REML", family = tw())
summary(m_totabund)
# gam.check(m_totabund)
# vis.gam(m_totabund)

m_targetabund <- gam(maxn ~ s(biog, k = 3, bs = "cr")  + 
                       s(detrended, k = 3, bs = "cr") + 
                       s(macroalgae, k = 3, bs = "cr"),  # not necessarily the top model
                     data = fabund[fabund$scientific == "targeted.abundance", ], 
                     method = "REML", family = tw())
summary(m_targetabund)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)

m_richness <- gam(maxn ~ s(relief, k = 5, bs = "cr")  + 
                    s(tpi, k = 5, bs = "cr"), 
                  data = fabund[fabund$scientific == "species.richness", ], 
                  method = "REML", family = tw())
summary(m_richness)
# gam.check(m_richness)
# vis.gam(m_richness)

m_cauricularis <- gam(maxn ~ s(depth, k = 5, bs = "cr") +
                        s(biog, k = 5, bs = "cr"), 
                      data = fabund[fabund$scientific == "Labridae Coris auricularis", ], 
                      method = "REML", family = tw())
summary(m_cauricularis)

m_cwestaustralis <- gam(maxn ~ s(relief, k = 5, bs = "cr"), 
                      data = fabund[fabund$scientific == "Pomacentridae Chromis westaustralis", ], 
                      method = "REML", family = tw())
summary(m_cwestaustralis)

m_lminatus <- gam(maxn ~ s(depth, k = 5, bs = "cr") +
                    s(biog, k = 5, bs = "cr"), 
                      data = fabund[fabund$scientific == "Lethrinidae Lethrinus miniatus", ], 
                      method = "REML", family = tw())
summary(m_lminatus)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_argetabund" = predict(m_targetabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_cauricularis" = predict(m_cauricularis, preddf, type = "response"),
                "p_cwestaustralis" = predict(m_cwestaustralis, preddf, type = "response"),
                "p_lminatus" = predict(m_lminatus, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 27:32)], res = c(247, 277))
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/site_fish_predictions.rds")


