###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Kingsley Griffin
# date:    Sept-Nov 2021
##

rm(list = ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(tidyverse)

# read in
habi   <- readRDS("data/tidy/merged_habitat.rds") %>%                              # merged data from 'R/1_mergedata.R'
  dplyr::rename(totalpts = broad.total.points.annotated)
preds  <- readRDS("data/spatial/spatial_covariates.rds")                        # spatial covs from 'R/1_mergedata.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth <- preddf$Z * -1

# reduce predictor space to fit survey area
# preddf <- preddf[preddf$Depth > min(habi$Depth), ]
# preddf <- preddf[preddf$Depth < 200, ]
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude.1, 
                                                habi$latitude.1), data = habi)
sbuff  <- buffer(habisp, 10000)

# # visualise patterns
# covs <- c("Depth", "slope", "roughness", "tpi", "tri", "detrended")             # all covariates
# habs <- c("kelps", "macroalgae", "sponge", "sand", "rock", "biogenic")          # all habitats
# habl <- habi[, colnames(habi) %in% c(covs, habs, "totalpts")]
# habl <- melt(habl, measure.vars = covs)
# habl <- melt(habl, measure.vars = habs)
# head(habl)
# colnames(habl) <- c("totalpts", "covariate", "value", "habitat", "count")
# ggplot(habl, aes(value, (count/totalpts) * 100)) + 
#   geom_point() + geom_smooth() + 
#   facet_grid(habitat ~ covariate, scales = "free")

# use formula from top model from '2_modelselect.R'
m_kelps <- gam(cbind(kelps, totalpts - kelps) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") + 
                 s(tpi, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
# summary(m_kelps)
# gam.check(m_kelps)
# vis.gam(m_kelps)

m_macro <- gam(cbind(macroalgae, totalpts - macroalgae) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
# summary(m_macro)
# gam.check(m_macro)
# vis.gam(m_macro)

m_biogenic <- gam(cbind(biog, totalpts - biog) ~ 
            s(depth,     k = 5, bs = "cr") + 
            s(detrended, k = 5, bs = "cr") + 
            s(roughness,       k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
# summary(m_biogenic)
# gam.check(m_biogenic)
# vis.gam(m_biogenic)

m_sand <- gam(cbind(sand, totalpts - sand) ~ 
                s(depth,     k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") + 
                s(tpi,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
# summary(m_sand)
# gam.check(m_sand)
# vis.gam(m_sand)

m_rock <- gam(cbind(rock, totalpts - rock) ~ 
                s(depth, k = 5, bs = "cr") + 
                s(detrended,  k = 5, bs = "cr") + 
                s(tpi,    k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
# summary(m_rock)
# gam.check(m_rock)
# vis.gam(m_rock)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pkelps" = predict(m_kelps, preddf, type = "response"),
                "pmacroalg" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "pbiogenic" = predict(m_biogenic, preddf, type = "response", se.fit = T))

# prasts$dom_tag <- which.max(prasts[[13:17]])
prasts <- rasterFromXYZ(preddf, res = c(247, 277))
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

crs(sprast) <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"
plot(sprast[[11:15]]) 

saveRDS(sprast, file = "output/fssgam/abrolhos-habitat-spatial_UTM50.rds")

# tidy and output data
spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = T)

# categorise by dominant tag
spreddf$dom_tag <- apply(spreddf[13:17], 1,
                        FUN = function(x){names(which.max(x))})
spreddf$dom_tag <- sub('.', '', spreddf$dom_tag)
head(spreddf)

test <- spreddf[,c(1:2,18)]
test$dom_tag <- as.factor(test$dom_tag)
test$ndom_tag <- as.numeric(test$dom_tag)
head(test)
testraster <- rasterFromXYZ(test[,c("x", "y", "ndom_tag")], 
                            crs = "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")
plot(testraster)
testraster[] = factor(levels(test$dom_tag)[testraster[]])
plot(testraster)

saveRDS(testraster, file = "output/fssgam/abrolhos-dominant-habitat_UTM50.rds")

saveRDS(spreddf, "output/site_habitat_predictions.rds")
