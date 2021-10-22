###
# Project: Parks - Abrolhos Post-Survey
# Data:    Relief data (scored during fish counts)
# Task:    Kriging relief
# author:  Kingsley Griffin
# date:    Oct 2021
##

library(INLA)
library(sp)

habi   <- readRDS("data/tidy/merged_habitat.rds")                               # merged data from 'R/1_mergedata.R'
preds  <- readRDS("data/spatial/spatial_covariates.rds")                        # spatial covs from 'R/1_mergedata.R'
colnames(habi)
# trim predictor data cols and subset to npz6 area
habi      <- habi[ , c(1, 2, 7, 39:49)]
habi$npz6 <- c(0)
habi$npz6[grep("npz6", habi$Sample)] <- 1
habi$npz6[grep("out6", habi$Sample)] <- 1
# habi      <- habi[habi$npz6 == 1, ]
head(habi)

# build inla mesh from spatial layout of sites - the constants need some tuning
habisp         <- SpatialPointsDataFrame(coords = habi[4:5], data = habi)
sitecoords     <- coordinates(habisp)
sitelocs       <- as.matrix(sitecoords)
max.edgelength <- c(500, 2500)
mesha          <- inla.mesh.2d(loc = sitelocs, max.edge = max.edgelength,
                               offset = c(500, 2000), cutoff = 300)
plot(mesha)
plot(habisp, add = T, col = "red")

# prep for modelling
meshadata      <- inla.spde.make.A(mesha, sitelocs)
spde           <- inla.spde2.matern(mesha, alpha = 2)
datn           <- nrow(habisp)
preddf         <- habi[, colnames(habi) %in% c("Z", "roughness", "tpi", "detrended")]

relief_stack   <- inla.stack(data = list(y = habi$relief),
                             A = list(meshadata, 1),
                             effects = list(c(sp = list(1:mesha$n)),
                                            list(depth = preddf$Z,
                                                 rough = preddf$roughness,
                                                 # tpi   = preddf$tpi,
                                                 dtren = preddf$detrended)),
                             remove.unused = TRUE)

modform        <- y ~ depth + rough + dtren + f(sp, model = spde)

# fit model
m1 <- inla(modform, data = inla.stack.data(relief_stack),
           control.predictor = list(A = inla.stack.A(relief_stack)))

summary(m1)

# # evaluate fit and tuning
# rf <- inla.spde.result(inla = m1, name = "sp", spde = spde, do.transf = TRUE)
# 
# par(mfrow=c(3,3))
# plot(rf$marginals.variance.nominal[[1]], type="l",xlab=expression(sigma[x]^2), ylab="Density", main = "SPDE local var")
# plot(rf$marginals.kap[[1]],type="l", xlab=expression(kappa), ylab="Density")
# plot(m1$marginals.hy[[1]], type="l", ylab="Density",xlab=expression(phi))
# # plot(m1$marginals.fix$Intercept, type="l", xlab="Intercept",ylab="Density")
# # plot(mod$marginals.fixed$nmoorid, type="l", ylab="Density",xlab= "nmoorid")
# # abline(v = m1$summary.fixed$mean, col = 'red')
# plot(m1$summary.random$s[,1:2], type="l",xlab="Spatial", ylab="random effect")
# plot.default(rf$marginals.range.nominal[[1]], type="l",xlab="Practical range", ylab="Density")
# abline(v = max.edgelength[1], col = 'red')
# abline(v = max.edgelength[2], col = 'red')
# 
# #as well as the posterior precision of the random effect.
# plot(m1$marginals.hy[[2]], type="l", ylab="Density",xlab=names(m1$marginals.hy)[2])
# 
# dev.off()

# predict spatial random effect back onto mesh from spde model fit
ypred <- m1$summary.random$s$mean

# xlim  <- c(extent(preds)[1], extent(preds)[2])
xlim  <- c(-102500 , 258000)

# ylim  <- c(extent(preds)[3], extent(preds)[4])
ylim  <- c(6775500, 7123250)
xdims <- (xlim[2] - xlim[1]) / 250
ydims <- (ylim[2] - ylim[1]) / 250

proj       <- inla.mesh.projector(mesha, xlim = xlim, ylim = ylim, 
                                  dims = c(xdims, ydims))
field.proj <- inla.mesh.project(proj, ypred)

datpred <- data.frame(x = rep(proj$x, ydims), 
                      y = rep(proj$y, each = xdims), 
                      pred = as.numeric(field.proj))

datpred <- na.omit(datpred)

predrast <- rasterize(x = cbind(datpred$x, datpred$y), 
                      y = preds, field = datpred$pred)

sitebuf  <- buffer(habisp, 20000)
predrast <- mask(predrast, sitebuf)
predrast <- crop(predrast, extent(sitebuf))
plot(predrast)
plot(habisp, add = TRUE, col = "red")

# predict relief score across mesh using model formula



