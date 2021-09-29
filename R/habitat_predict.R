###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Basic habitat models
# author:  Kingsley Griffin
# date:    July 2021
##

library(reshape2)
library(dplyr)
library(ggplot2)
library(raster)


# visualise relationships
allhabl <- melt(allhabw, measure.vars = c(6:32))
colnames(allhabl)[7:8] <- c("Tag", "Count")

ggplot(allhabl, aes(Depth, Count/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap (~ Tag, scales = "free_y")

ggplot(allhabl, aes(relief, Count/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap (~ Tag, scales = "free_y")

