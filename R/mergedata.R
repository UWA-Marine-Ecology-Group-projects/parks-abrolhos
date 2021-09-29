###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# author:  Kingsley Griffin
# date:    July 2021
##

library(reshape2)
library(dplyr)
library(ggplot2)


# read in data
bosmet <- readRDS("data/2105_abrolhos_boss.rds")                                # metadata
habdat <- read.table('data/2021-05_Abrolhos_BOSS_Habitat_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat annotations
reldat <- read.table('data/2021-05_Abrolhos_BOSS_Habitat_Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # relief annotations
subrel <- read.table('data/2021-05_Abrolhos_BOSS_Habitat_Substrate-Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # relief annotations

# clean all and merge to combine
head(reldat)
reldat <- reldat[ , c(1, 22)]
colnames(reldat) <- c("Site", "relief")
reldat$relief <- substr(reldat$relief, start = 1, stop = 3)
reldat$relief <- as.numeric(gsub("\\.", "", reldat$relief))
reldat$Site   <- gsub(".jpg", "", reldat$Site)
reldat <- as.data.frame(summarise(group_by(reldat, Site), 
                                  relief = mean(relief, na.rm = TRUE)))
head(reldat)

head(subrel)
subrel <- subrel[ , c(1, 22)]
colnames(subrel)  <- c("Site", "sub_relief")
subrel$sub_relief <- substr(subrel$sub_relief, start = 1, stop = 3)
subrel$sub_relief <- as.numeric(gsub("\\.", "", subrel$sub_relief))
subrel$Site       <- gsub(".jpg", "", subrel$Site)
subrel <- as.data.frame(summarise(group_by(subrel, Site), 
                                  sub_relief = mean(sub_relief, na.rm = TRUE)))
head(subrel)

summary(habdat)
habdat <- habdat[ , c(1, 4, 5, 18:21, 23, 26)]                                  # omit bare columns
colnames(habdat) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")            # fix colnames
habdat$Site      <- gsub(".jpg", "", habdat$Filename)
bosmet <- bosmet[, colnames(bosmet) %in% c("Date", "Time", "Latitude", 
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]                             # only cols of interest
allhab <- merge(bosmet, habdat, by = "Site")
allhab <- merge(allhab, reldat, by = "Site")
allhab <- merge(allhab, subrel, by = "Site")
head(allhab)
allhab$pa <- c(1)
# long to wide and summarise
allhabw <- reshape2::dcast(allhab, Site + Latitude + Longitude + Depth + 
                             relief + sub_relief ~ Broad + Morphology, 
                           value.var = "pa", fun.aggregate = sum, drop = TRUE)
allhabw$totalpts <- rowSums(allhabw[, 7:32]) - allhabw$Unknown_
head(allhabw)

# visualise
allhabl <- melt(allhabw, measure.vars = c(7:32))
colnames(allhabl)[9:10] <- c("Tag", "Count")

ggplot(allhabl, aes(Depth, Count/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap (~ Tag, scales = "free_y")

ggplot(allhabl, aes(relief, Count/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap (~ Tag, scales = "free_y")

ggplot(allhabl, aes(sub_relief, Count/totalpts)) + 
  geom_point() + geom_smooth() + 
  facet_wrap (~ Tag, scales = "free_y")


