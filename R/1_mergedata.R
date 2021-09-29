###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# author:  Kingsley Griffin, Brooke Gibbons
# date:    July 2021
##

library(reshape2)
library(dplyr)
library(ggplot2)


# read in data
bosmet <- readRDS("data/2105_abrolhos_boss.rds")                                # metadata
habdat <- read.table('data/raw/TM Export/2021-05_Abrolhos_BOSS_Habitat_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat annotations
reldat <- read.table('data/raw/TM Export/2021-05_Abrolhos_BOSS_Habitat_Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat annotations

# clean all and merge to combine
head(reldat)
reldat <- reldat[ , c(1, 22)]
colnames(reldat) <- c("Sample", "relief") # Brooke changed to Sample
reldat$relief <- substr(reldat$relief, start = 1, stop =3)
reldat$relief <- as.numeric(gsub("\\.", "", reldat$relief))
reldat$Sample   <- gsub(".jpg", "", reldat$Sample) # Changed to Sample
reldat <- as.data.frame(summarise(group_by(reldat, Sample), 
                                  relief = mean(relief, na.rm = TRUE)))
head(reldat)

summary(habdat)
habdat <- habdat[ , c(1, 4, 5, 18:21, 23, 26)]                                  # omit bare columns
colnames(habdat) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")            # fix colnames
habdat$Sample      <- gsub(".jpg", "", habdat$Filename)
bosmet <- bosmet[, colnames(bosmet) %in% c("Sample","Date", "Time", "Latitude", 
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]                             # only cols of interest
allhab <- merge(bosmet, habdat, by = "Sample")
allhab <- merge(allhab, reldat, by = "Sample")
head(allhab)
allhab$pa <- c(1)
# long to wide and summarise
allhabw <- reshape2::dcast(allhab, Sample + Site + Latitude + Longitude + Depth + relief ~ Broad + Morphology, 
                           value.var = "pa", fun.aggregate = sum, drop = TRUE)
allhabw$totalpts <- rowSums(allhabw[, 8:34]) - allhabw$Unknown_
head(allhabw)

saveRDS(allhabw, "data/tidy/merged_habitat.rds")

# # visualise relationships
# allhabl <- melt(allhabw, measure.vars = c(8:34))
# colnames(allhabl)[10:11] <- c("Tag", "Count")
# 
# ggplot(allhabl, aes(Depth, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")
# 
# ggplot(allhabl, aes(relief, Count/totalpts)) + 
#   geom_point() + geom_smooth() + 
#   facet_wrap (~ Tag, scales = "free_y")

# data checks (Brooke)
# check for habitat data that is missing metadata
t1 <- dplyr::anti_join(allhabw, bosmet) # none

# check for samples in metadata missing habitat
t2 <- dplyr::anti_join(bosmet, allhabw) # none

unique(allhabw$Site)