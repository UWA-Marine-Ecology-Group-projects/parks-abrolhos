###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting Community Thermal Index (CTI)
# author:  Claude
# date:    Nov-Dec 2021
##

# Set directories----
rm(list=ls())

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(googlesheets4)

#standard error
se <- function(x) sd(x)/sqrt(length(x))

#load theme
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    #legend.title = element_blank(),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once

# read in raw maxn data 
boss <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::filter(maxn>0)%>%
  glimpse()

bruv <- read.csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::filter(maxn>0)%>%
  glimpse()

full.maxn <- bind_rows(boss,bruv)

length(unique(maxn$id))

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,rls.thermal.niche)%>%
  distinct()%>%
  glimpse()

dat <- maxn %>%
  left_join(master)%>%
  dplyr::filter(!is.na(rls.thermal.niche))%>%
  dplyr::mutate(log.maxn=log1p(maxn),weightedSTI=log.maxn*rls.thermal.niche)%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(log.maxn=sum(log.maxn),w.STI = sum(weightedSTI),CTI=w.STI/log.maxn)%>%
  dplyr::ungroup()%>%
  glimpse()

