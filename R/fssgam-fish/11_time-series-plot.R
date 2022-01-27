###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting species richness and greater than legal targeted species
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
library(stringr)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

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


# read in maxn
maxn <- readRDS("data/Tidy/dat.maxn.rds")%>%
  glimpse()

length <- readRDS("data/Tidy/dat.length.rds")%>%
  glimpse()

#need to make a new dataframe - year, species richness (plus SE), greater than legal (plus SE)
year <- c("2019","2019","2020","2020","2021","2021","2022","2022")
status <- c("Fished","No-take")
dat <- data.frame(year,status)

#data for npz6
spr.npz6.sr <- maxn %>%
  dplyr::filter(location%in%"NPZ6",scientific%in%"species.richness")%>%
  dplyr::group_by(status)%>%
  summarise(species.richness = mean(maxn),species.richness.se=se(maxn))%>%
  dplyr::mutate(year="2021")

spr.npz6.l <- length %>%
  dplyr::filter(location%in%"NPZ6",scientific%in%"greater than legal size")%>%
  dplyr::group_by(status)%>%
  summarise(legal = mean(number),legal.se=se(number))%>%
  dplyr::mutate(year="2021")

npz6 <- dat %>%
  left_join(spr.npz6.sr)%>%
  left_join(spr.npz6.l)

#data for npz9
#no data for status
spr.npz9.sr <- maxn %>%
  dplyr::filter(location%in%"NPZ9",scientific%in%"species.richness")%>%
  dplyr::group_by(status)%>%
  summarise(species.richness = mean(maxn),species.richness.se=se(maxn))%>%
  dplyr::mutate(year="2021")

spr.npz9.l <- length %>%
  dplyr::filter(location%in%"NPZ9",scientific%in%"greater than legal size")%>%
  dplyr::group_by(status)%>%
  summarise(legal = mean(number),legal.se=se(number))%>%
  dplyr::mutate(year="2021")

npz9 <- dat %>%
  left_join(spr.npz9.sr)%>%
  left_join(spr.npz9.l)

#NPZ6
# plot year by species richness - plus a line for MPA gazetting time ---
gg.npz6.sr <- ggplot(data = npz6, aes(x = year, y = species.richness, fill = status))+
  geom_errorbar(data = npz6,aes(ymin=species.richness-species.richness.se,ymax= species.richness+species.richness.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.3),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,8))+
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Species richness")+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.npz6.sr

#greater than legal
gg.npz6.l <- ggplot(data = npz6, aes(x = year, y = legal, fill = status))+
  geom_errorbar(data = npz6,aes(ymin=legal-legal.se,ymax= legal+legal.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.3),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,8))+
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.npz6.l

#NPZ9
# plot year by species richness - plus a line for MPA gazetting time ---
gg.npz9.sr <- ggplot(data = npz9, aes(x = year, y = species.richness, fill = status))+
  geom_errorbar(data = npz9,aes(ymin=species.richness-species.richness.se,ymax= species.richness+species.richness.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.3),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,8))+
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Species richness")+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.npz9.sr

#greater than legal
gg.npz9.l <- ggplot(data = npz9, aes(x = year, y = legal, fill = status))+
  geom_errorbar(data = npz9,aes(ymin=legal-legal.se,ymax= legal+legal.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.3),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,8))+
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.npz9.l

# library(ggpubr)
library(patchwork)
grid.npz6 <- gg.npz6.sr/gg.npz6.l+plot_layout(guides = 'collect')
grid.npz6

grid.npz9 <- gg.npz9.sr/gg.npz9.l+plot_layout(guides = 'collect')
grid.npz9

#save out plot
ggsave("plots/time-series.npz6.png",grid.npz6,dpi=600,width=6.0)
ggsave("plots/time-series.npz9.png",grid.npz9,dpi=600,width=6.0)
