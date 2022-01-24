###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting fish importance GAM relationships
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Set the study name
name <- "2021-05_Abrolhos_npz6" # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Bring in and format the  data----
dat.maxn <- readRDS("data/Tidy/dat.maxn.rds")%>%
  dplyr::filter(location%in%"NPZ6")%>%
  dplyr::rename(number = maxn)%>%
  glimpse()

dat.length <- readRDS("data/Tidy/dat.length.rds")%>%
  dplyr::filter(location%in%"NPZ6")%>%
  glimpse()

dat <- bind_rows(dat.maxn,dat.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos MaxN ####
unique(dat$scientific)

# MODEL Total abundance (detrended bathy) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod=gam(number~s(detrended,k=3,bs='cr'), family=tw,data=dat.total)

# predict - detrended bathy ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# detrended bathy ----
ggmod.total.bathymetry<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.total,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=number),alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.bathymetry

# MODEL Species richness (relief) ----
dat.species <- dat %>% filter(scientific=="species.richness")

mod=gam(number~s(relief,k=3,bs='cr'), family=tw,data=dat.species)

# predict - relief ----
testdata <- expand.grid(relief=seq(min(dat$relief),max(dat$relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.relief = testdata%>%data.frame(fits)%>%
  group_by(relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for species richness ----
# relief ----
ggmod.species.relief<- ggplot() +
  ylab("")+
  xlab("Relief")+
  geom_point(data=dat.species,aes(x=relief,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=number),alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.relief,aes(x=relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.relief

# MODEL Greater than legal size (detrended + status) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

mod=gam(number~s(detrended,k=3,bs='cr') + status, family=tw,data=dat.legal)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        status=c("Fished","No-take")) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - status ----
testdata <- expand.grid(detrended=mean(mod$model$detrended),
                        status=c("Fished","No-take")) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# detrended ----
ggmod.legal.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.legal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.detrended

# Combine with cowplot
library(cowplot)

# view plots
plot.grid.abundance <- plot_grid(ggmod.total, NULL,NULL,
                       ggmod.species.relief, ggmod.species.tpi,NULL,
                       ggmod.target.biog, ggmod.target.detrended, ggmod.target.macroalgae,
                       ncol = 3, labels = c('a','','','b','c','','d','e','f'),align = "vh")
plot.grid.abundance

plot.grid.lengths <- plot_grid( ggmod.greater.biog, ggmod.greater.detrended, ggmod.greater.macroalgae,
                                 ggmod.legal.miniatus.biog, ggmod.legal.miniatus.depth,NULL,
                                NULL,NULL,NULL,
                                 ncol = 3, labels = c('g','h','i','j','k','','','',''),align = "vh")
plot.grid.lengths

plot.grid.species <- plot_grid(ggmod.coris.depth,NULL, NULL,
                                 ggmod.miniatus.biog, ggmod.miniatus.depth,NULL,
                                 ggmod.chromis.relief,NULL,NULL,
                                 ncol = 3, labels = c('l','','','m','n','','o','',''),align = "vh")
plot.grid.species

#Save plots
save_plot("plots/abrolhos.boss.gam.abundance.png", plot.grid.abundance,base_height = 9,base_width = 8.5)
save_plot("plots/abrolhos.boss.gam.species.png", plot.grid.species,base_height = 9,base_width = 8.5)
save_plot("plots/abrolhos.boss.gam.lengths.png", plot.grid.lengths,base_height = 9,base_width = 8.5)