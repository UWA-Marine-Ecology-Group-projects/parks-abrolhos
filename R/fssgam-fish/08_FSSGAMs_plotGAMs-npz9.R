###
# Project: Parks - Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Plotting fish GAM relationships at npz9
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
name <- "2021-05_Abrolhos_npz9" # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Bring in and format the  data----
dat.maxn <- readRDS("data/Tidy/dat.maxn.rds")%>%
  dplyr::filter(location%in%"NPZ9")%>%
  dplyr::rename(number = maxn)%>%
  glimpse()

dat.length <- readRDS("data/Tidy/dat.length.rds")%>%
  dplyr::filter(location%in%"NPZ9")%>%
  glimpse()

dat <- bind_rows(dat.maxn,dat.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos MaxN ####
unique(dat$scientific)

# MODEL Total abundance (relief + slope) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod=gam(number~s(relief,k=3,bs='cr')+s(slope,k=3,bs='cr'), family=tw,data=dat.total)

# predict - relief ----
testdata <- expand.grid(relief=seq(min(dat$relief),max(dat$relief),length.out = 20),
                        slope=mean(mod$model$slope)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.relief = testdata%>%data.frame(fits)%>%
  group_by(relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - slope ----
testdata <- expand.grid(slope=seq(min(dat$slope),max(dat$slope),length.out = 20),
                        relief=mean(mod$model$relief)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.slope = testdata%>%data.frame(fits)%>%
  group_by(slope)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# relief ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Relief")+
  geom_point(data=dat.total,aes(x=relief,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.relief,aes(x=relief,y=number),alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.relief

# slope ----
ggmod.total.slope<- ggplot() +
  ylab("")+
  xlab("Slope")+
  geom_point(data=dat.total,aes(x=slope,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.slope,aes(x=slope,y=number),alpha=0.5)+
  geom_line(data=predicts.total.slope,aes(x=slope,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.slope,aes(x=slope,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.slope

# MODEL Greater than legal size (slope) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

mod=gam(number~s(slope,k=3,bs='cr'), family=tw,data=dat.legal)

# predict - slope ----
testdata <- expand.grid(slope=seq(min(dat$slope),max(dat$slope),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.slope = testdata%>%data.frame(fits)%>%
  group_by(slope)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Greater than legal size ----
# slope ----
ggmod.legal.slope<- ggplot() +
  ylab("")+
  xlab("Slope")+
  geom_point(data=dat.legal,aes(x=slope,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.legal.slope,aes(x=slope,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.slope,aes(x=slope,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.slope,aes(x=slope,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.slope

# MODEL Smaller than legal size (depth) ----
dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(depth,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Smaller than legal size ----
# slope ----
ggmod.sublegal.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sublegal,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.depth

# Combine with cowplot
library(cowplot)

# view plots
plot.grid.npz9 <- plot_grid(ggmod.total.relief, ggmod.total.slope,
                            ggmod.legal.slope, NULL,
                            ggmod.sublegal.depth, NULL,
                            NULL,NULL,
                            ncol = 2, labels = c('a','b','c','','d',''),align = "vh")
plot.grid.npz9

#Save plots
save_plot("plots/abrolhos.npz9.gam.png", plot.grid.npz9,base_height = 9,base_width = 8.5)
