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

models <- readRDS("data/Tidy/all.mod.fits.RDS") %>%
  dplyr::mutate(taxa = c("total.abundance", "total.abundance", "total.abundance", "species.richness"))

models$formula <- gsub("\"","\'", models$formula)

resp.vars <- c("total.abundance","species.richness")

for (taxa in 1:length(unique(dat$scientific))) {
  use.dat <- as.data.frame(dat[which(dat$scientific == resp.vars[taxa]), ])
  mod = gam(eval(parse(text=(paste0("number~", models$formula[taxa])))), family = "tw",data = use.dat)

}


# MODEL Total abundance (relief) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod=gam(number~s(mean.relief,k=3,bs='cr'), family=tw,data=dat.total)
# predict - relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# detrended bathy ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Relief")+
  geom_point(data=dat.total,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.relief