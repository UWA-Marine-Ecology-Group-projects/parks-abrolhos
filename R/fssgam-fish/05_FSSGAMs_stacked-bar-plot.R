# Set directories----
rm(list=ls())

# Study name ----
study <- "2021-05_Abrolhos_BOSS" 

# Libraries required
library(GlobalArchive)

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(plyr)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own


theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major=element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

# Load fish pictures for plotting ----

a.g <- readPNG("data/images/Anampses_geographicus_nb_TAYLOR.png")
a.g <- as.raster(a.g)

c.ass <- readPNG("data/images/Chaetodon assarius-3cmL.png")
c.ass <- as.raster(c.ass)

c.r <- readPNG("data/images/Choerodon rubescens 3cm.png")
c.r <- as.raster(c.r)

c.w <- readPNG("data/images/Chromis westaustralis-3cmL.png")
c.w <- as.raster(c.w)

c.a <- readPNG("data/images/Coris auricularis-3cmL.png")
c.a <- as.raster(c.a)

s.c <- readPNG("data/images/Labridae-Dark.png")
s.c <- as.raster(s.c)

l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png")
l.m <- as.raster(l.m)

n.o <- readPNG("data/images/Neatypus obliquus-3cmL.png")
n.o <- as.raster(n.o)

p.s <- readPNG("data/images/Parupeneus_spilurus_nb_TAYLOR.png")
p.s <- as.raster(p.s)

p.spp <- readPNG("data/images/Pseudanthias rubrizonatus.png")
p.spp <- as.raster(p.spp)

# read in maxn
maxn <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  glimpse()

metadata <- maxn %>%
  distinct(sample, latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

# workout total maxn for each species ---
maxn.10<-maxn%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(12)%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  dplyr::mutate(scientific= plyr::revalue(scientific, c('Chaetodon assarius' = 'Chaetodon assarius*')))%>%
  glimpse()

## Total frequency of occurance
bar<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar


## Make labels for x axis
#allbarlabs<-c("Terapon jarbua","Carcharhinus sorrah","Saurida undosquamis","Netuma thalassina","Caranx ignobilis","Pristipomoides multidens","Decapterus spp","Lagocephalus lunaris","Nemipterus spp","Carangoides chrysophrys")

## Top ten plot ----
bar.top.10<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 630)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(labels=allbarlabs)+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  #   scale_y_log10()+
  # Apperance
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.w, xmin=9.65,xmax=10.25,ymin=610, ymax=650)+
  annotation_raster(c.a, xmin=8.65,xmax=9.35,ymin=220, ymax=290)+
  annotation_raster(c.ass, xmin=7.8, xmax=8.3, ymin=130, ymax=180)+
  annotation_raster(l.m, xmin=6.5,xmax=7.5,ymin=70, ymax=180)+
  annotation_raster(n.o, xmin=5.7,xmax=6.3,ymin=50, ymax=100)+
  annotation_raster(p.s, xmin=4.7,xmax=5.3,ymin=40, ymax=110)+
  annotation_raster(s.c, xmin=3.65,xmax=4.25,ymin=30, ymax=100)+
  annotation_raster(c.r, xmin=2.6,xmax=3.4,ymin=25, ymax=130)+
  annotation_raster(p.spp, xmin=1.6,xmax=2.4,ymin=23, ymax=100)+
  annotation_raster(a.g, xmin=0.7,xmax=1.3,ymin=20, ymax=90)
bar.top.10

ggsave("stacked.bar.plot.png",bar.top.10,dpi=600,width=5)
