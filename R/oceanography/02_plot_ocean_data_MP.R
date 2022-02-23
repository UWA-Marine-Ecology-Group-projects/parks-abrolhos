###
# Project: Parks - Abrolhos
# Data:    Oceanography - SST, SLA, currents & acidification
# Task:    Plot oceanography trends
# author:  Jess Kolbusz
# date:    Feb 2022
##

#remotes::install_github("hvillalo/satin2") #for quiver plots in R

library(sf)
library(reshape2)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(ggquiver)

#set working directory
working.dir <- getwd()
setwd(working.dir)

Zone = "Abrolhos"
# data_dir = paste('~/Desktop/MPA_work/data/Rdata',Zone, sep = '/')
# plot_dir = paste('~/Desktop/MPA_work/plots',Zone, sep = '/')

#lims of the spatial plots # change for each mp, bigger than you think because of arrrows #
xxlim = c(112.8, 115) #all NW c(114, 117)#ABR c(112.8, 115) #long
yylim = c(-29.5, -26) #all NW c(-21.5, -19) #ABR  #lat

#all this is on git already - i don't know how to code in different for git??
# only one I use for the plots so far is the "aus" one for the coast outline
#setting up mapping/coastal are for spatial, taken from kingsley script X_siteplots
aus    <- st_read("data/spatial/shp/61395_mif/australia/cstauscd_r.mif") #data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                        # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
#extra bits haven't used or loaded for these maps
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")         # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Montebello Islands", #"Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby
NW_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- NW_mpa[NW_mpa$ResName %in% c("Montebello", "Jurien", "Shark Bay"), ]    # just nat parks nearby
cwatr  <- readRDS('output/coastal_waters_limit_trimmed.rds')                    # coastal waters line trimmed in 'R/GA_coast_trim.R'
bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)
st_crs(dirkh)       <- st_crs(aumpa) 

## get data locations /limits that need from MPA
## do control F replace to replace names in the script 
##### SLA and CURR data load ####
load("data/spatial/oceanography/Abrolhos_sla_current_data.Rdata")

#get min and max for spatial plot colours from loaded data
min_sla =round(min(min(sla_monthly,na.rm = TRUE), na.rm = TRUE), digits = 2)
max_sla= round(max(max(sla_monthly,na.rm = TRUE), na.rm = TRUE), digits = 2) 

#function to melt data so ggplot can use it and plot 
melt_sla <- function(sla_monthly, chose_month) {
  L<- list()
  L$lats <- lat_sla
  L$lons <- lon_sla
  L$month <- chose_month
  L$sla <- sla_monthly[,,chose_month]
  dimnames(L$sla) <- list(long = L$lons, lat = L$lats)
  ret <- melt(L$sla, value.name = "sla")
  cbind(date = L$month, ret)
}

#plot each month
choose_month <- 1
chose_month_text <- "Jan"
title_legend <- "SLA"

msla <- melt_sla(sla_monthly,choose_month)

p_1 <- ggplot() +
  geom_tile( data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("January") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
# p_1

dev.off()
choose_month <- 2
chose_month_text <- "Feb"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_2 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("February") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_2

dev.off()
choose_month <- 3
chose_month_text <- "Mar"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_3 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("March") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_3

dev.off()
choose_month <- 4
chose_month_text <- "Apr"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_4 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("April") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_4

dev.off()
choose_month <- 5
chose_month_text <- "May"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_5 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("May") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_5

dev.off()
choose_month <- 6
chose_month_text <- "Jun"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_6 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("June") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_6

dev.off()
choose_month <- 7
chose_month_text <- "Jul"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_7 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("July") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_7

dev.off()
choose_month <- 8
chose_month_text <- "Aug"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_8 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("August") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_8

dev.off()
choose_month <- 9
chose_month_text <- "Sep"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_9 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("September") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_9

dev.off()
choose_month <- 10
chose_month_text <- "Oct"
# title_legend <- paste("SLA", chose_month_text)

msla <- melt_sla(sla_monthly,choose_month)

p_10 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("October") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_10

dev.off()
choose_month <- 11
chose_month_text <- "Nov"

msla <- melt_sla(sla_monthly,choose_month)

p_11 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("November") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_11

dev.off()
choose_month <- 12
chose_month_text <- "Dec"

msla <- melt_sla(sla_monthly,choose_month)

p_12 <- ggplot() +
  geom_tile(data = msla, aes(x = long, y = lat, fill = sla)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  ggtitle("December") +
  theme(plot.title = element_text(hjust = 0))+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
# p_12

# p_1+p_2+p_3+p_4+p_5+p_6+p_7+p_8+p_9+p_10+p_11+p_12 + 
#   plot_layout(ncol = 3, nrow = 4,guides = 'collect')

p_1+p_3+p_5+p_7+p_9+p_11+
  plot_layout(ncol = 3, nrow = 2,guides = 'collect')

ggsave('plots/spatial/Abrolhos_sla_monthly_spatial.png', dpi = 300, width = 9, height = 9)

##### SURFACE CURRENTS ######

#data should have loaded already above

#function to melt the data so its plottable with ggplot
melt_cur <- function(uu_monthly, vv_monthly, chose_month) {
  L<- list()
  L$lats <- lat_sla
  L$lons <- lon_sla
  L$month <- chose_month
  L$uu <- uu_monthly[,,chose_month]
  L$vv <- vv_monthly[,,chose_month]
  
  dimnames(L$uu) <- list(long = L$lons, lat = L$lats)
  dimnames(L$vv) <- list(longg = L$lons, latt = L$lats)
  
  ret <- melt(L$uu, value.name = "uu")
  ret1 <- melt(L$vv, value.name = "vv")
  
  cbind(date = L$month, ret, ret1)
}

##### plotting current ####
#gets only every split_size rows in current data so displayed better on map, 
#if is every current vector it's too messy

arrow_size <- 2.5
split_size <- 3 

dev.off()
choose_month <- 1
chose_month_text <- "Jan"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% 
  filter(!is.na(uu)) %>% glimpse()

p_1 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size ) +
  labs(x = "Longitude", y = "Latitude", title = "January") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
p_1

dev.off()
choose_month <- 2
chose_month_text <- "Feb"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_2 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "February") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_2

dev.off()
choose_month <- 3
chose_month_text <- "Mar"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_3 <-  
  ggplot() +  
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "March") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_3

dev.off()
choose_month <- 4
chose_month_text <- "Apr"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_4 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "April") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_4

dev.off()
choose_month <- 5
chose_month_text <- "May"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_5 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "May") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_5

dev.off()
choose_month <- 6
chose_month_text <- "Jun"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_6 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "June") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_6

dev.off()
choose_month <- 7
chose_month_text <- "Jul"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_7 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "July") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_7

dev.off()
choose_month <- 8
chose_month_text <- "Aug"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_8 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "August") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
# p_8

dev.off()
choose_month <- 9
chose_month_text <- "Sep"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_9 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "September") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_9

dev.off()
choose_month <- 10
chose_month_text <- "Oct"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_10 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "October") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_10

dev.off()
choose_month <- 11
chose_month_text <- "Nov"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_11 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "November") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_11

dev.off()
choose_month <- 12
chose_month_text <- "Dec"
# title_l <- paste("Current", chose_month_text)

curr <- melt_cur(uu_monthly,vv_monthly,choose_month)
cur_month <- subset(curr, select = -c(longg, latt))
curr_month <- cur_month %>% slice(which(row_number() %% split_size == 1)) %>% glimpse()

p_12 <-  
  ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), vecsize=arrow_size) +
  labs(x = "Longitude", y = "Latitude", title = "December") + 
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
#p_12

p_1+p_3+p_5+p_7+p_9+p_11+
  plot_layout(ncol = 3, nrow = 2)

ggsave('plots/spatial/Abrolhos_current_monthly_spatial.png', dpi = 300, width = 9, height = 9)

######### SST #########

load("data/spatial/oceanography/Abrolhos_sst_data.Rdata")

#get min and max for whole spatial scales legend/colours
min_sst =round(min(min(sst_monthly,na.rm = TRUE), na.rm = TRUE))
max_sst= round(max(max(sst_monthly,na.rm = TRUE), na.rm = TRUE)) 

#melts data to easily plot it
melt_sst <- function(sst_monthly, chose_month) {
  L<- list()
  L$lats <- lat_sst
  L$lons <- lon_sst
  L$month <- chose_month
  L$sst <- sst_monthly[,,chose_month]
  dimnames(L$sst) <- list(long = L$lons, lat = L$lats)
  ret <- melt(L$sst, value.name = "sst")
  cbind(date = L$month, ret, degF = ret$sst * 9/5 + 32)
}

#### plot each month
dev.off()
choose_month <- 1
chose_month_text <- "Jan"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_1 <- ggplot() +
  geom_tile(data = msst, aes(x = long, y = lat, fill = sst))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = "SLA",title = "January") +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))
# p_1

dev.off()
choose_month <- 2
chose_month_text <- "Feb"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_2 <- ggplot() +
  geom_tile(data = msst, aes(x = long, y = lat, fill = sst)) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_2

dev.off()
choose_month <- 3
chose_month_text <- "Mar"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_3 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_3

dev.off()
choose_month <- 4
chose_month_text <- "Apr"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_4 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_4

dev.off()
choose_month <- 5
chose_month_text <- "May"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_5 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_5

dev.off()
choose_month <- 6
chose_month_text <- "Jun"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_6 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_6

dev.off()
choose_month <- 7
chose_month_text <- "Jul"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_7 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_7

dev.off()
choose_month <- 8
chose_month_text <- "Aug"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_8 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_8

dev.off()
choose_month <- 9
chose_month_text <- "Sep"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_9 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_9

dev.off()
choose_month <- 10
chose_month_text <- "Oct"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_10 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_10

dev.off()
choose_month <- 11
chose_month_text <- "Nov"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_11 <- ggplot() +
  geom_raster(data = msst, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_11

dev.off()
choose_month <- 12
chose_month_text <- "Dec"
title_legend <- paste("SST", chose_month_text)

msst <- melt_sst(sst_monthly,choose_month)

p_12 <- ggplot() +
  geom_tile(data = msst, aes(x = long, y = lat, fill = sst)) + 
  scale_fill_gradientn(colours = rev(viridis(20)),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()
# p_12

p_1+p_3+p_5+p_7+p_9+p_11+
  plot_layout(ncol = 3, nrow = 2, guides = 'collect')

ggsave('plots/spatial/Abrolhos_SST_monthly_spatial.png', dpi = 300, width = 9, height = 9)

##### ACIDIFICATION #####
setwd(data_dir)
dir()
load(paste(Zone,'acd_data.Rdata', sep = '_'))

legend_title = "Season"
acd_mean_plot <- ggplot(data = acd_ts_monthly, aes(x = year, y = acd_mean)) + 
  geom_line() +
  geom_errorbar(aes(ymin = acd_mean-acd_sd, ymax = acd_mean+acd_sd), size = 0.2, width = 1, position = position_dodge(0.05)) +
  theme_minimal() +
  labs(x = "Year", y = "pH", title = "NW")
acd_mean_plot #plot with the other time series


##### Average plots - time series ####
sla_ts <- as.data.frame(dates_sla)
sla_ts$mean_sla <- apply(sla_all, 3, mean, na.rm = TRUE)
sla_ts$sd_sla <- apply(sla_all, 3, sd, na.rm = TRUE)
sla_ts$month <- as.numeric(format(as.Date(sla_ts$dates_sla), "%m"))

sla_tss <- sla_ts %>% dplyr::mutate(year = ifelse(month < 4, as.numeric(format(as.Date(dates_sla), "%Y"))-1, as.numeric(format(as.Date(dates_sla), "%Y")))) %>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", month %in% c(12,1,2) ~ "Summer", 
                                   month %in% c(3,4,5) ~ "Autumn", month %in% c(9,10,11) ~ "Spring" )) %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarise(sla_mean_sea = mean(mean_sla, na.rm = TRUE), sla_sd_sea = mean(sd_sla, na.rm = TRUE)) %>%
  glimpse()

#get only winter and summer to plot
sla_plot <- sla_tss %>% filter(grepl('Winter|Summer', season))

#plot for sla, summer and winter mean
legend_title = "Season"
sla_mean_plot <- ggplot(data = sla_plot, aes(x = year, y = sla_mean_sea, color = season)) + 
  geom_line() + geom_point()+
  geom_errorbar(aes(ymin = sla_mean_sea-sla_sd_sea, ymax = sla_mean_sea+sla_sd_sea), size = 0.2, width = 1, position = position_dodge(0.05)) +
  theme_minimal() +
  labs(x = "Year", y = "SLA mean (m)", color = legend_title) 
sla_mean_plot

sst_ts <- as.data.frame(dates_sst)
sst_ts$mean_sst <- apply(sst_all, 3, mean, na.rm = TRUE)
sst_ts$sd_sst <- apply(sst_all, 3, sd, na.rm = TRUE)
sst_ts$month <- as.numeric(format(as.Date(sst_ts$dates_sst), "%m"))

sst_tss <- sst_ts %>% mutate(year = ifelse(month < 4, as.numeric(format(as.Date(dates_sst), "%Y"))-1, as.numeric(format(as.Date(dates_sst), "%Y")))) %>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", month %in% c(12,1,2) ~ "Summer", 
                                   month %in% c(3,4,5) ~ "Autumn", month %in% c(9,10,11) ~ "Spring" )) %>%
  group_by(year, season) %>%
  summarise(sst_mean = mean(mean_sst, na.rm = TRUE),sd_sst = mean(sd_sst, na.rm = TRUE)) %>%
  glimpse()

sst_plot <- sst_tss %>% filter(grepl('Winter|Summer', season))

#plot for sst, summer and winter mean
legend_title = "Season"
sst_mean_plot <- ggplot(data = sst_plot, aes(x = year, y = sst_mean, color = season)) + 
  geom_line() +geom_point()+
  geom_errorbar(aes(ymin = sst_mean-sd_sst, ymax = sst_mean+sd_sst), size = 0.2, width = 1, position = position_dodge(0.05)) +
  theme_minimal() +
  labs(x = "Year", y = expression(paste("SST (",degree~C,")")), color = legend_title) 
sst_mean_plot

acd_mean_plot+sla_mean_plot+sst_mean_plot + plot_layout(ncol = 1, nrow = 3)#, widths = c(0.8, 2.2))

setwd(plot_dir)
ggsave(paste(Zone,'acd_sla_sst_ts.png',sep = "_"), dpi = 300, width = 18, height = 30)





