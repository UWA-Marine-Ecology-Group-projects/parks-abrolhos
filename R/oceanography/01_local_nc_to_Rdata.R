###
# Project: Parks - Abrolhos
# Data:    Oceanography - SST, SLA, currents & acidification
# Task:    Load in netCDF files from local copy
# author:  Jess Kolbusz & Claude
# date:    Feb 2022
##

#### --- FILES ARE TOO BIG FOR GITHUB
#### --- IN GITIGNORE

# Clear memory----
rm(list=ls())

library(dplyr)
library(magrittr)
library(RNetCDF)
library(weathermetrics)
library(iemisc)

## get data locations /limits that need from MPA

#set working directory
working.dir <- getwd()
setwd(working.dir)

locations <-   read.csv("data/spatial/oceanography/network_scale_boundaries.csv", header = TRUE) %>%
  glimpse()

#i use the "zone" column for each since it distinguishes them all spatially
Zone <- 'Abrolhos' #NW or SW
locs <- locations[locations$Zone %in% c(Zone), ]               # just wa parks nearby

#gets bounds
Lon_w <- locs$lon_w
Lon_e <- locs$lon_e
Lat_n <- locs$lat_n
Lat_s <- locs$lat_s

######### SEA LEVEL ANOMALY #########
#location of netcdf sla data
# setwd(paste(wd_data_loc,'oceanography/OceanCurrent_IMOS', sep = '/'))
# dir()

## add in where data from etc. 
#the numbers are just how the download from IMOS works - can rename the files if easier
#Altimeter and tidegauge estimates of adjusted sea level anomaly mapped onto a grid using optimal interpolation (OI)
#print.nc also shows reference for the data 
#IMOS - OceanCurrent - Gridded sea level anomaly - Delayed mode - DM01
#There is also this one you could look at - IMOS - OceanCurrent - Gridded sea level anomaly - Near real time
#but I looks like there is less QA/QC applied

#auto gets the nc file
filename_nc <- Sys.glob("*.nc")
nc_file_to_get_sla <- open.nc(filename_nc,write = TRUE)
print.nc(nc_file_to_get_sla) #shows you all the file details

#plot lims for mapstime_nc<- var.get.nc(nc_file_to_get_sla, 'TIME')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
#get time out of nc file
time_nc <- var.get.nc(nc_file_to_get_sla, 'TIME') #some latitude, some lat -> watch for spelling
time_nc_sla <- utcal.nc("days since 1981-01-01 00:00:00", time_nc,type = "c") #specifed in the nc file what the time stamp is 
dates_sla <- as.Date(time_nc_sla)

lat <- var.get.nc(nc_file_to_get_sla, 'LATITUDE') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_sla, 'LONGITUDE')

#get lats in nc file which intersect with lats and lons from the marine park
lat_i <- which(lat <= Lat_n & lat >= Lat_s) 
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values of lat and lon that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sea level anomalies and currents from the imos netcdf file that's
#already been downloaded
#start for time is 1 since we want all the time
sla_all <- var.get.nc(nc_file_to_get_sla,'GSLA', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla))); #sea level anomaly
ucur_all <- var.get.nc(nc_file_to_get_sla,'UCUR', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla))); #sea level anomaly
vcur_all <- var.get.nc(nc_file_to_get_sla,'VCUR', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla))); #sea level anomaly

#gets mean monthly value, same size for sla and currents since using same grid (lat and lon) and data
size_matrix <- size(sla_all) #all should be the same size
R <- size_matrix[1]
C <- size_matrix[2]
Z <- size_matrix[3]
sla_monthly <- tapply(sla_all,list(rep(1:R,C*Z),rep(1:C,each=R,times=Z),rep(strftime(dates_sla,'%m'),each=R*C)),mean, na.rm = TRUE);
uu_monthly <- tapply(ucur_all,list(rep(1:R,C*Z),rep(1:C,each=R,times=Z),rep(strftime(dates_sla,'%m'),each=R*C)),mean, na.rm = TRUE);
vv_monthly <- tapply(vcur_all,list(rep(1:R,C*Z),rep(1:C,each=R,times=Z),rep(strftime(dates_sla,'%m'),each=R*C)),mean, na.rm = TRUE);

#data to save is the time, lat, lon, whole sla, ucur and vcur and monthly averages for the plots
lat_sla <- check_lat
lon_sla <- check_lon
file_name <- paste(Zone,"sla_current_data.Rdata",sep = '_')

setwd(paste(wd_data_save,Zone, sep = '/'))
save(list = c("dates_sla","lat_sla","lon_sla","sla_all","sla_monthly","ucur_all","uu_monthly","vcur_all","vv_monthly"),
     file = file_name)

######### SST #########

####----Claude re-doing with 6 day average temperature

#IMOS - SRS - SST - L3S - Single Sensor - 1 month - day and night time - Australia
#just got the monthly data file cause it was smaller and we only need the monthly
# filename_nc <- Sys.glob("*.nc")
nc_file_to_get_sst <- open.nc("data/spatial/oceanography/large/IMOS_aggregation_20220224T013630Z/IMOS_aggregation_20220224T013630Z.nc",write = TRUE)
print.nc(nc_file_to_get_sst) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_sst, 'time')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sst <- utcal.nc("seconds since 1981-01-01 00:00:00", time_nc,type = "c")
dates_sst <- as.Date(time_nc_sst)

lat <- var.get.nc(nc_file_to_get_sst, 'lat') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_sst, 'lon')

#get lats of sst file which correspond to the lats of the zone
lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sst
sst_all <- var.get.nc(nc_file_to_get_sst,'sea_surface_temperature', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sst)));
sst_all <- kelvin.to.celsius(sst_all, round = 2) 

# # gets mean monthly value
# melt_sst_all <- function(chose_month) {
#   L<- list()
#   L$lats <- check_lat
#   L$lons <- check_lon
# 
#   time_i <- which(time_data$month == chose_month)
#   sst_month <- sst_all[,,time_i]
#   L$month <- chose_month
# 
#   dimnames(sst_month)[[1]] <- L$lons
#   dimnames(sst_month)[[2]] <- L$lats
#   dimnames(sst_month)[[3]] <- rep(chose_month,length(time_i))
# 
#   L$sst_month <- sst_month
# 
#   ret <- melt(L$sst_month, value.name = "sst") %>% rename(Lon = Var1, Lat = Var2) %>% glimpse()
# }
# 
# plot_sst <- ret %>% group_by(Lon, Lat) %>% summarise(sst = mean(sst,na.rm = TRUE)) %>% glimpse()

size_matrix <- size(sst_all)
R <- as.numeric(size_matrix[1])
C <- as.numeric(size_matrix[2])
Z <- as.numeric(size_matrix[3])
sst_monthly <- tapply(sst_all,list(rep(1:R,C*Z),rep(1:C,each=R,times=Z),rep(strftime(dates_sst,'%m'),each=R*C)),mean, na.rm = TRUE);

#data to save is the time, lat, lon, whole sst, monthly sst
lat_sst <- check_lat
lon_sst <- check_lon
file_name <- paste(Zone,"sst_data.Rdata",sep = '_')

save(list = c("dates_sst","lat_sst","lon_sst","sst_all","sst_monthly"),
     file = file_name)

##### Acidification ####
setwd(paste(wd_data_loc,'acidification', sep = '/'))
dir()

#Ocean_acidification_historical_reconstructionfrom AODN portal
filename_nc <- Sys.glob("*.nc")
nc_file_to_get_acd <- open.nc(filename_nc,write = TRUE)
print.nc(nc_file_to_get_acd) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_acd, 'TIME')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_acd <- utcal.nc("months since 1800-01-01 00:00:00", time_nc,type = "c")
dates_acd <- as.Date(time_nc_acd)

lat <- var.get.nc(nc_file_to_get_acd, 'LATITUDE') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_acd, 'LONGITUDE')

lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sea level anomalies
acd_all <- var.get.nc(nc_file_to_get_acd,'pH_T', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_acd))); #sea level anomaly

## get average plots - time series
acd_ts_all <- as.data.frame(dates_acd)

#different function to get mean since is only 2 in certain direction
acd_ts_all$acdd <-apply(acd_all, 2, mean, na.rm = TRUE) #for larger ares is in 3D so use apply(acd_all, 3, mean, na.rm = TRUE) #acd_all for monties is only 1 cell #for abrolhos -> apply(acd_all, 2, mean, na.rm = TRUE) #3 is 3rd dumension
acd_ts_all$month <- as.numeric(format(as.Date(acd_ts_all$dates_acd), "%m"))
acd_ts_all$year <- as.numeric(format(as.Date(acd_ts_all$dates_acd), "%Y"))

acd_ts_monthly <- acd_ts_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(acd_mean = mean(acdd, na.rm = TRUE), acd_sd = sd(acdd, na.rm = TRUE)) %>%
  glimpse()

## save acidification, don't need to get lat and lon for acd since is only time series 
file_name <- paste(Zone,"acd_data.Rdata",sep = '_')

setwd(paste(wd_data_save,Zone, sep = '/'))
save(list = c("acd_ts_monthly","acd_ts_all"),
     file = file_name)


###### -----DEGREE HEATING WEEKS
nc_file_to_get_dhw <- open.nc("data/spatial/oceanography/large/DHW_2021/dhw_5km_82f1_a212_461c.nc",write = TRUE)
print.nc(nc_file_to_get_dhw) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_dhw, 'time')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_dhw <- utcal.nc("seconds since 1970-01-01 00:00:00", time_nc,type = "c")
dates_dhw <- as.Date(time_nc_dhw)

lat <- var.get.nc(nc_file_to_get_dhw, 'latitude') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_dhw, 'longitude')

#get lats of sst file which correspond to the lats of the zone
lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sst
dhw_all <- var.get.nc(nc_file_to_get_dhw,'CRW_DHW', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_dhw)));
# dhw_all <- as.data.frame.table(dhw_all)

#gets mean monthly value 

size_matrix <- size(dhw_all)
R <- as.numeric(size_matrix[1])
C <- as.numeric(size_matrix[2])
Z <- as.numeric(size_matrix[3])
dhw_monthly <- tapply(dhw_all,list(rep(1:R,C*Z),rep(1:C,each=R,times=Z),rep(strftime(dates_dhw,'%m'),each=R*C)),mean, na.rm = TRUE);

#data to save is the time, lat, lon, whole sst, monthly sst
lat_sst <- check_lat
lon_sst <- check_lon
file_name <- paste(Zone,"sst_data.Rdata",sep = '_')

setwd(paste(wd_data_save,Zone, sep = '/'))
save(list = c("dates_sst","lat_sst","lon_sst","sst_all","sst_monthly"),
     file = file_name)




