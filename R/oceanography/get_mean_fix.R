sst_all <- var.get.nc(nc_file_to_get_sst,'sea_surface_temperature', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sst)));
sst_all <- kelvin.to.celsius(sst_all, round = 2) 

time_data <- list()
time_data$dates <- dates_sst
time_data$month <- lubridate::month(as.POSIXlt(time_data$dates, format="%Y-%m-%d"))

melt_sst_all <- function(chose_month) {
  L<- list()
  L$lats <- check_lat
  L$lons <- check_lon
  
  time_i <- which(time_data$month == chose_month) 
  sst_month <- sst_all[,,time_i]
  L$month <- chose_month
  
  dimnames(sst_month)[[1]] <- L$lons
  dimnames(sst_month)[[2]] <- L$lats
  dimnames(sst_month)[[3]] <- rep(chose_month,length(time_i))
  
  L$sst_month <- sst_month
  
  ret <- melt(L$sst_month, value.name = "sst") %>% rename(Lon = Var1, Lat = Var2) %>% glimpse()
}

plot_sst <- ret %>% group_by(Lon, Lat) %>% summarise(sst = mean(sst,na.rm = TRUE)) %>% glimpse()
