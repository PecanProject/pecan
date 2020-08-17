library(tidyr)
library(tidyverse)
library(lubridate)
library(PEcAn.all)

download_soilmoist_Harvard  <- function(start_date, end_date) {
  
  if(end_date > Sys.Date()) end_date = Sys.Date()
  if(start_date < as.Date("2019-11-06")) start_date = "2019-11-06"
  date = seq(from = as.Date(start_date), to = as.Date(end_date), by = 'days')
  data = NA
  for(i in 1:length(date)){
    
    yy <- strftime(date[i], format = "%y")
    doy <- strftime(date[i], format = "%j")
    #my_host <- list(name = "geo.bu.edu", user = 'kzarada', tunnel = "/tmp/tunnel")
    
    #try(remote.copy.from(host = my_, src = paste0('/projectnb/dietzelab/NEFI_data/HFEMS_prelim_', yy, '_', doy, '_dat.csv'), 
                         #dst = paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy, doy,'.csv'), delete=FALSE))
    
    
    
    if(file.exists(paste0('/projectnb/dietzelab/NEFI_data/HFEMS_prelim_', yy, '_', doy, '_dat.csv'))){
      data1 = read.csv(paste0('/projectnb/dietzelab/NEFI_data/HFEMS_prelim_', yy, '_', doy, '_dat.csv'), header = T, sep = "")
      data = rbind(data, data1)}                
    
  }
  
  
  data <- data  %>% 
    drop_na(TIME_START.YYYYMMDDhhmm) %>% 
    mutate(Time = lubridate::with_tz(as.POSIXct(strptime(TIME_START.YYYYMMDDhhmm, format = "%Y%m%d%H%M", tz = "EST"),tz = "EST"), tz = "UTC")) %>% 
    dplyr::select(Time, SWC15)
  
  colnames(data) <- c("Time", "SWC15")
  
  
  Time = lubridate::force_tz(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by = "30 mins"), tz = "UTC")
  
  data.full = data.frame(Time, SWC15 = rep(NA, length(Time)))
  
  
  
  
  match(Time, data$Time)
  
  data.full$SWC15 <- data$SWC15[match(Time, data$Time)]

  
  
  return(data.full)
}


#manually check if files are available 
#read.csv('ftp://ftp.as.harvard.edu/pub/exchange/jwm/Forecast_data/HFEMS_prelim_20_196_dat.csv')
