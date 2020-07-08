library(tidyr)
library(tidyverse)
library(lubridate)
library(PEcAn.all)

download_US_Harvard  <- function(start_date, end_date) {
  
  if(end_date > Sys.Date()) end_date = Sys.Date()
  date = seq(from = start_date, to = end_date, by = 'days')
  data = NA
  for(i in 1:length(date)){
    
    yy <- strftime(date[i], format = "%y")
    doy <- strftime(date[i], format = "%j")
    my_host <- list(name = "geo.bu.edu", user = 'kzarada', tunnel = "/tmp/tunnel")
    
    try(remote.copy.from(host = my_host, src = paste0('/projectnb/dietzelab/NEFI_data/HFEMS_prelim_', yy, '_', doy, '_dat.csv'), 
                         dst = paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy, doy,'.csv'), delete=FALSE))
    
    
    
    if(file.exists(paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy, doy,'.csv'))){
      data1 = read.csv(paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy,doy,'.csv'), header = T, sep = "")
      data = rbind(data, data1)}                 
    
  }
  
  
  data <- data  %>% 
    drop_na(TIME_START.YYYYMMDDhhmm) %>% 
    mutate(Time = lubridate::with_tz(as.POSIXct(strptime(TIME_START.YYYYMMDDhhmm, format = "%Y%m%d%H%M", tz = "EST"),tz = "EST"), tz = "UTC")) %>% 
    dplyr::select(Time, LHF.W.m.2, Fco2.e.6mol.m.2.s.1)
  
  colnames(data) <- c("Time", "LE", "NEE")
  
  
  Time = lubridate::force_tz(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by = "30 mins"), tz = "UTC")
  
  data.full = data.frame(Time, NEE = rep(NA, length(Time)), LE = rep(NA, length(Time)))
  
  
  
  
  match(Time, data$Time)
  
  data.full$NEE <- data$NEE[match(Time, data$Time)]
  data.full$LE <- data$NEE[match(Time, data$Time)]
  data.full$NEE <- PEcAn.utils::misc.convert(data.full$NEE, "umol C m-2 s-1", "kg C m-2 s-1") 
  
  
  return(data.full)
  }


#manually check if files are available 
#read.csv('ftp://ftp.as.harvard.edu/pub/exchange/jwm/Forecast_data/HFEMS_prelim_19_330_dat.csv')






