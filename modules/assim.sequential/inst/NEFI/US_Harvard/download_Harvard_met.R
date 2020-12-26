library(tidyr)
library(tidyverse)
library(lubridate)

download_US_Harvard_met <- function(start_date, end_date) {
 
  
  date = seq(from = start_date, to = end_date, by = 'days')
  data = NA
  for(i in 1:length(date)){
    
    yy <- strftime(date[i], format = "%y")
    doy <- strftime(date[i], format = "%j")

    if(file.exists(paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy, doy,'.csv'))){
      data1 = read.csv(paste0('/fs/data3/kzarada/NEFI/US_Harvard/Flux_data/', yy,doy,'.csv'), header = T, sep = "")
      data = rbind(data, data1)}                 
    
  }
  
  
  data <- data  %>% 
    drop_na(TIME_START.YYYYMMDDhhmm) %>% 
    mutate(Time = lubridate::with_tz(as.POSIXct(strptime(TIME_START.YYYYMMDDhhmm, format = "%Y%m%d%H%M", tz = "EST"),tz = "EST"), tz = "UTC")) %>% 
    dplyr::select(Time, Wspd.m.s.1, Ta.C, RH)
  
  colnames(data) <- c("date", "ws", "Tair", "rH")
  
  
  date = lubridate::force_tz(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by = "30 mins"), tz = "UTC")
  
  data.full = data.frame(date, ws = rep(NA, length(date)), Tair = rep(NA, length(date)), rH =  rep(NA, length(date)) )
  
  
  
  

  
  data.full$ws <- data$ws[match(date, data$date)]
  data.full$Tair <- data$Tair[match(date, data$date)]
  data.full$rH <- data$rH[match(date, data$date)]
  
  
  return(data.full)
  
} #end function
  