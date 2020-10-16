
#setwd('/fs/data3/kzarada/NEFI/Willow_Creek')
library("ggplot2")
library("plotly")
library("gganimate")
library("tidyverse")
library('PEcAn.all')
library("RCurl")
source("/fs/data3/kzarada/NEFI/sda.graphs.R")
#source("/fs/data3/kzarada/NEFI/Willow_Creek/download_WCr_met.R")


#WCR
WCR.num.SDA = 676
WCR.abv.SDA = "WCr"
WCR.outdir.SDA = '/fs/data3/kzarada/ouput/'
WCR.db.num.SDA = "0-676"


sda.tower.graphs <- function(site.num, site.abv, outdir, db.num){ 
  
  
  ### Site numbers ###
  # WCr = 676
  # Syv = 622
  # Wlef = 678
  # Los = 679
  frame_end = Sys.Date() + lubridate::days(16)
  frame_start = Sys.Date() - lubridate::days(10)
  
  ftime = seq(as.Date(frame_start), as.Date(frame_end), by="days")
  ctime = seq(as.Date(frame_start), Sys.Date(), by = "days") - lubridate::days(4)
  vars = c("NEE", "LE", "soil")
  
  for(j in 1:length(vars)){
    
    
    for(i in 1:length(ctime)){
      
      args = c(as.character(ctime[i]), vars[j], site.num, outdir)
      
      assign(paste0(ctime[i], "_", vars[j]), sda.graphs(args))
      
    }
  }
  NEE.index <- ls(pattern = paste0("_NEE"), envir=environment())
  LE.index <- ls(pattern = paste0("_LE"), envir=environment())
  soil.index <- ls(pattern = paste0("_soil"), envir=environment())
  
  
  nee.data = get(NEE.index[1])
  for(i in 2:length(NEE.index)){
    
    nee.data = rbind(nee.data, get(NEE.index[i]))
  }
  
  le.data = get(LE.index[1])
  for(i in 2:length(LE.index)){
    
    le.data = rbind(le.data, get(LE.index[i]))
  }
  
  soil.data = get(soil.index[1])
  for(i in 2:length(LE.index)){
    
    soil.data = rbind(soil.data, get(soil.index[i]))
  }
  
  nee.data$Time <- as.POSIXct(paste(nee.data$date, nee.data$Time, sep = " "), format = "%Y-%m-%d %H")
  nee.data$Time <- lubridate::force_tz(nee.data$Time, "UTC")
  nee.data$start_date <- as.factor(nee.data$start_date)
  
  le.data$Time <- as.POSIXct(paste(le.data$date, le.data$Time, sep = " "), format = "%Y-%m-%d %H")
  le.data$Time <- lubridate::force_tz(le.data$Time, "UTC")
  le.data$start_date <- as.factor(le.data$start_date)
  
  soil.data$Time <- as.POSIXct(paste(soil.data$date, soil.data$Time, sep = " "), format = "%Y-%m-%d %H")
  soil.data$Time <- lubridate::force_tz(soil.data$Time, "UTC")
  soil.data$start_date <- as.factor(soil.data$start_date)
  
  Time = seq(from = head(unique(nee.data$date), 1), to = tail(unique(nee.data$date), 1), by = 1)
  #Download observed data 
  source(paste0('/fs/data3/kzarada/NEFI/US_', site.abv,"/download_", site.abv, ".R"))
  real_data <- do.call(paste0("download_US_", site.abv), list(Time[1], last(Time)))
  real_data$Time = lubridate::with_tz(as.POSIXct(real_data$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "UTC")
  
  
  
  #Time1 <- lubridate::with_tz(seq(from = as.POSIXct(frame_start, tz = "UTC"), to = as.POSIXct(frame_end, tz = "UTC"), by ="hour"), "UTC")
  #Time1 <- Time1[-1] #first time isn't included 
  
  #combine observed with predicted data 
  real_data_nee <- as_tibble(real_data %>% dplyr::select(Time, NEE))
  real_data_le <- as_tibble(real_data %>% dplyr::select(Time, LE))
  
  nee.data <- left_join(as_tibble(nee.data), real_data_nee, by = c("Time"), suffix = c("nee", "real")) 
  le.data <- left_join(as_tibble(le.data), real_data_le, by = c("Time"), suffix = c("le", "real"))
  
  if(file.exists(paste0('/fs/data3/kzarada/NEFI/US_', site.abv, '/download_soilmoist_', site.abv, '.R'))){ 
    source(paste0('/fs/data3/kzarada/NEFI/US_', site.abv, '/download_soilmoist_', site.abv, '.R')) 
    real_soil <- do.call(paste0("download_soilmoist_", site.abv), list(frame_start, frame_end))
    soil.data <- left_join(as_tibble(soil.data), real_soil, by = c("Time"), suffic = c("soil", "real"))
    soil.data$avgsoil = soil.data$avgsoil/100
    
    Time = lubridate::with_tz(as.POSIXct(Time), tz = "UTC")
    x.breaks <- match(Time, nee.data$Time)
    x.breaks <- x.breaks[!is.na(x.breaks)]
    
    s <-ggplot(soil.data, aes(group = start_date, ids = start_date, frame = start_date)) + #, label= LE - Predicted
      geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
      geom_line(aes(x = Time, y = avgsoil, color = "Observed Data"), size = 1) + 
      geom_line(aes(x = Time, y = Predicted, color = "Predicted Mean")) + 
      ggtitle(paste0("Soil Moisture for ", frame_start, " to ", frame_end,", at ", site.abv)) +
      scale_color_manual(name = "Legend", labels = c("Predicted Mean", "Observed Data"), values=c("Predicted Mean" = "skyblue1", "Observed Data" = "firebrick4")) +
      scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
      scale_y_continuous(name="Soil Moisture (%)") + #, limits = c(qle_lower, qle_upper)) + 
      scale_x_discrete(name = "", breaks = x.breaks, labels = format(Time, "%m-%d")) + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 
    
    
    
    ggplot.soil<-ggplotly(s, tooltip = 'all', layerData = 2) %>% 
      animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
      animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
      animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
      layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
      layout(showlegend = T, margin = c(30,50,30,50)) 
    
    ggplot.soil$x$data[[1]]$name <-"95% Confidence Interval"
    ggplot.soil$x$data[[2]]$name <- "Observed Data"
    ggplot.soil$x$data[[3]]$name <- "Predicted Mean"
    
    soil.data$error = soil.data$avgsoil - soil.data$Predicted
    
  } else(s = "NA")
  
  
  Time = lubridate::with_tz(as.POSIXct(Time), tz = "UTC")
  x.breaks <- match(Time, nee.data$Time)
  x.breaks <- x.breaks[!is.na(x.breaks)]
  
  
  # These variables control the start and end dates of the y axis
  nee_upper = max(nee.data %>% dplyr::select(Upper, Lower, Predicted, NEE), na.rm = TRUE)
  nee_lower = min(nee.data %>% dplyr::select(Upper, Lower, Predicted, NEE), na.rm = TRUE)
  
  qle_upper = max(le.data %>% dplyr::select(Upper, Predicted, LE) %>% drop_na())
  qle_lower = min(le.data %>% dplyr::select(Lower, Predicted, LE) %>% drop_na())
  
  p <-ggplot(nee.data, aes(group = start_date, ids = start_date, frame = start_date)) + 
    geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
    geom_line(aes(x = Time, y = NEE, color = "Observed Data"), size = 1) + 
    geom_line(aes(x = Time, y = Predicted, color = "Predicted Mean")) + 
    ggtitle(paste0("Net Ecosystem Exchange for ", frame_start, " to ", frame_end, ", at ", site.abv)) +
    scale_color_manual(name = "Legend", labels = c("Predicted Mean", "Observed Data"), values=c("Predicted Mean" = "skyblue1", "Observed Data" = "firebrick4")) +
    scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
    scale_y_continuous(name="NEE (kg C m-2 s-1)", limits = c(nee_lower, nee_upper)) + 
    scale_x_discrete(name = "", breaks = x.breaks, labels = format(Time, "%m-%d")) + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 
  
  q <- ggplot(le.data, aes(group = start_date, ids = start_date, frame = start_date)) + #, label= LE - Predicted
    geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
    geom_line(aes(x = Time, y = LE, color = "Observed Data"), size = 1) + 
    geom_line(aes(x = Time, y = Predicted, color = "Predicted Mean")) + 
    ggtitle(paste0("Latent Energy for ", frame_start, " to ", frame_end, ", at ", site.abv)) +
    scale_color_manual(name = "Legend", labels = c("Predicted Mean", "Observed Data"), values=c("Predicted Mean" = "skyblue1", "Observed Data" = "firebrick4")) +
    scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
    scale_y_continuous(name="LE (W m-2 s-1)") + #, limits = c(qle_lower, qle_upper)) + 
    scale_x_discrete(name = "", breaks = x.breaks, labels = format(Time, "%m-%d")) + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 
  
  
  
  ggplot.nee<-ggplotly(p, tooltip = 'all') %>% 
    animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
    animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
    animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
    layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
    layout(showlegend = T, margin = c(30,50,30,50)) 
  
  ggplot.nee$x$data[[1]]$name <-"95% Confidence Interval"
  ggplot.nee$x$data[[2]]$name <- "Observed Data"
  ggplot.nee$x$data[[3]]$name <- "Predicted Mean"
  
  
  ggplot.le<-ggplotly(q, tooltip = 'all', layerData = 2) %>% 
    animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
    animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
    animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
    layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
    layout(showlegend = T, margin = c(30,50,30,50)) 
  
  ggplot.le$x$data[[1]]$name <-"95% Confidence Interval"
  ggplot.le$x$data[[2]]$name <- "Observed Data"
  ggplot.le$x$data[[3]]$name <- "Predicted Mean"
  
  
  
  #for shiny app 
  if(file.exists(paste0("/fs/data3/kzarada/NEFI/US_", site.abv, "/download_", site.abv,"_met.R"))){
    source(paste0("/fs/data3/kzarada/NEFI/US_", site.abv, "/download_", site.abv,"_met.R"))
    met = do.call(paste0("download_US_", site.abv,"_met"), list(frame_start, Sys.Date()))
    
    if("Tsoil" %in% names(met)){
      met <- as_tibble(met) %>% mutate(Time = as.POSIXct(date)) %>% dplyr::select(Time, Tair,Tsoil, rH)
    }else{met <- as_tibble(met) %>% mutate(Time = as.POSIXct(date)) %>% dplyr::select(Time, Tair, rH)}
    
    nee.met <- nee.data %>% inner_join(met,nee.data,  by = c("Time"))
    
    #Calculate Error 
    nee.met$error <- (nee.met$NEE - nee.met$Predicted)
  }
  
  nee.data$error = nee.data$NEE - nee.data$Predicted
  le.data$error = le.data$LE - le.data$Predicted
  
  #for met comparison 
  
  library(ncdf4)
  forecast.path <- paste0("/fs/data3/kzarada/pecan.data/dbfiles/NOAA_GEFS_downscale_site_", db.num, "/")
  
  forecasted_data <- data.frame()
  #### stopping here-- need to make sure that it goes through each dir index and saves and then moves on
  dirs <- list.dirs(path = forecast.path)
  dir.1 <- dirs[grepl(paste0(".21.", Sys.Date(), "T*"), dirs)]
  nc.files = list()
  index = list()
  dir.index = list()
  
  index= strsplit(dir.1[1], split = ".21.20")[[1]][2]
  dir.index= dirs[grepl(index[1], dirs)]
  
  
  
  for(k in 1:21){ 
    nc.files[k]<- list.files(path = dir.index[k], pattern = '*.nc' )
  }
  
  forecasted_data <- data.frame()
  for(i in 1:21){
    setwd(dir.index[i])
    nc <- nc_open(nc.files[[i]][1])
    sec <- nc$dim$time$vals
    sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
    dt <- mean(diff(sec), na.rm=TRUE)
    tstep <- round(86400 / dt)
    dt <- 86400 / tstep
    
    
    Tair <-ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
    Tair_C <- udunits2::ud.convert(Tair, "K", "degC")
    Qair <-ncdf4::ncvar_get(nc, "specific_humidity")  #humidity (kg/kg)
    ws <- try(ncdf4::ncvar_get(nc, "wind_speed"))
    if (!is.numeric(ws)) {
      U <- ncdf4::ncvar_get(nc, "eastward_wind")
      V <- ncdf4::ncvar_get(nc, "northward_wind")
      ws <- sqrt(U ^ 2 + V ^ 2)
      PEcAn.logger::logger.info("wind_speed absent; calculated from eastward_wind and northward_wind")
    }
    
    
    Rain <- ncdf4::ncvar_get(nc, "precipitation_flux")
    pres <- ncdf4::ncvar_get(nc,'air_pressure') ## in pascal
    SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## in W/m2
    LW <- ncdf4::ncvar_get(nc, "surface_downwelling_longwave_flux_in_air")
    RH <- PEcAn.data.atmosphere::qair2rh(Qair, Tair_C, press = 950)
    
    file.name <- nc.files[[i]][1]
    
    hour <- strsplit(strsplit(index, split = "T")[[1]][2], split = ".20")[[1]][1]
    
    start_date <- as.POSIXct(paste0(strsplit(strsplit(nc$dim$time$units, " ")[[1]][3], split = "T")[[1]][1]," ", hour), format = "%Y-%m-%d %H:%M")
    sec <- nc$dim$time$vals
    
    
    timestamp <- seq(from = start_date + lubridate::hours(6), by = "6 hour", length.out = length(sec))
    ensemble <- rep(i, times = length(timestamp))
    tmp <- as.data.frame(cbind(
      ensemble,
      Tair_C, 
      Qair,
      RH, 
      Rain = Rain * dt,
      ws, 
      SW, 
      LW
    ))
    tmp$timestamp <- timestamp
    nc_close(nc)
    forecasted_data <- rbind(forecasted_data, tmp)
  } 
  
  forecasted_data$ensemble = as.factor(forecasted_data$ensemble)
  
  
  
  
  #Save Rdata for shiny app  
  save(list = ls(), file = paste0("/srv/shiny-server/Flux_Dashboard/data/", site.abv, ".SDA.RData"))
  save(list = ls(), file = paste0("/fs/data3/kzarada/NEFI/", site.abv, ".SDA.RData"))
  
  
  print(ls())
} 


sda.tower.graphs(WCR.num.SDA,
             WCR.abv.SDA,
             WCR.outdir.SDA,
             WCR.db.num.SDA)




