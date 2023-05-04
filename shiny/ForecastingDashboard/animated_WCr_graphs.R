
setwd('/fs/data3/kzarada/NEFI/Willow_Creek')
library("ggplot2")
library("plotly")
library("gganimate")
library("tidyverse")
source("/fs/data3/kzarada/NEFI/Willow_Creek/wcr.graphs.R")
source("/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek/download_WCr_met.R")
### Site numbers ###
# WCr = 676
# Syv = 622
#Wlef = 678
# Los = 679
frame_end = Sys.Date() + lubridate::days(16)
frame_start = Sys.Date() - lubridate::days(10)

ftime = seq(as.Date(frame_start), as.Date(frame_end), by="days")
ctime = seq(as.Date(frame_start), Sys.Date(), by = "days")
vars = c("NEE", "LE")
site = 676
for(j in 1:length(vars)){


for(i in 1:length(ctime)){
  
  args = c(as.character(ctime[i]), vars[j], site)
  
  assign(paste0(ctime[i], "_", vars[j]), wcr.graphs(args))
  
}
}

NEE.index <- ls(pattern = paste0("_NEE"), envir=.GlobalEnv)
LE.index <- ls(pattern = paste0("_LE"), envir=.GlobalEnv)



nee.data = get(NEE.index[1])
for(i in 2:length(NEE.index)){
  
  nee.data = rbind(nee.data, get(NEE.index[i]))
}

le.data = get(LE.index[1])
for(i in 2:length(LE.index)){
  
  le.data = rbind(le.data, get(LE.index[i]))
}


nee.data$Time <- as.factor(paste(nee.data$date, nee.data$Time, sep = "_"))
nee.data$start_date <- as.factor(nee.data$start_date)

le.data$Time <- as.factor(paste(le.data$date, le.data$Time, sep = "_"))
le.data$start_date <- as.factor(le.data$start_date)

#Download observed data 
real_data <- PEcAn.data.atmosphere::download.US_WCr(frame_start, frame_end, timestep = 6)
real_data <- do.call(cbind.data.frame, real_data)
colnames(real_data) <- c("NEE", "LE")

#combine observed with predicted data 
real_data_nee <- as_tibble(real_data %>% dplyr::select(NEE) %>% mutate(Time= unique(nee.data$Time)))
real_data_le <- as_tibble(real_data %>% dplyr::select(LE) %>% mutate(Time= unique(le.data$Time)))

nee.data <- left_join(as_tibble(nee.data), real_data_nee, by = c("Time"), suffix = c("nee", "real"))
le.data <- left_join(as_tibble(le.data), real_data_le, by = c("Time"), suffix = c("le", "real"))


x.breaks <- unique(nee.data$Time[seq(1, length(nee.data$Time), by = 4)])


# These variables control the start and end dates of the y axis
nee_upper = 1e-07
nee_lower = min(nee.data$Lower)
qle_upper = max(le.data$Upper)
qle_lower = -50

p <-ggplot(nee.data, aes(group = start_date, ids = start_date, frame = start_date)) + #, label = NEE - Predicted
    geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
    geom_line(aes(x = Time, y = NEE, color = "Observed Data"), size = 1) + 
    geom_line(aes(x = Time, y = Predicted, color = "Predicted Mean")) + 
    ggtitle(paste0("Net Ecosystem Exchange for ", frame_start, " to ", frame_end, ", Willow Creek, Wisconsin")) +
    scale_color_manual(name = "Legend", labels = c("Predicted Mean", "Observed Data"), values=c("Predicted Mean" = "skyblue1", "Observed Data" = "firebrick4")) +
    scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
    scale_y_continuous(name="NEE (kg C m-2 s-1)", limits = c(nee_lower, nee_upper)) + 
    scale_x_discrete(name = "", breaks = x.breaks, labels = format(ftime[-length(ftime)], "%m-%d")) + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 

q <- ggplot(le.data, aes(group = start_date, ids = start_date, frame = start_date)) + #, label= LE - Predicted
  geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
  geom_line(aes(x = Time, y = LE, color = "Observed Data"), size = 1) + 
  geom_line(aes(x = Time, y = Predicted, color = "Predicted Mean")) + 
  ggtitle(paste0("Latent Energy for ", frame_start, " to ", frame_end, ", Willow Creek, Wisconsin")) +
  scale_color_manual(name = "Legend", labels = c("Predicted Mean", "Observed Data"), values=c("Predicted Mean" = "skyblue1", "Observed Data" = "firebrick4")) +
  scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
  scale_y_continuous(name="LE (W m-2 s-1)", limits = c(qle_lower, qle_upper)) + 
  scale_x_discrete(name = "", breaks = x.breaks, labels = format(ftime[-length(ftime)], "%m-%d")) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 

  
ggplot.nee<-ggplotly(p, tooltip = c("Time", "y", "Lower", "Upper")) %>% 
  animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
  animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
  animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
  layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
  layout(showlegend = T, margin = c(30,50,30,50)) 
  
  ggplot.nee$x$data[[1]]$name <-"95% Confidence Interval"
  ggplot.nee$x$data[[2]]$name <- "Observed Data"
  ggplot.nee$x$data[[3]]$name <- "Predicted Mean"

  
ggplot.le<-ggplotly(q, tooltip = c("Time", "y", "Lower", "Upper"), layerData = 2) %>% 
    animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
    animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
    animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
    layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
    layout(showlegend = T, margin = c(30,50,30,50)) 
  
  ggplot.le$x$data[[1]]$name <-"95% Confidence Interval"
  ggplot.le$x$data[[2]]$name <- "Observed Data"
  ggplot.le$x$data[[3]]$name <- "Predicted Mean"
  

#for shiny app 
  met = download_US_WCr_met(frame_start, Sys.Date())
  met$Time = paste0(format(met$date, "%Y-%m-%d"), "_" ,as.numeric(met$Hour))
  met <- as_tibble(met) %>% dplyr::select(Time, Tair, rH, Tsoil, Rg) %>% mutate(Time = as.factor(Time))
  nee.met <- nee.data %>% inner_join(met,nee.data,  by = c("Time"))
  

save.image("/fs/data3/kzarada/NEFI/Willow_Creek/wcr.RData")


