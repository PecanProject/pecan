setwd('/fs/data3/kzarada/NEFI/Willow_Creek')
library("ggplot2")
library("plotly")
library("gganimate")
library("tidyverse")
library("htmltools")
source("/fs/data3/kzarada/NEFI/Willow_Creek/wcr.graphs.R")

### FORECAST 
vars = c("NEE", "LE")
site = 676
for(j in 1:length(vars)){
  
      args = c(as.character(Sys.Date()), vars[j], site)
    
      assign(paste0("data_", vars[j]), wcr.graphs(args))
    
  }

nee.data = get("data_NEE")
le.data = get("data_LE")

nee.data$Time <- as.factor(paste(nee.data$date, nee.data$Time, sep = "_"))
nee.data$start_date <- as.factor(nee.data$start_date)

le.data$Time <- as.factor(paste(le.data$date, le.data$Time, sep = "_"))
le.data$start_date <- as.factor(le.data$start_date)

x.breaks <- unique(nee.data$time[seq(1, length(nee.data$time), by = 4)])
labels <- format(seq.Date(from = Sys.Date(), by = "day", length.out = 16), "%m-%d")

nee <- ggplot(nee.data) + 
  geom_ribbon(aes(x = time, ymin=Lower, ymax=Upper, fill="95% confidence interval"), alpha = 0.4) + 
  geom_line(aes(x=time, y=Predicted, color="Predicted"), size = 1) +
  ggtitle(paste0("Net Ecosystem Exchange Forcast for ", Sys.Date())) +
  scale_x_continuous(name="Time (days)", breaks = x.breaks, labels = labels) + 
  scale_y_continuous(name="NEE (kg C m-2 s-1)") + 
  scale_colour_manual(name='Legend', values=c("Predicted"="lightskyblue1")) +
  scale_fill_manual(name=element_blank(), values=c("95% confidence interval" = "blue3")) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 



le <- ggplot(le.data) + 
  geom_ribbon(aes(x = time, ymin=Lower, ymax=Upper, fill="95% confidence interval"), alpha = 0.4) + 
  geom_line(aes(x=time, y=Predicted, color="Predicted"), size = 1) +
  ggtitle(paste0("Latent Energy Forcast for ", Sys.Date())) +
  scale_x_continuous(name="Time (days)", breaks = x.breaks, labels = labels) + 
  scale_y_continuous(name="LE (W m-2 s-1)") + 
  scale_colour_manual(name='Legend', values=c("Predicted"="lightskyblue1")) +
  scale_fill_manual(name=element_blank(), values=c("95% confidence interval" = "blue3")) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 45), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 


### Forecast Horizon
source("/fs/data3/kzarada/NEFI/Willow_Creek/animated_WCr_graphs.R")

sub.nee <- subset(nee.data, nee.data$Time == paste0(Sys.Date() - lubridate::days(1), "_12"))

x.breaks = sub.nee$start_date
labels = rev(seq(from = 1, to = length(x.breaks), by = 1))


horiz <- ggplot(sub.nee, aes(group = 1)) + 
  geom_ribbon(aes(x = start_date, ymin = Lower, ymax = Upper, fill="95% Confidence Interval"), alpha = 0.4) + 
  geom_line(aes(x = start_date, y = Predicted, color = "Predicted")) + 
  geom_line(aes(x = start_date, y = NEE, color = "Observed Data"), size = 1) +  
  ggtitle(paste0("Forecast Horizon for ", Sys.Date() - lubridate::days(1))) +
  scale_color_manual(name = "Legend", labels = c("Observed Data", "Predicted"), values=c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")) +
  scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
  scale_y_continuous(name="NEE (kg C m-2 s-1)") + 
  scale_x_discrete(name = "Days from Observed Date", breaks = x.breaks, labels = labels) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.title = element_blank(), legend.text = element_text(size = 10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 12)) 




