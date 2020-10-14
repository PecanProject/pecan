#load necessities 
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)
library(purrr)
library(lubridate)
library(tidyr)
library(dplyr)
library(ncdf4)

par(mfrow = c(1,2))

#set start and end dates
start = "2019-04-01"
end = as.character(Sys.Date())



                      ##############################
                      ######## WILLOW CREEK ########
                      ##############################

    ######## Download Ameriflux field data########

#download and get daily average
source("/fs/data3/jbateman/pecan/modules/assim.sequential/inst/NEFI/US_WCr/download_soilmoist_WCr.R")
sm_wcr = download_soilmoist_WCr(start, end) %>%
  dplyr::mutate(Day = lubridate::day(Time), Month = lubridate::month(Time), Year = lubridate::year(Time)) %>%
  group_by(Year, Month, Day)
sm_wcr$Date = as.Date(with(sm_wcr, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
  
sm_wcr.Dayavg = sm_wcr %>% 
  summarise(DayAvgsm1 = mean(avgsoil1)) %>%
  ungroup()
sm_wcr.Dayavg2= sm_wcr %>% 
  summarise(DayAvgsm2 = mean(avgsoil2)) %>%
  ungroup()
sm_wcr.Dayavg$DayAvgsm2 =sm_wcr.Dayavg2$DayAvgsm2 
sm_wcr.Dayavg$Date = as.Date(with(sm_wcr.Dayavg, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_wcr.Dayavg = sm_wcr.Dayavg %>% dplyr::select(Date, DayAvgsm1, DayAvgsm2)



    ######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst/" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"
site_info <- list(
  site_id = 676,
  site_name = "Willow Creek",
  lat = 45.805925,
  lon = -90.07961,
  time_zone = "UTC")

wcr.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
wcr.d = ggplot() + 
  geom_line(data = na.omit(wcr.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) + 
  geom_point(data = na.omit(wcr.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_wcr.Dayavg, aes(x=Date, y=DayAvgsm1, color = "red"), linetype = "dashed") +
  geom_line(data = sm_wcr.Dayavg, aes(x=Date, y=DayAvgsm2, color = "purple"), linetype = "dashed") +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: Willow Creek") +
  labs(x = "Date",
       y = "Soil Moisture (%)" ,
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "purple"),
    labels = c("SMAP", "Old Field", "New Field"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
wcr.half = ggplot() + 
  geom_line(data = sm_wcr, aes(x=Date, y=avgsoil1, color="red"), linetype ="solid") +
  geom_line(data = sm_wcr, aes(x=Date, y=avgsoil2, color="purple"), linetype ="solid") +
  geom_line(data = na.omit(wcr.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(wcr.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: Willow Creek") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "purple"),
    labels = c("SMAP", "Old Field", "New Field"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())

require(gridExtra)
grid.arrange(wcr.d, wcr.half)


                  ##############################
                  ########    SYLVANIA  ########
                  ##############################

######## Download Ameriflux field data########

#download and get daily average
source("/fs/data3/jbateman/pecan/modules/assim.sequential/inst/NEFI/US_Syv/download_soilmoist_Syv.R")
sm_syv = download_soilmoist_Syv(start, end) %>%
  mutate(Day = day(Time), Month = month(Time), Year = year(Time)) %>%
  group_by(Year, Month, Day)
sm_syv$Date = as.Date(with(sm_syv, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")

sm_syv.Dayavg = sm_syv %>%
  summarise(DayAvgsm = mean(avgsoil)) %>%
  ungroup()
sm_syv.Dayavg$Date = as.Date(with(sm_syv.Dayavg, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_syv.Dayavg = sm_syv.Dayavg %>% dplyr::select(Date, DayAvgsm)



######## Download SMAP ssm data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 622,
  site_name = "Sylvania",
  lat = 46.242017,
  lon = -89.347567,
  time_zone = "UTC")
syv.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
syv.d = ggplot() + 
  geom_line(data = na.omit(syv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) + 
  geom_point(data = na.omit(syv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_syv.Dayavg, aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dashed") +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: SYLVANIA") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") + 
  scale_color_identity(
      breaks = c("steel blue","red"),
      labels = c("SMAP", "Field"),
      guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
syv.half = ggplot() + 
  geom_line(data = sm_syv, aes(x=Date, y=avgsoil, color="red"), linetype ="solid") +
  geom_line(data = na.omit(syv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(syv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: SYLVANIA") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())

grid.arrange(syv.d, syv.half)




                  ##############################
                  ########     WLEF     ########
                  ##############################

######## Download Ameriflux field data########

#download and get daily average
source("/fs/data3/jbateman/pecan/modules/assim.sequential/inst/NEFI/US_WLEF/download_soilmoist_WLEF.R")
sm_wlef = download_soilmoist_WLEF(start, end) %>%
  mutate(Day = day(Time), Month = month(Time), Year = year(Time)) %>%
  group_by(Year, Month, Day)
sm_wlef$Date = as.Date(with(sm_wlef, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")

sm_wlef.Dayavg = sm_wlef %>%
  summarise(DayAvgsm = mean(avgsoil)) %>%
  ungroup()
sm_wlef.Dayavg$Date = as.Date(with(sm_wlef.Dayavg, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_wlef.Dayavg = sm_wlef.Dayavg %>% dplyr::select(Date, DayAvgsm)



######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 678,
  site_name = "WLEF",
  lat = 45.9408,
  lon = -90.27,
  time_zone = "UTC")
wlef.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
wlef.d = ggplot() + 
  geom_line(data = na.omit(wlef.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(wlef.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_wlef.Dayavg, aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dashed") +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: WLEF") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
wlef.half = ggplot() + 
  geom_line(data = sm_wlef, aes(x=Date, y=avgsoil, color="red"), linetype ="solid") +
  geom_line(data = na.omit(wlef.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(wlef.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: WLEF") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())

grid.arrange(wlef.d, wlef.half)


                  ##############################
                  ########    HARVARD   ########
                  ##############################

######## Download Ameriflux field data########

#download and get daily average
source("/fs/data3/jbateman/pecan/modules/assim.sequential/inst/NEFI/US_Harvard/download_soilmoist_harvard.R")
sm_harv = download_soilmoist_Harvard(start, end) %>%
  mutate(Day = day(Time), Month = month(Time), Year = year(Time)) %>%
  group_by(Year, Month, Day)
sm_harv$Date = as.Date(with(sm_harv, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_harv$SWC15 = replace(sm_harv$SWC15, sm_harv$SWC15 == -9999, NA)

sm_harv.Dayavg = sm_harv %>%
  summarise(DayAvgsm = mean(SWC15)) %>%
  ungroup()
sm_harv.Dayavg$Date = as.Date(with(sm_harv.Dayavg, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_harv.Dayavg = sm_harv.Dayavg %>% dplyr::select(Date, DayAvgsm)



######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 1126,
  site_name = "Harvard Forest",
  lat = 42.531453,
  lon = -72.188896,
  time_zone = "UTC")
harv.smap_sm = download_SMAP_gee2pecan("2019-11-06", end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
harv.d = ggplot() + 
  geom_line(data = na.omit(harv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(harv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_harv.Dayavg, aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dashed") +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: Harvard") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
harv.half = ggplot() + 
  geom_line(data = na.omit(sm_harv), aes(x=Date, y=SWC15, color="red"), linetype ="solid") +
  geom_line(data = na.omit(harv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(harv.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: Harvard") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())


grid.arrange(harv.d, harv.half)

        ##############################
        ########     BART     ########
        ##############################

######## NEON data ########

#download and get daily average
BART_ssm = split(BART, list(BART$verticalPosition, BART$horizontalPosition, BART$VSWCFinalQF))
BART_ssm = split(BART, BART$VSWCFinalQF)
sm_bart = BART_ssm$'0' %>%
  na.omit() %>%
  dplyr::select(startDateTime, VSWCMean, horizontalPosition, verticalPosition) %>%
  mutate(Day = day(startDateTime), Month = month(startDateTime), Year = year(startDateTime)) %>%
  group_by(Year, Month, Day)
sm_bart$Date = as.Date(with(sm_bart, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_bart$VSWCMean = sm_bart$VSWCMean * 100
sm_bart = split(sm_bart, list(sm_bart$verticalPosition, sm_bart$horizontalPosition))

sm_bart.Dayavg = vector(mode = "list", length = 40) 
names(sm_bart.Dayavg) = names(sm_bart)
for (i in 1:length(sm_bart)){
  sm_bart.Dayavg[[i]] = dplyr::select(sm_bart[[i]], Date, VSWCMean) %>% 
    summarise(DayAvgsm = mean(VSWCMean)) %>%
    ungroup()
  sm_bart.Dayavg[[i]]$Date = as.Date(with(sm_bart.Dayavg[[i]], paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
}


######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 796,
  site_name = "Bartlett",
  lat = 44.06464,
  lon = -71.288077,
  time_zone = "UTC")
bart.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
bart.d = ggplot() + 
  geom_line(data = na.omit(bart.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(bart.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"),linetype = "dotted", size=.5) +
  geom_point(data = sm_bart.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), size=1) +
  geom_line(data = sm_bart.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), linetype = "dotted", size=.5) +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: Bartlett") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "green", "purple", "orange", "yellow"),
    labels = c("SMAP", "Field 1 (-6cm)", "Field 2 (-6cm)", "Field 3 (-6cm)", "Field 4 (-6cm)", "Field 5 (-6cm)"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
bart.half = ggplot() + 
  geom_line(data = sm_bart$'502.1', aes(x=Date, y=VSWCMean, color = "red"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart$'502.1', aes(x=Date, y=VSWCMean, color = "red"), size=1) +
  geom_line(data = sm_bart$'502.1', aes(x=Date, y=VSWCMean, color = "red"),linetype = "dotted", size=.5) +
  geom_point(data = sm_bart$'502.2', aes(x=Date, y=VSWCMean, color = "green"), size=1) +
  geom_line(data = sm_bart$'502.2', aes(x=Date, y=VSWCMean, color = "green"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart$'502.3', aes(x=Date, y=VSWCMean, color = "purple"), size=1) +
  geom_line(data = sm_bart$'502.3', aes(x=Date, y=VSWCMean, color = "purple"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart$'502.4', aes(x=Date, y=VSWCMean, color = "orange"), size=1) +
  geom_line(data = sm_bart$'502.4', aes(x=Date, y=VSWCMean, color = "orange"), linetype = "dotted", size=.5) +
  geom_point(data = sm_bart$'502.5', aes(x=Date, y=VSWCMean, color = "yellow"), size=1) +
  geom_line(data = sm_bart$'502.5', aes(x=Date, y=VSWCMean, color = "yellow"), linetype = "dotted", size=.5) +
  ylim(0,60) +
  geom_line(data = na.omit(bart.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(bart.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: Bartlett") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "green", "purple", "orange", "yellow"),
    labels = c("SMAP", "Field 1 (-6cm)", "Field 2 (-6cm)", "Field 3 (-6cm)", "Field 4 (-6cm)", "Field 5 (-6cm)"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())

#require(gridExtra)
#grid.arrange(bart.d, bart.half)
plot(bart.half)



##############################
########     SRER     ########
##############################

######## NEON data ########

#download and get daily average
SRER_ssm = split(SRER, list(SRER$verticalPosition, SRER$horizontalPosition, SRER$VSWCFinalQF))
SRER_ssm = split(SRER, SRER$VSWCFinalQF)
sm_srer = SRER_ssm$'0' %>%
  na.omit() %>%
  dplyr::select(startDateTime, VSWCMean, horizontalPosition, verticalPosition) %>%
  mutate(Day = day(startDateTime), Month = month(startDateTime), Year = year(startDateTime)) %>%
  group_by(Year, Month, Day)
sm_srer$Date = as.Date(with(sm_srer, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_srer$VSWCMean = sm_srer$VSWCMean * 100
sm_srer = split(sm_srer, list(sm_srer$verticalPosition, sm_srer$horizontalPosition))

sm_srer.Dayavg = vector(mode = "list", length = 40) 
names(sm_srer.Dayavg) = names(sm_srer)
for (i in 1:length(sm_srer)){
  sm_srer.Dayavg[[i]] = dplyr::select(sm_srer[[i]], Date, VSWCMean) %>% 
    summarise(DayAvgsm = mean(VSWCMean)) %>%
    ungroup()
  sm_srer.Dayavg[[i]]$Date = as.Date(with(sm_srer.Dayavg[[i]], paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
}


######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 1000004876,
  site_name = "Santa Rita",
  lat = 31.91068,
  lon = -110.83549,
  time_zone = "UTC")
srer.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
srer.d = ggplot() + 
  geom_line(data = na.omit(srer.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(srer.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"),linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), linetype = "dotted", size=.5) +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: Santa Rita") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "green", "purple", "orange", "yellow"),
    labels = c("SMAP", "Field 1 (-6cm)", "Field 2 (-6cm)", "Field 3 (-6cm)", "Field 4 (-6cm)", "Field 5 (-6cm)"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
srer.half = ggplot() + 
  geom_line(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"),linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), linetype = "dotted", size=.5) +
  geom_point(data = sm_srer.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), size=1) +
  geom_line(data = sm_srer.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), linetype = "dotted", size=.5) +
  geom_line(data = na.omit(srer.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(srer.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ylim(0,60) +
  ggtitle("SMAP vs 1/2 hr Field Data: Santa Rita") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "green", "purple", "orange", "yellow"),
    labels = c("SMAP", "Field 1 (-6cm)", "Field 2 (-6cm)", "Field 3 (-6cm)", "Field 4 (-6cm)", "Field 5 (-6cm)"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())


grid.arrange(srer.d, srer.half)
plot(srer.half)


##############################
########     KONA     ########
##############################

######## NEON data ########

#download and get daily average
KONA_ssm = split(KONA, list(KONA$verticalPosition, KONA$horizontalPosition, KONA$VSWCFinalQF))
KONA_ssm = split(KONA, KONA$VSWCFinalQF)
sm_kona = KONA_ssm$'0' %>%
  na.omit() %>%
  dplyr::select(startDateTime, VSWCMean, horizontalPosition, verticalPosition) %>%
  mutate(Day = day(startDateTime), Month = month(startDateTime), Year = year(startDateTime)) %>%
  group_by(Year, Month, Day)
sm_kona$Date = as.Date(with(sm_kona, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
sm_kona$VSWCMean = sm_kona$VSWCMean * 100
sm_kona = split(sm_kona, list(sm_kona$verticalPosition, sm_kona$horizontalPosition))

sm_kona.Dayavg = vector(mode = "list", length = 40) 
names(sm_kona.Dayavg) = names(sm_kona)
for (i in 1:length(sm_kona)){
  sm_kona.Dayavg[[i]] = dplyr::select(sm_kona[[i]], Date, VSWCMean) %>% 
    summarise(DayAvgsm = mean(VSWCMean)) %>%
    ungroup()
  sm_kona.Dayavg[[i]]$Date = as.Date(with(sm_kona.Dayavg[[i]], paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
}



######## Download SMAP data ########
geoJSON_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst" 
smap_outdir = "/projectnb/dietzelab/jbateman/pecan/modules/data.remote/inst"

site_info <- list(
  site_id = 1000004925,
  site_name = "KONA",
  lat = 39.11044,
  lon = -96.61295,
  time_zone = "UTC")
kona.smap_sm = download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)

##### plot time series

# Daily sm average
kona.d = ggplot() + 
  geom_line(data = na.omit(kona.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(kona.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  geom_line(data = sm_kona.Dayavg, aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dashed") +
  ylim(0,60) +
  ggtitle("SMAP vs Daily Field Data: Konza Prairie") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red"),
    labels = c("SMAP", "Field"),
    guide = "legend") +
  theme(
    legend.position = "none",
    legend.title = element_blank())

# 1/2 hr field data vs daily smap (6am)
kona.half = ggplot() + 
  geom_line(data = sm_kona.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), linetype = "dotted", size=.5) +
  geom_point(data = sm_kona.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"), size=1) +
  geom_line(data = sm_kona.Dayavg$'502.1', aes(x=Date, y=DayAvgsm, color = "red"),linetype = "dotted", size=.5) +
  geom_point(data = sm_kona.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), size=1) +
  geom_line(data = sm_kona.Dayavg$'502.2', aes(x=Date, y=DayAvgsm, color = "green"), linetype = "dotted", size=.5) +
  geom_point(data = sm_kona.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), size=1) +
  geom_line(data = sm_kona.Dayavg$'502.3', aes(x=Date, y=DayAvgsm, color = "purple"), linetype = "dotted", size=.5) +
  geom_point(data = sm_kona.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), size=1) +
  geom_line(data = sm_kona.Dayavg$'502.4', aes(x=Date, y=DayAvgsm, color = "orange"), linetype = "dotted", size=.5) +
  geom_point(data = sm_kona.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), size=1) +
  geom_line(data = sm_kona.Dayavg$'502.5', aes(x=Date, y=DayAvgsm, color = "yellow"), linetype = "dotted", size=.5) +
  geom_line(data = na.omit(kona.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue")) +
  geom_point(data = na.omit(kona.smap_sm), aes(x=Date, y=ssm.vol, color="steel blue"), size=1) +
  ggtitle("SMAP vs 1/2 hr Field Data: Konza Prairie") +
  labs(x = "Date",
       y = "Soil Moisture (%)",
       color = "Legend\n") +
  scale_color_identity(
    breaks = c("steel blue","red", "green", "purple", "orange", "yellow"),
    labels = c("SMAP", "Field 1 (-6cm)", "Field 2 (-6cm)", "Field 3 (-6cm)", "Field 4 (-6cm)", "Field 5 (-6cm)"),
    guide = "legend") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())


grid.arrange(kona.d, kona.half)
plot(kona.half)

