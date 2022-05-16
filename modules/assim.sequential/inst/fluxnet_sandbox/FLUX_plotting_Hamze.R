library(readr)
library(tidyverse)
library(ncdf4)
library(scales)
library(lubridate)
setwd("/projectnb/dietzelab/hamzed/SDA/ProductionRun/500Sites/Outs/NC")
site_level_w <- readRDS("/projectnb/dietzelab/hamzed/SDA/ProductionRun/500Sites/Weights/site_level_w.RDS")
# The main difference between this and other flux comparison files is that this 
# would use the nc files not the SQL database
#-----------------------------------------------------------------------------------
Var <- 'NEE'
ncin <- nc_open(paste0(Var, '.nc'))
rw <- ncvar_get(ncin, 'weights_rrel')
sites <- ncvar_get(ncin, 'site') %>% as.character()
Year <- ncvar_get(ncin, 'Year')
time <-  ncvar_get(ncin, 'time')
tunits <- ncatt_get(ncin, "time", "units")
var.nc <- ncvar_get(ncin, Var)
#convert time
posix<- seq.POSIXt(as.POSIXct("1986-01-01 00:00:00", tz="EDT"),
                   as.POSIXct("2018-12-30 21:00:00", tz="EDT"),
                   length.out = length(time))
#-- Reformating the nc from matrix to data.frame
flux.data <-seq_len(length(sites)) %>%
  map_dfr(function(site){
    seq_len(20) %>% # foreach ensemble
      map_dfr(~{
        var.nc[, site, .x] %>%
          as.data.frame() %>%
          mutate(Site=sites[site],
                 ensemble=.x
          ) %>%
          `colnames<-`(c(Var, 'Site', 'ensemble')) %>%
          mutate(posix=posix)
      })
  })
#---------------------------------------------------------------------------------
new.p <- flux.data %>%
  mutate(Year=lubridate::year(posix)) %>%
  left_join(site_level_w , 
            by=c('Site'='Site', 
                 'ensemble'='ens',
                 'Year'='Year'))
flux.data.df <- new.p %>%
  mutate(Week=week(posix)) %>%
  group_by(Site, Year, Week) %>%
  summarise(
    Forecast = mean(NEE),
    ForecastVar=var(NEE),
    Analysis= Hmisc::wtd.mean(NEE, weights = Relative_weight),
    AnalysisVar= Hmisc::wtd.var(NEE, weights = Relative_weight, normwt=TRUE, method='unbiased'),
  ) %>%
  mutate(Date=as.Date(
    paste(Year, Week, 1), 
    format = "%Y %U %u"
  )) %>%
  mutate(
    ForecastSD=sqrt(ForecastVar),
    AnalysisSD=sqrt(AnalysisVar)
  ) %>%
  mutate(
    FLL =Forecast + ForecastSD, # Change this to what level of uncertainty you wanna plot
    FUL =Forecast - ForecastSD,
    ALL =Analysis + AnalysisSD,
    AUL =Analysis - AnalysisSD
  )
flux.data.df %>%
  split(.$Site) %>%
  {.[1:5]} %>% # plot just the first site
  map(~ {
    .x %>%
      mutate(Date=as.POSIXct(Date))%>%
      ggplot(aes(x=Date))+
      geom_pointrange(aes(y= Forecast, ymin=FLL, ymax=FUL, color="Forecast"), alpha=0.75)+
      geom_pointrange(aes(y= Analysis, ymin=ALL, ymax=AUL, color="Analysis"), alpha=0.75)+
      labs(title = .x$Site, y=Var)+
      scale_color_manual(values=c("#ff7f00",'#4daf4a','#984ea3'), name="")+
      theme_minimal(base_size = 15)+
      scale_x_datetime(date_breaks = "2 year")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(legend.position = "top")
    ggsave(file=paste0("timeseries_",.x$Site %>% unique(),".png"),
           width=14, height=7)
    #browser()
  })