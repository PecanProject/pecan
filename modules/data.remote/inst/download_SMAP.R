#load necessities 
library(tidyverse)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)
library(purrr)
library(lubridate)
library(tidyr)
library(dplyr)
library(ncdf4)

download_SMAP <- function(){
  
  #set start and end dates
  ## 2015-04-01 is first available smap data
  start <- '2015-04-01'
  end <- '2022-04-01'# as.character(Sys.Date())
  
  # 2016-01-02
  # 2016-07-16
  
  ##############################
  ########    HARVARD   ########
  ##############################
  
  ######## Download SMAP data ########
  geoJSON_outdir = "/projectnb/dietzelab/jbowers1/" 
  smap_outdir = "/projectnb/dietzelab/jbowers1/"
  
  site_info <- list(
    site_id = 1126,
    site_name = "Harvard_Forest",
    lat = 42.531453,
    lon = -72.188896,
    time_zone = "UTC")
  
  source('~/pecan/modules/data.remote/inst/download_SMAP_from_gee.R')
  
  harv.smap_sm <- download_SMAP_gee2pecan(start, end, site_info, geoJSON_outdir, smap_outdir)
  return(harv.smap_sm)
}

