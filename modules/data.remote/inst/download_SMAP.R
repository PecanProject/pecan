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

download_SMAP <- function(start, end){
  
  #set start and end dates
  ## 2015-04-01 is first available smap data
  start <- '2016-01-02'
  end <- '2016-07-16'# as.character(Sys.Date()) for current date
  
  geoJSON_outdir = "/projectnb/dietzelab/jbowers1/geoFiles/" 
  smap_outdir = "/projectnb/dietzelab/jbowers1/smap_ncFiles/"
  
  
  ######## Download CONUS SMAP data########

  # read in CONUS sites from xml file
  all_sites <- PEcAn.settings::read.settings("~/pecan.xml")
  
  # Initialize empty list
  output_smap <- list()
  
  source('~/pecan/modules/data.remote/inst/download_SMAP_from_gee.R')
  
  # Inupt each site's info as a new list
  for(index in 1:length(all_sites)){ # There are 39 sites in the CONUS
    
    if (index != 4 & index != 33 & index != 37) { # sites 4, 33, and 37 don't seem to be available smap coords
      output_smap[[length(output_smap) + 1]] <-  
        download_SMAP_from_gee(start, end, all_sites[[index]]$run$site, geoJSON_outdir, smap_outdir)
      names(output_smap)[length(output_smap)] <- all_sites[[index]]$run$site$id
    }
    
  }

  return(output_smap)
}

