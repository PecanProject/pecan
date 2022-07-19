##'@name download_SMAP
##'@description: This script downloads SMAP data for all CONUS NEON sites
##'
##'Inputs:
##'1. Data range to produce SMAP data
##'2. File location containing XML file of all 39 CONUS NEON sites and their information 
##'
##'
##'@author Joshua Bowers

## read in CONUS sites from xml file ##
all_sites <- PEcAn.settings::read.settings('~/pecan/modules/data.remote/inst/conus_sites.xml')

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

#' @title download_SMAP
#'
#' @param start start date of analysis as (Date) or (String) 
#' @param end end date of analysis as (Date) or (String)
#' @param geoJSON_outdir file directory to store output site geoJSON files
#' @param smap_outdir file directory to store output SMAP netCDF file
#'
#' @return A list containing soil moisture values in a data frames for each site in the CONUS
#'
#'
#' @examples
#' all_smap_data <- download_SMAP('2016-01-02', '2016-07-16', 
#' '/projectnb/dietzelab/jbowers1/geoFiles/', '/projectnb/dietzelab/jbowers1/smap_ncFiles/')
#' 
download_SMAP <- function(start, end, geoJSON_outdir, smap_outdir){
  ## 2015-04-01 is first available smap data
  
  ######## Download CONUS SMAP data########
  
  # Initialize empty list
  output_smap <- list()
  
  source('~/pecan/modules/data.remote/inst/download_SMAP_from_gee.R')
  
  # Inupt each site's info as a new list
  for(index in 1:length(all_sites)){ # There are 39 sites in the CONUS
    
    if (index != 4 & index != 33 & index != 37) { # sites 4, 33, and 37 don't seem to be accessible smap coords
      output_smap[[length(output_smap) + 1]] <-  
        download_SMAP_from_gee(start, end, all_sites[[index]]$run$site, geoJSON_outdir, smap_outdir)
      names(output_smap)[length(output_smap)] <- all_sites[[index]]$run$site$id
    }
    
  }

  return(output_smap)
}

