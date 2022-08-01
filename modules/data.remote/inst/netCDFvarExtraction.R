library(tidyverse)
library(plotly)
library(lubridate)
library(tidyr)
library(dplyr)
library(ncdf4)

#' @title extractPredictionVar
#' @description extracts a single ensemble run of a specified variable during the 
#' given year range from a specified file location
#'
#' @param betySiteId bety site id for the desired location (String)
#' @param yearStart starting year YYYY as (date) or (numeric)
#' @param yearEnd ending year YYYY as (date) or (numeric)
#' @param ENS ensemble member front padded with 0's to five values (String)
#' @param predictVar the prediction variable to be extracted from nc file as (String)
#' @param ensemble_dir the directory for which ensembles are stored as (String)
#'
#' @return A list containing soil moisture and time values
#'
#' @examples 
#' newEns <- extractPredictionVar(betySiteId = '1000004945',
#' yearStart = as.numeric(format(as.Date('2016-01-02'), '%Y')), 
#' yearEnd = as.numeric(format(as.Date('2016-07-16'), '%Y')), 
#' ENS = str_pad('6', 5, pad = '0'), 
#' predictVar = 'SoilMoist', 
#' ensemble_dir = '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/')
#' 
#' This extracts soil moisture values and time values from the 6th ensemble member netCDF file 
#' in a specified file directory for the Harvard Forest ('1000004945') site during the year 2016
#' 
extractPredictionVar <- function(betySiteId, yearStart, yearEnd, ENS, predictVar, ensemble_dir){
  
  for (x in 0:(yearEnd - yearStart)) {
    nc.file <- paste0(ensemble_dir, 'ENS-', ENS, '-', betySiteId, '/', yearStart + x,'.nc')
    nc <- nc_open(nc.file)
    if (x == 0) {
      soilMoist <- ncvar_get(nc, predictVar)
      time <- PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units)
    } else {
      soilMoist <- append(soilMoist, ncvar_get(nc, predictVar))
      time <- append(time, PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units))
    }
  }
  return(list(time, soilMoist))
}

#' @title extractVariableENS
#' @description utilizes extractPredictionVar function to extract an entire ensemble of a 
#' specified variable over the course of a given date range.
#'
#' @param betySiteId bety site id for the desired location as (String)
#' @param ENSnum number of members in the ensemble as (numeric)
#' @param predictVar prediction variable to be extract from nc file as (String)
#' @param start start date of values to extract YYYY-MM-DD as (Date)
#' @param end end date of values to extract YYYY-MM-DD as (Date)
#' @param ensemble_dir directory of ensemble runs for extraction
#'
#' @return a data frame consisting of the soil mositure as well as their date-time values 
#' during the specified date range
#' 
#'
#' @examples 
#' moistureFrame <- extractVariableENS(betySiteId = '1000004945', 
#' ENSnum = 25, 
#' predictVar = 'SoilMoist', 
#' start = as.Date('2016-01-02'),
#' end = as.Date('2016-07-01'), 
#' ensemble_dir = '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/')
#' 
#' Extracts soil moisture values for all 25 ensembles members from the Harvard 
#' Forest (1000004945) site between the 2016-01-02' - '2016-07-01' date range from
#' files at the specified 
#' 
extractVariableENS <- function(betySiteId, ENSnum=25, predictVar, start, end, ensemble_dir){
  for (x in 1:ENSnum) {
    newEns <- extractPredictionVar(betySiteId = betySiteId, yearStart = as.numeric(format(start, '%Y')), 
                                   yearEnd = as.numeric(format(end, '%Y')), ENS = str_pad(x, 5, pad = '0'), 
                                   predictVar = predictVar, ensemble_dir = ensemble_dir)
    
    if (x == 1){
      var_time <- newEns
    } else {
      var_time[[1]] <- append(var_time[[1]], newEns[[1]])
      var_time[[2]] <- append(var_time[[2]], newEns[[2]]) 
    }
  }
  
  # Eliminate extraneous date range values #
  vtr <- as.data.frame(var_time, col.names = c('time', predictVar))
  vtr <- vtr[vtr$time >= as.Date(start) & vtr$time <= as.Date(end), ]
  
  return(vtr)
}


