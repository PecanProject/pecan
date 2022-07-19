##'@name SMAPvsModel_Comparison
##'@description: This script collects ensemble model data from a specified file location, 
##'analyzes that data to produce two exploratory graphs, and finally compares the
##'model data to actual soil moisture data collected from NASA's SMAP satellite to
##'produce an additional 5 graphs and a set of comparative statistics including: pbias, 
##'RMSE, CRPS, Correlation, and r-squared values.
##'
##'This script is broken into 4 functions: extractSoilMoist, extractEnsambles, 
##'runAnalysis, and main. The functions can be used separately, however, it is 
##'meant for the main method to be run with the desired CONUS NEON sites id's in vector form.
##'
##'WARNING! D17 NEON sites including Lower Teakettle, Soaproot Saddle, and San Joaquin 
##'Experimental Range are not available available in SMAP and thus should not be
##'run with this script.
##'
##'Inputs:
##'1. Data range to be analyized as (String)
##'2. File location for ensemble netCDF files 
##'3. File location containing XML file of all 39 CONUS NEON sites and their information 
##'4. File location to place output analytial data
##'5. File location for output of NEON site geoJson files
##'6. File location for output of SMAP netCDF files
##'
##'@author Joshua Bowers
##'
##'@examples
##'
##'# Run all CONUS NEON sites at once
##'all_sites <- PEcAn.settings::read.settings("~/pecan.xml")
##'sites_vector <- c()
##'for (index in 1:length(all_sites)) {
##'sites_vector <- append(sites_vector, all_sites[[index]]$run$site$id) 
##'}
##'sites_vector <- sites_vector[c(-4, -33, -37)] # Removes D17 sites that are not collected by SMAP
##'main(sites_vector, compare = TRUE, pdf_output = '/projectnb/dietzelab/jbowers1/graphic_output/')\
##'
##'# Or run individual outputs printed to your screen
##'var <- runAnalysis('1000004945') # harvard
##'var1 <- runAnalysis('1000004876') # santa rita
##'var2 <- runAnalysis('1000004927') # konza
##'var3 <- runAnalysis('1000004916') # ordway
##'
##'

# load necessary packages
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
library(stringr)
library(hydroGOF)
library(Metrics)
library(verification)


#################### INPUTS #################### 
## Set Date Range ##
start <- '2016-01-02'
end <- '2016-07-16'

## Directory of Ensembles Files ##
ensamble_dir <- '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/'

## CONUS NEON Sites XML File Location ##
input_dir <- '~/pecan/modules/data.remote/inst/conus_sites.xml'

## File Directory to Place Output PDFs In ##
output_dir <- '/projectnb/dietzelab/jbowers1/graphic_output/'

## Output Directories for geoJson Files ##
geoJSON_outdir <- '/projectnb/dietzelab/jbowers1/geoFiles/'

## Output Directories for SMAP netCDF Files ##
smap_outdir <- '/projectnb/dietzelab/jbowers1/smap_ncFiles/'

## Download SMAP data ##
source('~/pecan/modules/data.remote/inst/download_SMAP.R')
all_smap_data <- download_SMAP(start, end, geoJSON_outdir, smap_outdir)
####################^ INPUTS ^####################
start <- as.Date(start)
end <- as.Date(end)


#' @title extractSoilMoist
#' @description extracts a single ensamble member during the given year range from a specified file location
#'
#' @param betySiteId bety site id for the desired location (String)
#' @param yearStart starting year YYYY as (date) or (numeric)
#' @param yearEnd ending year YYYY as (date) or (numeric)
#' @param ENS ensamble member front padded with 0's to five values (String)
#'
#' @return A list containing soil moisture and time values
#'
#' @examples 
#' extractSoilMoist(betySiteId = '1000004945', yearStart = 2016, yearEnd = 2017, ENS = '25')
#' 
#' This extracts soil moisture values and time values from the 25th ensamble member netCDF file 
#' in a specified file directory for the Harvard Forest ('1000004945') site during the years 
#' 2016 and 2017.
#' 
extractSoilMoist <- function(betySiteId, yearStart, yearEnd, ENS){

  for (x in 0:(yearEnd - yearStart)) {
    nc.file <- paste0(ensamble_dir, 'ENS-', ENS, '-', betySiteId, '/', yearStart + x,'.nc')
    nc <- nc_open(nc.file)
    if (x == 0) {
      soilMoist <- ncvar_get(nc,'SoilMoist')
      time <- PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units)
    } else {
      soilMoist <- append(soilMoist, ncvar_get(nc,'SoilMoist'))
      time <- append(time, PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units))
    }
  }
  return(list(time, soilMoist))
}


#' @title extractEnsamlbes
#' @description utilizes extractSoilMoist function to extract an entire ensamble of a specified
#' over the course of a specified date range.
#'
#' @param betySiteId bety site id for the desired location as (String)
#' @param ENSnum number of members in the ensamble as (numeric)
#'
#' @return a data frame consisting of the soil mositure as well as their date-time values 
#' during the specified date range
#' 
#'
#' @examples 
#' extractEnsambles(betySiteId = '1000004945', start = as.Date('2016-01-02'), end = as.Date('2016-07-01'))
#' 
#' Extracts all 25 ensample members (default) from the Harvard Forest (1000004945) site between the
#' '2016-01-02' - '2016-07-01' date range.
#' 
extractEnsambles <- function(betySiteId, ENSnum=25){
  for (x in 1:ENSnum) {
    newEns <- extractSoilMoist(betySiteId = betySiteId, yearStart = as.numeric(format(start, '%Y')), 
                               yearEnd = as.numeric(format(end, '%Y')), ENS = str_pad(x, 5, pad = '0'))
    
    if (x == 1){
      moisture_time <- newEns
    } else {
      moisture_time[[1]] <- append(moisture_time[[1]], newEns[[1]])
      moisture_time[[2]] <- append(moisture_time[[2]], newEns[[2]]) 
    }
  }
  
  # Eliminate extraneous date range values #
  vtr <- as.data.frame(moisture_time, col.names = c('time', 'moisture'))
  vtr <- vtr[vtr$time >= as.Date(start) & vtr$time <= as.Date(end), ]
  
  return(vtr)
}


#' @title runAnalysis
#' 
#' @description Runs multiple analyses on the specified site during a given date range
#' and has the functionality to compare those values to actual soil moisture values
#' collected from SMAP
#'
#' @param betySiteId bety site id for the desired location as (String)
#' @param compare (boolean) to turn on/off comparison to actual smap data
#'
#' @return returns a list consisting of all of the analysis plots and statistics
#'
#' @examples 
#' var <- runAnalysis('1000004945', '2016-01-02', '2016-07-16', TRUE)
#' This runs an analysis on the Harvard Forest (1000004945) site with results between
#' the date range of '2016-01-02' and '2016-07-16'
#' 
runAnalysis <- function(betySiteId, compare = TRUE){
  
  moistureFrame <- extractEnsambles(betySiteId)
  
  ## Grouping data by Date ##
  grouped_ENS <- moistureFrame %>% 
    group_by(time) %>% 
    mutate(max = max(moisture)) %>%
    mutate(min = min(moisture))
  
  ## Calculating Mean Per Ensemble ##
  mean_per_ENS <- summarize(grouped_ENS, mean = mean(moisture))
  
  ## Calculating Standard Deviation ##
  sd_per_ENS <- summarize(grouped_ENS, sd = sd(moisture))
  
  ## Calculating .025 and .975 Quantiles ##
  quantiles_lower <- summarize(grouped_ENS, q_lower = quantile(moisture, .025))
  quantiles_upper <- summarize(grouped_ENS, q_upper = quantile(moisture, .975))
  
  ## Hex Data Distribution with Mean-line Highlighted ##
  p1 <- ggplot(moistureFrame, aes(x = time, y = moisture)) + geom_hex() + theme_bw() + 
    guides(fill=guide_legend(title = 'Frequency of Model Data')) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle('Hex Distribution of Model Data') +
    geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) + 
    labs(colour = '') + ylab('Surface Soil Moisture (ssm)') + xlab('Date of Prediction') +
    labs(colour = 'Legend')
  print(p1)
  
  ## Min/Max Values with 95% Quantile Range ## Still has messed up color legend
  p2 <- ggplot(grouped_ENS, aes(x = time, y = moisture)) + theme_bw() + 
    ggtitle('Daily Min and Max Values with 95% Quantile Range in Black') +
    theme(legend.position = c(0.87, 0.75),
          legend.background = element_rect(fill = "white", color = "black"),
          plot.title = element_text(hjust = 0.5)) +
    geom_point(data = grouped_ENS, aes(x = time, y = max, color = 'Daily Max')) +
    geom_point(data = grouped_ENS, aes(x = time, y = min, color = 'Daily Min')) +
    geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean')) +
    geom_line(data = quantiles_lower, aes(x = time, y = q_lower)) +
    geom_line(data = quantiles_upper, aes(x = time, y = q_upper)) + 
    xlab('Date of Prediction') + ylab('Surface Soil Moisture (ssm)') +
    labs(colour = 'Legend')
  print(p2)
  
  if (compare) {
    ## Analyses Requiring Actual SMAP Data ##
    smap_data <- all_smap_data[[betySiteId]]
    
      
    ## Quick Plot of Actual SMAP Data ##
    plot(smap_data$Date, smap_data$ssm, main = 'SMAP ssm (Surface Soil Moisture) vs Time',
         xlab = 'Date of Capture', ylab = 'Surface Soil Moisture (ssm)')
    p3 <- recordPlot()
    
    ## Hex distribution of Model Data with Actual Values as well as Mean and Quantile Range (Way too much) ##
    p4 <- ggplot(moistureFrame, aes(x = time, y = moisture)) + geom_hex() + theme_bw() + 
      geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) +
      geom_line(data = quantiles_lower, aes(x = time, y = q_lower, color = '95% Quantile Range')) + 
      geom_line(data = quantiles_upper, aes(x = time, y = q_upper, color = '95% Quantile Range')) +
      geom_line(data = smap_data, aes(x = as_datetime(Date), y = ssm, color = 'Actual SMAP Values')) +
      ggtitle('Hex Distribution, Quantile Range, and Mean of \nModel Data Along with Actual SMAP Values') +
      theme(plot.title = element_text(hjust = 0.5)) + xlab('Date of Prediction/Collection') + 
      ylab('Surface Soil Moisture (ssm)') + labs(colour = 'Legend') + 
      guides(fill=guide_legend(title = 'Frequency of Model Data'))
    print(p4)
    
    ## Actual Data with Mean and 95% quantile range of Model Data ##
    p5 <- ggplot() + theme_bw() + 
      geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) +
      geom_line(data = quantiles_lower, aes(x = time, y = q_lower, color = '95% Quantile Range')) + 
      geom_line(data = quantiles_upper, aes(x = time, y = q_upper, color = '95% Quantile Range')) +
      geom_line(data = smap_data, aes(x = as_datetime(Date), y = ssm, color = 'Actual SMAP Values')) + 
      ggtitle('Actual SMAP vs Mean and 95% Quantile Range of Model Data') + 
      theme(plot.title = element_text(hjust = 0.5)) + xlab('Date of Prediction/Collection') + 
      ylab('Surface Soil Moisture (ssm)') + labs(colour = 'Legend')
    print(p5)
    
    ## Collapsing Temporal Scale to Match SMAP Output Data ##
    vec_time <- vector()
    vec_mean <- vector()
    vec_sd <- vector()
    running_mean <- 0
    running_sd <- 0
    
    for (row in 1:nrow(mean_per_ENS)) {
      cur_day <- as_date(mean_per_ENS$time[row]) - 1 ## -1 to match days. This really isn't that necessary
      running_mean <- running_mean + as.numeric(mean_per_ENS[row, 'mean'])
      running_sd <- running_sd + as.numeric(sd_per_ENS[row, 'sd'])
      
      if (row %% 24 == 0){ ## 1:24 ratio between actual and model sample duration
        vec_time <- append(vec_time, cur_day)
        vec_mean <- append(vec_mean, (running_mean / 24))
        vec_sd <- append(vec_sd, running_sd / 24)
        running_mean <- 0
        running_sd <- 0 
        cur_day <- (cur_day + 3)
      }
    }
    df <- data.frame(vec_time, vec_mean, vec_sd)
    
    ## Quick Plot of Actual SMAP Data vs Mean Model Data ##
    mod <- lm(smap_data$ssm ~ df$vec_mean)
    plot(df$vec_mean, smap_data$ssm, main= 'Actual SMAP vs Mean Model Data\nw/ Linear Regression Line', 
         xlab = 'Mean of Model Data', ylab = 'Actual SMAP (ssm)') + abline(mod)
    p6 <- recordPlot()
    
    ## Calculating Bias ##
    pbias <- pbias(df['vec_mean'], smap_data['ssm'], rm.NA=TRUE)
    
    ## Calculating RMSE ##
    RMSE <- rmse(smap_data$ssm, df$vec_mean)
    
    ## Calculating CRPS ##
    CRPS <- crps(smap_data$ssm, as.matrix(cbind(df$vec_mean, df$vec_sd)))$CRPS
    
    ## Calculating Correlation ##
    correlation <- cor(df$vec_mean, smap_data$ssm)
    
    ## Stat Summary of Linear Regression Model Including R^2 Value ##
    r.squared <- summary(mod)$r.squared
    
    ## Outputs ##
    cat(paste0('\nStats for site ', betySiteId, ':'))
    cat('\nPercent Bias:', pbias, '%\nRMSE:', RMSE,'\nCRPS:', CRPS,
        '\nCorrelation:', correlation, '\nR-Squared:', r.squared, '\n')
    
    ## Quick Plot of Residuals vs. Time ##
    plot(df$vec_time, mod$residuals, main='Residuals vs. Time', xlab = 'Date of Collection', 
         ylab = 'Residuals of SMAP and Model Data') + abline(h = 0, lty = 2)
    p7 <- recordPlot()
    
    vtr <- list(p1, p2, p3, p4, p5, p6, p7, pbias, RMSE, CRPS, correlation, r.squared)
    
    names(vtr) <-  c('Plot1', 'Plot2', 'Plot3', 'Plot4', 'Plot5', 'Plot6', 'Plot7',
                     'pbias','RMSE', 'CRPS', 'correlation', 'r.squared')
    
  } else {
    vtr <- list(p1, p2)
    
    names(vtr) <- c('Plot1', 'Plot2')
  }
  return(vtr)
}


#' @title main
#'
#' @param sites a vector of site id numbers to run the analysis on
#' @param compare (boolean) to determine whether the user wishes to compare to actual 
#' SMAP data or simply look at the model data
#' @param pdf_output directory for pdf of output graphics
#'
#' @examples 
#' main(c('1000004945', '1000004876', '1000004927', '1000004916'), compare = TRUE, 
#' pdf_output = '/projectnb/dietzelab/jbowers1/graphic_output/')
#' 
main <- function(sites, compare = TRUE, pdf_output){
  
  ## run analysis on each specified site ##
  for (site in sites) {
    pdf(paste0(pdf_output, site,'.pdf'), width = 7.5, height = 5)
    runAnalysis(site, compare)
    dev.off()
  }
}

# Running All Sites #
all_sites <- PEcAn.settings::read.settings(input_dir)
sites_vector <- c()
for (index in 1:length(all_sites)) {
 sites_vector <- append(sites_vector, all_sites[[index]]$run$site$id) 
}
sites_vector <- sites_vector[c(-4, -33, -37)] # Removes D17 sites that are not collected by SMAP
main(sites_vector, compare = TRUE, pdf_output = output_dir)

