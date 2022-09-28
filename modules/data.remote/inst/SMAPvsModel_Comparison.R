##'@name SMAPvsModel_Comparison
##'@description: This script collects ensemble model data from a specified file location, 
##'analyzes that data to produce two exploratory graphs, and finally compares the
##'model data to actual soil moisture data collected from NASA's SMAP satellite to
##'produce an additional 5 graphs and a set of comparative statistics including: pbias, 
##'RMSE, CRPS, Correlation, and r-squared values.
##'
##'This script is broken into 2 functions: runSMAPvalidation and main.  runSMAPvalidation can
##'be run on its own, however, the main method was developed to make utilizing runSMAPvalidation
##'more streamlined.
##'
##'This script also utilize the download_SMAP.R and netCDFvarExtraction scripts.
##'
##'WARNING! D17 NEON sites including Lower Teakettle, Soaproot Saddle, and San Joaquin 
##'Experimental Range are not available available in SMAP and thus should not be
##'run with this script.
##'
##'@author Joshua Bowers
##'
##'@examples
##'
##'## Desired Date Range ##
##'test_start <- '2016-01-02'
##'test_end <- '2016-07-16'
##'
##'## Directory of Ensembles Files ##
##'test_ensemble_dir <- '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/'
##'
##'## CONUS NEON Sites XML File Location ## - could also be vector of all conus site IDs
##'test_input_dir <- '~/pecan/modules/data.remote/inst/conus_sites.xml'
##'
##'## File Directory to Place Output PDFs In ##
##'test_output_dir <- '/projectnb/dietzelab/jbowers1/graphic_output/'
##'
##'## Output Directories for geoJson Files ##
##'test_geoJSON_outdir <- '/projectnb/dietzelab/jbowers1/geoFiles/'
##'
##'## Output Directories for SMAP netCDF Files ##
##'test_smap_outdir <- '/projectnb/dietzelab/jbowers1/smap_ncFiles/'
##'
##'# Running All Sites #
##'all_sites <- PEcAn.settings::read.settings(test_input_dir)
##'sites_vector <- c()
##'
##'for (index in 1:length(all_sites)) {
##'sites_vector <- append(sites_vector, all_sites[[index]]$run$site$id)
##'}
##'
##'sites_vector <- sites_vector[c(-4, -33, -37)] # Removes D17 sites that are not collected by SMAP
##'main_test_all <- main(sites_vector, test_start, test_end,
##'pdf_output = test_output_dir,
##'geoJSON_outdir = test_geoJSON_outdir,
##'smap_outdir = test_smap_outdir,
##'ensemble_dir = test_ensemble_dir)
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

#' @title runSMAPvalidation
#' 
#' @description runs multiple analyses on the specified site during a given date range
#' and has the functionality to compare those values to actual soil moisture values
#' collected from SMAP.  If smpa_data is left as default, no comparison will be made
#' and only forcast specific analyses will be run.
#'
#' @param betySiteId bety site id for the desired location as (String)
#' @param start start date of validation and comparison analysis YYYY-MM-DD as (Date)
#' @param end end date of validation and comparison analysis YYYY-MM-DD as (Date)
#' @param smap_data downloaded SMAP data as (data.frame) -- see download_SMAP.R module 
#' @param ensemble_dir directory of ensembles to run validation as (String)
#'
#' @return returns a list consisting all analysis plots and statistics
#'
#' @examples 
#' harv.var <- runSMAPvalidation('1000004945', as.Date('2016-01-02'), as.Date('2016-07-16'), 
#' download_SMAP('1000004945', '2016-01-02', '2016-07-16', 
#'    '/projectnb/dietzelab/jbowers1/geoFiles/', 
#'    '/projectnb/dietzelab/jbowers1/smap_ncFiles/'), 
#' '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/')
#' 
runSMAPvalidation <- function(betySiteId, start, end, smap_data = NULL, ensemble_dir){
  
  source('~/pecan/modules/data.remote/inst/netCDFvarExtraction.R')
  moistureFrame <- extractVariableENS(betySiteId = betySiteId, predictVar = 'SoilMoist', 
                                      start = start, end = end, ensemble_dir = ensemble_dir)
  
  ## Grouping data by Date ##
  grouped_ENS <- moistureFrame %>% 
    group_by(time) %>% 
    mutate(max = max(SoilMoist)) %>%
    mutate(min = min(SoilMoist))
  
  ## Calculating Mean Per Ensemble ##
  mean_per_ENS <- summarize(grouped_ENS, mean = mean(SoilMoist))
  
  ## Calculating Standard Deviation ##
  sd_per_ENS <- summarize(grouped_ENS, sd = sd(SoilMoist))
  
  ## Calculating .025 and .975 Quantiles ##
  quantiles_lower <- summarize(grouped_ENS, q_lower = quantile(SoilMoist, .025))
  quantiles_upper <- summarize(grouped_ENS, q_upper = quantile(SoilMoist, .975))
  
  ## Hex Data Distribution with Mean-line Highlighted ##
  p1 <- ggplot(moistureFrame, aes(x = time, y = SoilMoist)) + geom_hex() + theme_bw() + 
    guides(fill=guide_legend(title = 'Frequency of Model Data')) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle('Hex Distribution of Model Data') +
    geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) + 
    labs(colour = '') + ylab('Surface Soil Moisture (ssm)') + xlab('Date of Prediction') +
    labs(colour = 'Legend')
  print(p1)
  
  ## Min/Max Values with 95% Quantile Range ## Still has messed up color legend
  p2 <- ggplot(grouped_ENS, aes(x = time, y = SoilMoist)) + theme_bw() + 
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
  
  if (!is.null(smap_data)) {
    ## Analyses Requiring Actual SMAP Data ##
    smap_data <- smap_data[[betySiteId]]
    
      
    ## Quick Plot of Actual SMAP Data ##
    plot(smap_data$Date, smap_data$ssm, main = 'SMAP ssm (Surface Soil Moisture) vs Time',
         xlab = 'Date of Capture', ylab = 'Surface Soil Moisture (ssm)')
    p3 <- recordPlot()
    
    ## Hex distribution of Model Data with Actual Values as well as Mean and Quantile Range (Way too much) ##
    p4 <- ggplot(moistureFrame, aes(x = time, y = SoilMoist)) + geom_hex() + theme_bw() +
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
    
    vtr <- list(betySiteId, p1, p2, p3, p4, p5, p6, p7, pbias, RMSE, CRPS, correlation, r.squared)
    
    names(vtr) <-  c('betySiteId', 'Plot1', 'Plot2', 'Plot3', 'Plot4', 'Plot5', 'Plot6', 'Plot7',
                     'pbias','RMSE', 'CRPS', 'correlation', 'r.squared')
    
  } else {
    
    vtr <- list(betySiteId, p1, p2)
    
    names(vtr) <- c('betySiteId', 'Plot1', 'Plot2')
  }
  return(vtr)
}

#' @title main
#'
#' @param sites a vector of site id numbers to run the analysis on
#' @param start start date of validation and comparison analysis YYYY-MM-DD as (String)
#' @param end end date of validation and comparison analysis YYYY-MM-DD as (String)
#' @param pdf_output directory for pdf of output graphics as (String)
#' @param geoJSON_outdir directory to place geoJSON files necessary for SMAP download as (String)
#' @param smap_outdir directory to place SMPA output netCDF files as (String)
#' @param ensemble_dir directory of ensembles to run validation as (String)
#' 
#' @return data frame object containing stats for each site
#'
#' @examples 
#'## runs validation on 1000004945, 1000004876, 1000004927, 1000004927 sites ##
#'main_test_sample <- main(c('1000004945', '1000004876', '1000004927', '1000004927'), 
#'start = '2016-01-02', 
#'end = '2016-07-16', 
#'pdf_output = '/projectnb/dietzelab/jbowers1/graphic_output/', 
#'geoJSON_outdir = '/projectnb/dietzelab/jbowers1/geoFiles/', 
#'smap_outdir = '/projectnb/dietzelab/jbowers1/smap_ncFiles/', 
#'ensemble_dir = '/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/')
#'
#' 
main <- function(sites, start, end, pdf_output, geoJSON_outdir, smap_outdir, ensemble_dir){
  ## Downloads SMAP data for sites ##
  source('~/pecan/modules/data.remote/inst/download_SMAP.R')
  smap_data <- download_SMAP(sites, start, end, geoJSON_outdir, smap_outdir)
  
  ## Convert to Date for Validation ##
  start <- as.Date(start)
  end <- as.Date(end)
  
  stat_frame <- data.frame(site_id = character(),
                   pbias = double(), 
                   RMSE = double(), 
                   CRPS = double(),
                   correlation = double(),
                   r.squared = double())
  
  ## run analysis on each specified site ##
  count <- 0
  for (site in sites) {
    count <- count + 1
    ## Prints graphics to pdf and stats to returned df ##
    pdf(paste0(pdf_output, site,'.pdf'), width = 7.5, height = 5)
    temp_run <- runSMAPvalidation(site, start, end, smap_data, ensemble_dir)
    stat_frame[count, ] <- list(temp_run$betySiteId, 
                                temp_run$pbias, 
                                temp_run$RMSE, 
                                temp_run$CRPS,
                                temp_run$correlation,
                                temp_run$r.squared)
    dev.off()
  }
  return(stat_frame)
}

