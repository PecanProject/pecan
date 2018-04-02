library(PEcAn.visualization)
library(PEcAn.DB)
library(PEcAn.settings)
library(PEcAn.benchmark)
library(PEcAn.utils)
library(shiny)
library(ncdf4)
library(ggplot2)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
# source('helper.R')
library(plotly)
library(scales)
library(lubridate)
library(dplyr)
library(reshape2)
library(purrr)
# Maximum size of file allowed to be uploaded: 100MB 
options(shiny.maxRequestSize=100*1024^2) 
# Define server logic
server <- shinyServer(function(input, output, session) {
  bety <- betyConnect()
  source("workflowPlot_fcns.R", local = TRUE) # Load all functions that need to be defined for this script
  
  # Sidebar
  source("server_files/sidebar_server.R", local = TRUE)
  
  # Page 1: Select Data
  source("server_files/select_data_server.R", local = TRUE)
  
  # Page 2: Exploratory Plots
  source("server_files/exploratory_plots_server.R", local = TRUE)
  
  # Page 3: Benchmarking
  source("server_files/benchmarking_server.R", local = TRUE)
  
}) # Shiny server closes here  
