# Load PEcAn specific packages, does this need to be so specific?
library(PEcAn.visualization)
library(PEcAn.DB)
library(PEcAn.settings)
library(PEcAn.benchmark)
library(PEcAn.utils)

# Shiny and plotting packages 
library(shiny)
library(ggplot2)
library(plotly)

# Data management
library(dplyr)
library(reshape2)
library(purrr)
library(ncdf4)
library(scales)
library(lubridate)

# Maximum size of file allowed to be uploaded: 100MB 
options(shiny.maxRequestSize=100*1024^2) 

# Define server logic
server <- shinyServer(function(input, output, session) {
  bety <- betyConnect()
  
  # Source Extra Funtions 
  source("workflowPlot_fcns.R", local = TRUE) # Load all functions that need to be defined for this script
  
  # Sidebar
  source("server_files/sidebar_server.R", local = TRUE)
  
  # Page 1: Select Data
  source("server_files/select_data_server.R", local = TRUE)
  
  # Page 2: Exploratory Plots
  source("server_files/model_plots_server.R", local = TRUE)
  source("server_files/model_data_plots_server.R", local = TRUE)
  
  # Page 3: Benchmarking
  source("server_files/benchmarking_server.R", local = TRUE)
  
}) # Shiny server closes here  
