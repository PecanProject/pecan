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
library(shinyjs)

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

  # Hiding the animation and showing the application content
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  showElement("app")

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
  observeEvent(input$load_model,{
    req(input$all_run_id)
    ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    button <- FALSE
    print(nrow(ids_DF))
    if(nrow(ids_DF) == 1){
      source("server_files/benchmarking_server.R", local = TRUE)
    }else if(nrow(ids_DF) > 1){
      brr_message <- "Benchmarking currently only works when one run is selected."
    }else{
      brr_message <- "Cannot do benchmarking"
    }
  })

  }) # Shiny server closes here
