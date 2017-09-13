library(shiny)
library(plotly)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
# source('helper.R')
# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),
  sidebarLayout(
    sidebarPanel(
      p("Please select the workflow IDs to continue. You can select multiple IDs"),
      selectizeInput("all_workflow_id", "Mutliple Workflow IDs", c(),multiple=TRUE),
      p("Please select the run IDs. You can select multiple IDs"),
      selectizeInput("all_run_id", "Mutliple Run IDs", c(),multiple=TRUE),
      actionButton("load", "Load Model outputs"),
      selectInput("variable_name", "Variable Name", ""),
      radioButtons("plotType", "Plot Type (for Model Outputs)", 
                   c("Scatter Plot" = "scatterPlot", 
                     "Line Chart" = "lineChart"), 
                   selected="scatterPlot"),
      # uiOutput("slider"),
      sliderInput("smooth_n", "Value for smoothing:",
                  min=0, max=100, value=80),
      tags$hr(),
      tags$hr(),
      selectizeInput("all_site_id", "Select Site ID", c()),
      # If loading multiple sites in future
      # selectizeInput("all_site_id", "Select Site ID", c(), multiple=TRUE),
      selectizeInput("all_input_id", "Select Input ID", c()),
      radioButtons("data_geom", "Plot Type (for loaded data)", 
                   c("Scatter Plot" = "point", 
                     "Line Chart" = "line"), 
                   selected="point"),
      actionButton("load_data", "Load External Data")
    ),
    mainPanel(
      plotlyOutput("outputPlot"),
      verbatimTextOutput("outputNoVariableFound")
    )
  )
))
