library(shiny)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
source('helper.R')
# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),
  sidebarLayout(
    sidebarPanel(
      # helpText(),
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
      selectizeInput("all_input_id", "Select Input ID", c()),
      # fileInput('fileUploaded', 'Choose file to upload data'
      #           # accept=c('text/csv', 
      #           #          'text/comma-separated-values,text/plain', 
      #           #          '.csv')
      #           ),
      # textInput("inputRecordID", "Input Record ID for file", "1000011260"),
      # textInput("formatID", "Format ID for file (Default CSV)", "5000000002"),
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
