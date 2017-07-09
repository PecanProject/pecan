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
      radioButtons("plotType", "Plot Type", c("Scatter Plot" = "scatterPlot", "Line Chart" = "lineChart"), selected="scatterPlot"),
      tags$hr(),
      tags$hr(),
      fileInput('file1', 'Choose CSV File to upload data',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   ''),
      actionButton("load_data", "Load External Data")
    ),
    mainPanel(
      plotlyOutput("outputPlot"),
      verbatimTextOutput("info1"),
      verbatimTextOutput("info")
    )
  )
))
