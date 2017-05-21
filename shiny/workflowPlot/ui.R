library(shiny)
source('helper.R')

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),

  sidebarLayout(
    sidebarPanel(
      selectInput("workflow_id", "Workflow ID", c()),
      selectInput("run_id", "Run ID", c()),
      selectInput("variable_name", "Variable Name", "")
    ),
    mainPanel(
      plotlyOutput("outputPlot"
                 ## brushOpts and dblclick not supported by plotly  
                 # brush = brushOpts(id = "plot_brush",
                 #                   resetOnNew = TRUE),
                 # dblclick = "plot_dblclick"
                 )
      # Checking variable names
      ,verbatimTextOutput("info")
    )
  )
))
