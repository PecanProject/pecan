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
      actionButton("go", "Load Data"),
      selectInput("variable_name", "Variable Name", "")
      
      # selectInput("workflow_id", "Workflow ID", c(99000000077)),
      # selectInput("run_id", "Run ID", c(99000000002)),
      # selectInput("variable_name", "Variable Name", c("AutoResp","GPP"))
    ),
    mainPanel(
      plotlyOutput("outputPlot"
                 ## brushOpts and dblclick not supported by plotly  
                 # brush = brushOpts(id = "plot_brush",
                 #                   resetOnNew = TRUE),
                 # dblclick = "plot_dblclick"
                 ),
      # Checking variable names
      verbatimTextOutput("info"),
      verbatimTextOutput("info1"),
      verbatimTextOutput("info2"),
      verbatimTextOutput("info3")
    )
  )
))
