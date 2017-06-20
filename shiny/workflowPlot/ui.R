library(shiny)
source('helper.R')

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),
  
  sidebarLayout(
    sidebarPanel(
      # helpText(),
      p("Please select the workflow ID to continue. You can select multiple IDs"),
      selectizeInput("all_workflow_id", "Mutliple Workflow IDs", c(),multiple=TRUE),
      p("Please select the run ID. You can select multiple IDs"),
      selectizeInput("all_run_id", "Mutliple Run IDs", c(),multiple=TRUE),
      actionButton("load", "Load Model outputs"),
      selectInput("workflow_id", "Workflow ID", c()),
      selectInput("run_id", "Run ID", c()),
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
