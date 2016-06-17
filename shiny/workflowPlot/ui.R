library(shiny)

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),

  sidebarLayout(
    sidebarPanel(
      selectInput("workflow_id", "Workflow ID", c()),
      selectInput("run_id", "Run ID", c()),
      selectInput("variable_name", "Variable Name", ""),
      dateRangeInput("dates", "Date range")
    ),

    mainPanel(
      tableOutput("params"),
      plotOutput("outputPlot")
    )
  )

))
