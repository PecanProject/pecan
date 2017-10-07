library(shiny)

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
      plotOutput("outputPlot",
                 brush = brushOpts(id = "plot_brush",
                                   resetOnNew = TRUE),
                 dblclick = "plot_dblclick")
    )
  )
))
