library(shiny)

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Prior Elicitation"),

  sidebarLayout(
    sidebarPanel(
      textInput("user", "User", c()) ,
      textInput("var","Variable",c()),
      textOutput("instructions"),
      textInput("paramVal", "Enter parameter value", "No Limit"),
      actionButton("Next", label = "Next"),
      tableOutput('table')
    ),
    mainPanel(
      plotOutput("outputPlot",
                 brush = brushOpts(id = "plot_brush",
                                   resetOnNew = TRUE),
                 dblclick = "plot_dblclick")
    )
  )
))
