library(shiny)

# Define UI
ui <- shinyUI(
    fluidPage(
        titlePanel("Workflow plots"),
        sidebarLayout(
            sidebarPanel(
                selectInput("workflow_id", "Workflow ID", c()),
                selectInput("x_variable", "Parameter (X)", c()),
                selectInput("y_variable", "Variable (Y)", c())
            ),
            mainPanel(
                p("If no plot or error message appears, please be patient. Loading ensemble output can take a few minutes."),
                plotOutput("ensemble_plot")
                # verbatimTextOutput("test")
            ) # End mainPanel
        ) # End sidebarLayout
    ) # End fluidPage
) # End shinyUI
