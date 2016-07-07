library(shiny)

# Define UI
ui <- shinyUI(
    fluidPage(
        titlePanel("Global sensitivity analysis"),
        sidebarLayout(
            sidebarPanel(
                selectInput("workflow_id", "Workflow ID", c()),
                selectInput("output_type", "Output type", c("Pairwise", "All parameters", "All variables")),
                conditionalPanel(
                    condition = "input.output_type == 'Pairwise' || input.output_type == 'All variables'",
                    selectInput("parameter", "Parameter (X)", c())
                ),
                conditionalPanel(
                    condition = "input.output_type == 'Pairwise' || input.output_type == 'All parameters'",
                    selectInput("variable", "Variable (Y)", c())
                )
            ),
            mainPanel(
                p("If no plot or error message appears, please be patient. Loading ensemble output can take a few minutes."),
                plotOutput("ensemble_plot", height="800"),
                conditionalPanel(
                  condition = "input.output_type == 'Pairwise'",
                  tableOutput("coef_table"),
                  textOutput("r2")
                )
            ) # End mainPanel
        ) # End sidebarLayout
    ) # End fluidPage
) # End shinyUI
