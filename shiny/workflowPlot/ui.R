library(shiny)
library(plotly)
library(shinythemes)
library(knitr)
library(shinyjs)

source("ui_utils.R", local = TRUE)

# Define UI
ui <- fluidPage(theme = shinytheme("simplex"),
                # Initializing shinyJs
                useShinyjs(),
                # Adding CSS to head
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                # Showing the animation
                div( id = "loading-content",
                     div(class = "plotlybars-wrapper",
                         div( class="plotlybars",
                              div(class="plotlybars-bar b1"),
                              div(class="plotlybars-bar b2"),
                              div(class="plotlybars-bar b3"),
                              div(class="plotlybars-bar b4"),
                              div(class="plotlybars-bar b5"),
                              div(class="plotlybars-bar b6"),
                              div(class="plotlybars-bar b7")
                         ),
                         div(class="plotlybars-text",
                             p("Shiny is on its way!")
                         )
                     )
                ),
                # Hiding the application content till the page is ready
                hidden(
                  div(
                    id = "app",
                    sidebarLayout(
                      source_ui("sidebar_UI.R"), # Sidebar
                      mainPanel(navbarPage(title = NULL,
                                           tabPanel(h4("Select Data"),
                                                    # tabsetPanel(
                                                    source_ui("select_data_UI.R")
                                                    # )
                                           ),
                                           tabPanel(h4("Exploratory Plots"),
                                                    tabsetPanel(
                                                      source_ui("model_plots_UI.R"),
                                                      source_ui("model_data_plots_UI.R")
                                                    )
                                           ),
                                           tabPanel(h4("Benchmarking"),
                                                    tabsetPanel(
                                                      source_ui("benchmarking_settings_UI.R"),
                                                      source_ui("benchmarking_scores_UI.R"),
                                                      source_ui("benchmarking_plots_UI.R")
                                                    )
                                           ),
                                           tabPanel(h4("Documentation"),
                                                    withMathJax(includeMarkdown("markdown/workflowPlot_doc.Rmd"))
                                           )
                      )

                      )
                    )
                  )
                )
                )