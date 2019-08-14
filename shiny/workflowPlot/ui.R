library(shiny)
library(plotly)
library(highcharter)
library(shinythemes)
library(knitr)
library(shinyjs)
library(shinytoastr)
library(shinyWidgets)
library(bsplus)

source("ui_utils.R", local = TRUE)

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                # Initializing shinyJs
                useShinyjs(),
                # Initializing shinytoastr
                useToastr(),
                # Adding CSS to head
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                tags$head(
                  tags$script(src="scripts.js")
                  ),
                tags$head(
                  tags$style(HTML("
                       .modal-lg {width: 85%;}
                       .navbar-default .navbar-nav{font-size: 16px;
                                                   padding-top: 10px;
                                                   padding-bottom: 10px;
                                                  }
                                  ")
                      )
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
                      navbarPage(title = NULL,
                                           tabPanel("Select Data",
                                                    icon = icon("hand-pointer"),
                                                   tagList(
                                                     column(3,
                                                            source_ui("sidebar_UI.R")
                                                            ),
                                                     column(9,
                                                            source_ui("select_data_UI.R")
                                                            )
                                                   )
                                           ),
                                           tabPanel("History Runs",
                                                    icon = icon("history"),
                                                    DT::DTOutput("historyfiles")
                                           ),
                                           tabPanel("Exploratory Plots",
                                                    icon = icon("chart-bar"),
                                                    tabsetPanel(
                                                      source_ui("model_plots_UI.R"),
                                                      source_ui("model_data_plots_UI.R"),
                                                      source_ui("pdf_viewer_UI.R")
                                                    )
                                           ),
                                           tabPanel("Benchmarking",
                                                    icon = icon("pencil-ruler"),
                                                    tabsetPanel(
                                                      source_ui("benchmarking_ScoresPlots_UI.R"),
                                                      source_ui("benchmarking_settings_UI.R")
                                                    )
                                           ),
                                           tabPanel("Documentation",
                                                    icon = icon("book"),
                                                    #withMathJax(includeMarkdown("markdown/workflowPlot_doc.Rmd"))
                                                    source_ui("documentation_UI.R"),
                                                    use_bs_accordion_sidebar()
        
                                           )
                      )
                  )
                )
                )
