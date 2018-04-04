library(shiny)
library(plotly)
library(shinythemes)
library(knitr)

source("ui_utils.R", local = TRUE)

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                sidebarLayout(
                  source_ui("sidebar_UI.R"), # Sidebar
                  mainPanel(navbarPage(title = NULL, 
                                       source_ui("select_data_UI.R"),
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
                ))