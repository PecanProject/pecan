library(shiny)
library(plotly)
library(highcharter)
library(shinythemes)
library(knitr)
library(shinyjs)
library(shinytoastr)
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
                                           tabPanel(h4("Select Data"),
                                                   tagList(
                                                     column(4,
                                                            source_ui("sidebar_UI.R")
                                                            ),
                                                     column(8,
                                                            source_ui("select_data_UI.R")
                                                            )
                                                   )
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
                                                    #withMathJax(includeMarkdown("markdown/workflowPlot_doc.Rmd"))
                                                    bs_accordion_sidebar(id = "documentation") %>%
                                                      bs_append(
                                                        title_side = "App Documentation", 
                                                        content_side = NULL,
                                                        content_main = withMathJax(includeMarkdown("markdown/app_documentation.Rmd"))
                                                      ) %>%
                                                      bs_append(
                                                        title_side = "Setup page", 
                                                        content_side = NULL,
                                                        content_main = withMathJax(includeMarkdown("markdown/setup_page.Rmd"))
                                                      ) %>%
                                                      bs_append(
                                                        title_side = "Exploratory Plots", 
                                                        content_side = NULL,
                                                        content_main = withMathJax(includeMarkdown("markdown/exploratory_plot.Rmd"))
                                                      ) %>%
                                                      bs_append(
                                                        title_side = "Benchmarking", 
                                                        content_side = NULL,
                                                        content_main = 
                                                          bs_accordion_sidebar(id = "benchmarking") %>%
                                                          bs_append(
                                                            title_side = "Settings",
                                                            content_side = NULL,
                                                            content_main = withMathJax(includeMarkdown("markdown/benchmarking_setting.Rmd"))
                                                          ) %>%
                                                          bs_append(
                                                            title_side = "Scores",
                                                            content_side = NULL,
                                                            content_main = withMathJax(includeMarkdown("markdown/benchmarking_scores.Rmd"))
                                                          ) %>% 
                                                          bs_append(
                                                            title_side = "Plots",
                                                            content_side = NULL,
                                                            content_main = withMathJax(includeMarkdown("markdown/benchmarking_plots.Rmd"))
                                                          )
                                                      ),
                                                    use_bs_accordion_sidebar()
           
                                                    
                                           )
                      )
                  )
                )
                )