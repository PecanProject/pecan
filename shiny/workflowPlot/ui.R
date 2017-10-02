library(shiny)
library(plotly)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
# source('helper.R')
# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Workflow Plots"),
  sidebarLayout(
    sidebarPanel(
      h3("Load Model Output"),
      wellPanel(
        p("Please select the workflow IDs to continue. You can select multiple IDs"),
        selectizeInput("all_workflow_id", "Mutliple Workflow IDs", c(),multiple=TRUE),
        p("Please select the run IDs. You can select multiple IDs"),
        selectizeInput("all_run_id", "Mutliple Run IDs", c(),multiple=TRUE),
        actionButton("load", "Load Model outputs")
      ),
      
      h3("Load External Data"),
      wellPanel(
        selectizeInput("all_site_id", "Select Site ID", c()),
        # If loading multiple sites in future
        # selectizeInput("all_site_id", "Select Site ID", c(), multiple=TRUE),
        selectizeInput("all_input_id", "Select Input ID", c()),
        actionButton("load_data", "Load External Data")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizations", 
                 column(12, plotlyOutput("outputPlot")),
                 column(12, wellPanel( 
                   selectInput("variable_name", "Variable Name", ""),
                   radioButtons("plotType", "Plot Type (for Model Outputs)", 
                                c("Scatter Plot" = "scatterPlot", 
                                  "Line Chart" = "lineChart"), 
                                selected="scatterPlot"),
                   radioButtons("data_geom", "Plot Type (for loaded data)", 
                                c("Scatter Plot" = "point", 
                                  "Line Chart" = "line"), 
                                selected="point"),
                   # uiOutput("slider"),
                   sliderInput("smooth_n", "Value for smoothing:",
                               min=0, max=100, value=80))
                 ),
                 verbatimTextOutput("outputNoVariableFound")
        ),
        tabPanel("Benchmarking Settings", 
                 column(12, h3("Setup Reference Run")),
                 column(12, 
                        verbatimTextOutput("brr_message"),
                        uiOutput("button_BRR")
                 ),
                 column(12,
                        h3("Setup Benchmarks")),
                 column(12, 
                        uiOutput("results_message"),
                        uiOutput("bm_inputs")
                 ),
                 column(12, h3("Calculate Benchmarks")),
                 column(12, 
                        verbatimTextOutput("calc_bm_message"),
                        # verbatimTextOutput("report"),
                        uiOutput("calc_bm_button"),
                        uiOutput("inputs_df_table"),
                        uiOutput("config_list_table"),
                        uiOutput("reportvars"),
                        uiOutput("reportmetrics"),
                        uiOutput("print_bm_settings")
                 )
        ),
        tabPanel("Benchmarking Scores", 
                 DT::dataTableOutput("results_table")
        ),
        tabPanel("Benchmarking Plots",
                 verbatimTextOutput("blarg_message"),
                 uiOutput("bm_plots"),
                 plotlyOutput("bmPlot")
        )
      )
    )
  )
))
