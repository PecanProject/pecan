tabPanel(
  "Model-Data Plots",
  br(),
  column(
    3,
    wellPanel(
      selectInput("var_name_modeldata", "Variable Name", ""),
      textInput("units_modeldata", "Units",
                placeholder = "Type units in udunits2 compatible format"),
      verbatimTextOutput("unit_text2"),
      dateRangeInput("date_range2", "Date Range", separator = " - "),
      fluidRow(
        column(6, 
               selectInput("agg2", "Aggregation", 
                           choices = c("NONE", "daily", "weekly", "monthly", "quarterly", "annually"), 
                           selected = "daily")),
        column(6,
               selectInput("func2", "function", 
                           choices = c("mean", "sum"), 
                           selected = "mean")
        )
      ),
      radioButtons(
        "plotType_modeldata",
        "Plot Type (for Model Outputs)",
        c("Scatter Plot" = "point", "Line Chart" = "line"),
        selected = "point"
      ),
      sliderInput(
        "smooth_n_modeldata",
        "Value for smoothing:",
        min = 0,
        max = 1,
        value = 0.8
      ),
      tags$hr(),
      actionButton("ex_plot_modeldata", "Generate Plot", icon = icon("pencil-alt"),
                   width = "100%", class="btn-primary")
    )
  ),
  column(
    9,
    h3("Time series"),
    highchartOutput("modelDataPlot", height = "500px"), br(),
    h3("Scatter Plot"),
    highchartOutput("modelDataPlotscatter", height = "500px")
  )
)
