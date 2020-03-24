tabPanel(
  "Model Plots",
  br(),
  column(
    3,
    wellPanel(
      selectInput("var_name_model", "Variable Name", ""),
      textInput("units_model", "Units",
                placeholder = "Type units in udunits2 compatible format"),
      verbatimTextOutput("unit_text"),
      dateRangeInput("date_range", "Date Range", separator = " - "),
      fluidRow(
        column(6, 
               selectInput("agg", "Aggregation", 
                           choices = c("NONE", "daily", "weekly", "monthly", "quarterly", "annually"), 
                           selected = "daily")),
        column(6,
               selectInput("func", "function", 
                           choices = c("mean", "sum"), 
                           selected = "mean")
               )
      ),
      radioButtons(
        "plotType_model",
        "Plot Type (for Model Outputs)",
        c("Scatter Plot" = "point", "Line Chart" = "line"),
        selected = "point"
      ),
      sliderInput(
        "smooth_n_model",
        "Value for smoothing:",
        min = 0,
        max = 1,
        value = 0.8
      ),
      tags$hr(),
      actionButton("ex_plot_model", "Generate Plot", icon = icon("pencil-alt"),
                   width = "100%", class="btn-primary")
    )
  ),
  column(
    9,
    highchartOutput("modelPlot", height = "500px")
  )
)
