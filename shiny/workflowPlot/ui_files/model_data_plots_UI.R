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
      br(),
      actionButton("ex_plot_modeldata", "Generate Plot", width = "100%", class="btn-primary")
    )
  ),
  column(
    9,
    highchartOutput("modelDataPlot", height = "500px")
  )
)
