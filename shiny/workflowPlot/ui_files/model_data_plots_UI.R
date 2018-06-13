tabPanel(
  "Model-Data Plots",
  hidden(div(id = "model_data_plot_interactive", column(
    12, load_anim_div(plot_div = "modelDataPlot")
  ))),
  div(id = "model_data_plot_static", column(
    12, load_anim_div(plot_div = "modelDataPlotStatic")
  )),
  column(12, wellPanel(
    actionButton("ex_plot_modeldata", "Generate Plot"),
    div(actionButton("model_data_toggle_plot", "Toggle Plot"),
        style = "float:right")
  )),
  column(
    12,
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
        max = 100,
        value = 80
      )
    )
  )
)
