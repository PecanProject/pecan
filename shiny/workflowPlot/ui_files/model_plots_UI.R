tabPanel(
  "Model Plots",
  hidden(div(id = "model_plot_interactive", column(
    12, load_anim_div(plot_div = "modelPlot")
  ))),
  div(id = "model_plot_static", column(
    12, load_anim_div(plot_div = "modelPlotStatic")
  )),
  column(12, wellPanel(
    actionButton("ex_plot_model", "Generate Plot"),
    div(actionButton("model_toggle_plot", "Toggle Plot"),
        style = "float:right")
  )),
  column(
    12,
    wellPanel(
      selectInput("var_name_model", "Variable Name", ""),
      textInput("units_model", "Units",
                placeholder = "Type units in udunits2 compatible format"),
      verbatimTextOutput("unit_text"),
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
        max = 100,
        value = 80
      )
    )
  )
)
