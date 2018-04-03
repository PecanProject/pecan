tabPanel("Model-Data Plots",
         column(12, plotlyOutput("modelDataPlot")),
         column(12, wellPanel( actionButton("ex_plot_modeldata", "Generate Plot"))),
         column(12, wellPanel(
           selectInput("var_name_modeldata", "Variable Name", ""),
           selectInput("tstep_modeldata", "Time step", ""),
           textInput("units_modeldata", "Units", 
                     placeholder = "Type units in udunits2 compatible format"),
           radioButtons("plotType_modeldata", "Plot Type (for Model Outputs)",
                        c("Scatter Plot" = "point","Line Chart" = "line"),
                        selected = "point"),
           sliderInput("smooth_n_modeldata", "Value for smoothing:",
                       min = 0, max = 100, value = 80))
         )
)
