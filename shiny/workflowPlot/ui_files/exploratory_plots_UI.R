tabPanel(h4("Exploratory Plots"), 
         column(12, plotlyOutput("outputPlot")),
         column(12, wellPanel( actionButton("ex_plot", "Generate Plot"))),
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
)
