tabPanel("Scores/Plots", 
         column(12, h3("Setup Reference Run")),
         column(12, 
                verbatimTextOutput("brr_message"),
                uiOutput("button_BRR")
         ),
         column(12,
                h3("Setup Benchmarks")),
         column(12, 
                uiOutput("results_message"),
                br(),
                uiOutput("bm_inputs")
         ),
         column(12, h3("Calculate Benchmarks")),
         column(12, 
                verbatimTextOutput("calc_bm_message"),
                # verbatimTextOutput("report"),
                uiOutput("calc_bm_button"),
                br(),
                textOutput("inputs_df_title"),
                br(),
                DT::dataTableOutput("inputs_df_table"),
                br(),
                br()
         ),
         fluidRow(
           column(8,
                  fluidRow(
                    column(3,offset = 1, textOutput("plots_tilte")),
                    column(8, uiOutput("bm_plots"))
                  ),
                  uiOutput("plotlybars"),
                  plotlyOutput("bmPlot"),
                  br()
                  ),
           column(4,
                  textOutput("results_df_title"),
                  br(),
                  DT::dataTableOutput("results_table")
                  )
         )
)