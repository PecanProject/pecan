tabPanel("Settings", 
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
)