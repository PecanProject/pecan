# ViewMet UI 

ui <- fluidPage(sidebarPanel(
                  h3("Select Files"),
                  wellPanel(
                  selectizeInput(inputId = "site.id", "Site ID", c()),
                    checkboxGroupInput(inputId = "met", label = "Met product", choices = c()),
                  checkboxGroupInput(inputId = "years", label = "Years", choices = c()),
                    actionButton(inputId ="load_data", label = "Load Met Data")
                  ),
                  h3("Select Plots"),
                  wellPanel(
                    selectizeInput(inputId = "var", "Variable", c()),
                    actionButton(inputId ="plot_data", label = "Plot Met Data")
                  )
                ),
                mainPanel(navbarPage(title = NULL,
                                     tabPanel("Files to be loaded", 
                                              DT::dataTableOutput("results_table")
                                     ),
                                     tabPanel("Combined Plot", 
                                              plotOutput("plot_overlay")
                                     ),
                                     tabPanel("Facet Plot", 
                                              plotOutput("plot_facet")
                                     )
                )
                )
)