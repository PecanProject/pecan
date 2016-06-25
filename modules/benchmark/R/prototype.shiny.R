require(shiny)

metrics.plot <- metrics$name[grep(".plot", metrics$name)]
metrics.score <- metrics$name[-grep(".plot", metrics$name)]

ui <- shinyUI(pageWithSidebar(
  headerPanel = ("Benchmarking Visualization Prototype"),
    sidebarPanel(
      conditionalPanel(condition="input.conditionedPanels==1",
                       helpText("Plot 1"),
                       selectInput(inputId = "r", label = "Reference Run ID", 
                                   choices = 90000000001:90000000004, selected=90000000004),
                       selectInput(inputId = "b", label = "Benchmark Data Set", choices = "CDIAC NPP"),
                       selectInput(inputId = "v", label = "Variable", choices = dat_vars, 
                                   selected = "LeafLitter"),
                       radioButtons(inputId = "metric.plot", label="What plot you like to see?", 
                                    choices = metrics.plot, selected = "timeseries.plot")
      ),
      conditionalPanel(condition="input.conditionedPanels==2",
                       helpText("Content Panel 2"),
                       selectInput(inputId = "r", label = "Reference Run ID", 
                                   choices = 90000000001:90000000004, selected=90000000004),
                       selectInput(inputId = "b", label = "Benchmark Data Set", choices = "CDIAC NPP")
      ) 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("p"), value = 1), 
        tabPanel("Scores", dataTableOutput('scores'), value=2), 
        tabPanel("Benchmarking Inputs", dataTableOutput('obvs'), value=2),
        tabPanel("Model Outputs", dataTableOutput('model'), value=2),
        tabPanel("Aligned data", dataTableOutput('dat'), value=2),
        tabPanel("Formats", dataTableOutput('format'), value=2), 
        id = "conditionedPanels"
      )
      # plotOutput("p"),
      # fluidRow(column(2,tableOutput('table')))
    )
  )
)


server <- function(input, output){
  output$p <- renderPlot({
    metric_dat <- dat[,c(paste(input$v, c("m", "o"),sep = "." ),"posix")]
    colnames(metric_dat)<- c("model","obvs","time")
    fcn <- paste0("metric.",input$metric.plot)
    do.call(fcn, args <- list(metric_dat,dat_vars[v]))
  })
  output$scores <- renderDataTable(results)
  output$obvs  <- renderDataTable(obvs[,-which(colnames(obvs)=="posix")])
  output$model <- renderDataTable(model[,-which(colnames(model)=="posix")])
  output$dat <- renderDataTable(dat[,-which(colnames(dat)=="posix")])
  output$format <- renderDataTable(format$vars)
}

shinyApp(ui=ui, server=server)

##############################
# shinyUI(pageWithSidebar(
#   headerPanel("Conditional Panels"),
#   sidebarPanel(
#     conditionalPanel(condition="input.conditionedPanels==1",
#                      helpText("Content Panel 1")
#     ),
#     conditionalPanel(condition="input.conditionedPanels==2",
#                      helpText("Content Panel 2")
#     ) 
#   ),
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Panel 1", value=1), 
#       tabPanel("Panel 2", value=2)
#       , id = "conditionedPanels"
#     )
#   )
# ))

