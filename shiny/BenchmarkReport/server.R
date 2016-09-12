#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$p <- renderPlot({
    metric_dat <- results[[input$b]]$aligned.dat[[input$v]]
    colnames(metric_dat)<- c("model","obvs","time")
    fcn <- paste0("metric.",input$metric.plot)
    do.call(fcn, args <- list(metric_dat,input$v,draw.plot=TRUE))
  })
  
  output$scores <- renderDataTable(results[[input$b]]$bench.results[-grep("plot",metrics),])
  output$obvs  <- renderDataTable(results[[input$b]]$obvs)
  output$model <- renderDataTable(results[[input$b]]$model)
  output$dat <- renderDataTable(results[[input$b]]$aligned.dat[[input$v]])
  output$format <- renderDataTable(results[[input$b]]$format)
  
})
