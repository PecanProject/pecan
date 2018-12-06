#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  headerPanel = ("Benchmarking Visualization Prototype"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     selectInput(inputId = "b", label = "Benchmark Data Set", choices = names(results)),
                     selectInput(inputId = "v", label = "Variable", choices = dat.vars),
                     radioButtons(inputId = "metric.plot", label="What plot you like to see?", 
                                  choices = metrics.plot)
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     selectInput(inputId = "b", label = "Benchmark Data Set", choices = names(results))
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
    
  )
))