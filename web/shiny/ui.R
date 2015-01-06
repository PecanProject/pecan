library(shiny)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "dates",label=NULL,start = "2006-01-01",end = "2006-12-13"),
      sliderInput(inputId = "smooth",label="Smoothing",min = 0,max = 7*48,value = 48,step = 24)
      ),
    mainPanel(plotOutput("timePlot"))
  )
))