#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PEcAn.data.land)
library(shinyDND)
# Define UI for application

ui <- fluidPage(
  
  titlePanel("Data Ingest"),
  
  textInput("id", label = h3("Import From DataONE"), placeholder = "Enter doi or id here"),
  actionButton(inputId = "D1Button", label = "Upload"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("identifier"))), 
  
  # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
  fileInput(inputId = "file", label = h3("Select Local Files for Upload"), accept = NULL, multiple = TRUE),
  p("One or more files")
  
)

server <- function(input, output) {
  
  d1d <- eventReactive(input$D1Button, { input$id }) #print doi on click
  
  # d1d <- eventReactive(input$D1Button, { PEcAn.data.land::dataone_download(input$id) }) #run dataone_download on click
  
  output$identifier <- renderText({
    d1d()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87