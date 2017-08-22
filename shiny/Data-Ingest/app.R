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
# source("dataone_download.R", local = FALSE)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Data Ingest"),
  
  textInput(inputId = "id", label = "Import From DataONE", value = "doi or identifier"),
  textOutput(outputId = "identifier")
  
 
)

# Define server logic
server <- function(input, output) {
  
  output$identifier <- renderText({ PEcAn.data.land::dataone_download(input$id) })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

