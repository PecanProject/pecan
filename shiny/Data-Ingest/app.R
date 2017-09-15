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
library(shinydashboard)
library(dataone)

#stopifnot

# Define UI for application

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Ingest"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import Data", tabName = "importData", icon = icon("file")),
      menuItem("Step 2 -- dbfiles record", tabName = "step2", icon = icon("cog")),
      menuItem("Step 3 -- format record", tabName = "step3", icon = icon("cog")),
      menuItem("Step 4 -- etc.", tabName = "step4", icon = icon("cog"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "importData",
              fluidRow( 
                box(
                  textInput("id", label = h3("Import From DataONE"), placeholder = "Enter doi or id here"),
                  actionButton(inputId = "D1Button", label = "Upload"),
                  hr(),
                  fluidRow(column(12, verbatimTextOutput("identifier"))) 
                ),
                
                box(
                  # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
                  fileInput(inputId = "file", label = h3("Upload Local Files"), accept = NULL, multiple = TRUE, placeholder = "Drag and drop files here"),
                  p("This isn't linked to the server yet")
                )
              )
      ),
      
      tabItem(tabName = "step2",
              h2("dbfiles tab content")
      )
      
      
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2) #maximum file input size
  
   #path <- PEcAn.utils::read_web_config(config.php)
    
   d1d <- eventReactive(input$D1Button, { PEcAn.data.land::dataone_download(input$id, filepath = path) }) #run dataone_download with input from id on click
  
  output$identifier <- renderText({
    d1d()
  })
  
   # output$debug <-
  
 # 
  
  output$upload <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
    #file.copy(inFile$datapath, header = input$header)


  
  
# file.copy copy from tmp file to 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87