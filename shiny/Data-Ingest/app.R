library(shiny)
library(PEcAn.data.land)
library(PEcAn.utils)
library(shinydashboard)
library(dataone)
library(shinyFiles)
library(stringr)


# Define UI for application

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Ingest"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Ingest Workflow", tabName = "importData", icon = icon("file")),
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
                  actionButton(inputId = "D1Button", label = "Download"),
                  hr(),
                  fluidRow(column(12, verbatimTextOutput("identifier"))) 
                ),
                
                box(
                  # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
                  fileInput(inputId = "file", label = h3("Upload Local Files"), accept = NULL, multiple = TRUE, placeholder = "Drag and drop files here"),
                  tableOutput("contents"),
                  actionButton(inputId = "EmptyDirectoryButton", "Empty Directory")
                )
              ),
              fluidRow(
               box(
                 
                 
               )
            
              )
      ),
      
      tabItem(tabName = "step2",
              h2("coming soon")
      ),
      
      tabItem(tabName = "step3",
              h2("under construction")
      ),
      
      tabItem(tabName = "step4",
              h2("under construction")
      )
      
      
    )
  )
)
############################################################################################################
######################################### SERVER SIDE ######################################################
############################################################################################################

server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2) #maximum file input size
  
   path <- tempdir() # PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder 
    
   d1d <- eventReactive(input$D1Button,{
                        withProgress(message = "Downloading", value = 0, {
                          PEcAn.data.land::dataone_download(input$id, filepath = path) 
                          }) #run dataone_download with input from id on click
                        })
  
  output$identifier <- renderText({
    d1d()
    
    
  })
  
 
###### FileInput 
  output$contents <- renderTable({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file
      n <- length(inFile$name)
      names <- inFile$name
      
    if (is.null(inFile))
      return(NULL)

      for(i in 1:n){
        # find the tempdir for the R session
        splits <- base::sub("/.../........../", "", inFile[i,"datapath"])
        t_dir <- stringr::str_replace(inFile[i,"datapath"], splits[i], "")
        base::file.rename(paste0(t_dir, "/", splits[i]), paste0(t_dir, "/", inFile[i, "name"])) # rename the file to include the original filename
        base::file.remove(paste0(t_dir, "/", splits[i])) # remove the file with the userhostile name
      }
      return(list.files(t_dir))
      
       unlink(t_dir)
      #return(inFile[,"datapath"])
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87