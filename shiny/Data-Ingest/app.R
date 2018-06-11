library(shiny)
library(PEcAn.data.land)
library(PEcAn.utils)
library(shinydashboard)
library(dataone)
library(shinyFiles)
library(stringr)
library(unixtools) #DEVELOPMENT VERSION! UNSURE IF THIS IS VIABLE FOR IMPLEMENTATION IN PECAN

#############################################################################
################################## UI #######################################
#############################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Data Ingest"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Import from DataONE",
      tabName = "importDataONE",
      icon = icon("file")
    ),
    menuItem(
      "Upload Local Files",
      tabName = "uploadLocal",
      icon = icon("file")
    ),
    menuItem(
      "Step 2 -- dbfiles record",
      tabName = "step2",
      icon = icon("cog")
    ),
    menuItem(
      "Step 3 -- format record",
      tabName = "step3",
      icon = icon("cog")
    ),
    menuItem("Step 4 -- etc.", 
             tabName = "step4", 
             icon = icon("cog"))
  )),
  dashboardBody(
    tabItems(
    ################ Tab 1 -- DataONE download ##################################
    tabItem(tabName = "importDataONE",
            fluidRow(
              box(
                textInput(
                  "id",
                  label = h3("Import From DataONE"),
                  placeholder = "Enter doi or id here"
                ),
                actionButton(inputId = "D1Button", label = "Download"),
                tableOutput("identifier")
              )
            )
            ),
    ################ Tab 2 -- Local File Upload ##################################
    tabItem(tabName = "uploadLocal",
            fluidRow(
              box(
                # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
                fileInput(
                  inputId = "file",
                  label = h3("Upload Local Files"),
                  accept = NULL,
                  multiple = FALSE,
                  placeholder = "Drag and drop files here"
                ),
                tableOutput("contents"),
                actionButton(inputId = "EmptyDirectoryButton", label = "Clear All")
              )
              )
            ),
    
    tabItem(tabName = "step2",
            h2("coming soon")),
    
    tabItem(tabName = "step3",
            h2("under construction")),
    
    tabItem(tabName = "step4",
            h2("under construction"))
    
  ))
)
############################################################################################################
######################################### SERVER SIDE ######################################################
############################################################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) #maximum file input size
  
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  print(temp) #for testing only -- shows in console only
  
  # Create a directory in the tempfile
  d1_tempdir <- dir.create(file.path(temp, "d1_tempdir"), showWarnings = F)
  local_tempdir <- dir.create(file.path(temp, "local_tempdir"), showWarnings = F)
  print(list.files(temp))
 # print(list.files(d1_tempdir))

  d1d <- eventReactive(input$D1Button, {
    withProgress(message = "Downloading", value = 0, {
      PEcAn.data.land::dataone_download(input$id, filepath = PEcAn_path)
    }) #run dataone_download with input from id on click
  })

   output$identifier <- renderTable({
     D1File <- input$id

     if (is.null(D1File))
       return(NULL)

       return(list.files(paste(temp, "d1_tempdir", sep = '/')))
   })


  ###### FileInput
  output$contents <- renderTable({
   #localdownload <- eventReactive({
    inFile <- input$file
    n <- length(inFile$name)
    names <- inFile$name

     if (is.null(inFile))  # Do I need this here?
       return(NULL)

    splits <- list()

    for (i in 1:n) {
      splits <- base::sub("/tmp/Rtmp[[:alnum:]]{6}/", "", inFile[i, "datapath"])  # Consider making this more program agnostic?
      print(splits)

      filenames <- list.files(temp)
      oldpath <- file.path(temp, splits[i])
        print(oldpath[i])
        print(list.files(temp)[i])
        print(file.path(temp, inFile[i, "name"]))
       base::file.rename(oldpath[i], file.path(temp, inFile[i, "name"])) # rename the file to include the original filename
       base::unlink(dirname(oldpath[i]), recursive = TRUE) # remove the file with the userhostile name
    }
     return(list.files(paste(temp, "local_temp", sep = '/'))) # I think I should move this too
  })

  # This doesn't work yet
  # output$contents <- renderTable({
  #   localdownload()
  #
  #   if (is.null(inFile))
  #      return(NULL)
  #
  #   return(list.files(temp))
  # })


############### Empty Directory Button (for testing) ########################
  observeEvent(input$"EmptyDirectoryButton", {
    tempfiles <- list.files(temp)
    nfiles <- length(tempfiles)
    # print(temp)
    # print(tempfiles)

    for (i in 1:nfiles) {
      filepath <- file.path(temp, tempfiles[i])
    #  base::file.remove(filepath[i])

    }
     lapply(tempfiles,unlink)
     print(list.files(temp))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87