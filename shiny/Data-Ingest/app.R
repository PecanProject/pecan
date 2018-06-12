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
    # menuItem(
    #   "Select Data Acquisition Method",
    #   tabName = "importDataONE",
    #   icon = icon("file")
    # ),
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
             icon = icon("cog")
    )
  )),
  dashboardBody(
    tabItems(
    ################ Tab 1 -- DataONE download ##################################
    tabItem(tabName = "importDataONE",
            fluidRow(
              box(
                textInput(
                  "id",
                  label = h2("Import From DataONE"),
                  placeholder = "Enter doi or id here"
                ),
                p("Copy Paste the following DOI as an example: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87"),
                actionButton(inputId = "D1Button", label = "Download"),
                hr(),
                tableOutput("identifier"),
                hr(),
                actionButton(inputId = "D1FinishButton", label = "Finish Download")
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
                  label = h2("Upload Local Files"), 
                  accept = NULL,
                  multiple = FALSE,
                  placeholder = "Drag and drop files here"
                ),
                h3("This feature is currently unavailable"),
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
  
  Shared.data<-reactiveValues(downloaded=NULL)
  
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  print(temp) #for testing only -- show in console only
  
  # Create two sub-directories in the tempfile
  d1_tempdir <- file.path(temp, "d1_tempdir")
    dir.create(d1_tempdir, showWarnings = F)
  local_tempdir <- file.path(temp, "local_tempdir")
    dir.create(local_tempdir, showWarnings = F)
    
  print(list.files(temp))

   observeEvent(input$D1Button, {
     # run dataone_download with input from id on click
     PEcAn.data.land::dataone_download(trimws(input$id), filepath = d1_tempdir) # store files in tempfile
     # newdir_D1 <<- "/tmp/Rtmp35jOEK/d1_tempdir/DataOne_doi:10.6073-pasta-63ad7159306bc031520f09b2faefcf87"
        Filename <- list.files(newdir_D1)
        D1_file_df <- as.data.frame(Filename)
        list_of_d1_files <<- list.files(newdir_D1) # I call this later
        
        # Grab the name of the D1_file from newdir_D1
        d1_dirname <<- base::sub("/tmp/Rtmp[[:alnum:]]{6}/d1_tempdir/", "", newdir_D1) # let users create their own filenames
        
        Shared.data$downloaded <- D1_file_df # Reactive Variable
  })
  
   # Display downloaded files in data.frame
   output$identifier <- renderTable(rownames = TRUE, hover = TRUE, {Shared.data$downloaded})
   
   # Move files to correct dbfiles location (make a custom function for this?)
   observeEvent(input$D1FinishButton, {
     # create the new directory in /dbfiles
     dir.create(paste0(PEcAn_path, d1_dirname))
     
     n <- length(list_of_d1_files)
     for (i in 1:n){
       base::file.copy(file.path(newdir_D1, list_of_d1_files[i]), file.path(PEcAn_path, d1_dirname, list_of_d1_files[i]))
     }
     
   })
   
   
  ######### FileInput ########################################
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