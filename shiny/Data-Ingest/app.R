 library(PEcAn.data.land)
 library(PEcAn.visualization)
 library(PEcAn.utils)
 library(shinydashboard)
 library(dataone)
 library(stringr)
 library(shinyFiles)
 library(DT)
 library(shinyjs)
 library(shiny)

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
  )
  ),
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
                p("Copy and Paste the following example data sets:"),
                p("doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87"),
                p("doi:10.6073-pasta-f31b28b912e6051bf1d383ff1ef18987"),
                  actionButton(inputId = "D1Button", label = "Download"),
                #  actionButton(inputId = "CancelD1Download", label = "Cancel Download"), This is WAY tricky. Not sure if I can add this functionality... 
                hr(),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div(id="loadmessage",
                                          HTML(paste0("<div> <h3>Download in Progress.</h3> <p>This download may take a couple of minutes.</p> <img src=\'http://www.lettersmarket.com/uploads/lettersmarket/blog/loaders/common_green/ajax_loader_green_64.gif' height=\"64\" width=\"64\"> </div>"))
                                           )), 
                DTOutput("identifier"), 
                actionButton(inputId = "D1FinishButton", label = "Finish Download"),
                hr(),
                p("Location of Downloaded files:"),
                verbatimTextOutput("D1dbfilesPath")
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
                tableOutput("contents"),
                hr(),
                textInput(
                  "new_local_filename",
                  label = h4("Set Destination Directory"),
                  placeholder = "Enter New Directory Name Here"
                ),
                actionButton(inputId = "LocalFinishButton", label = "Finish Download"),
                hr(),
                p("Location of Downloaded Files:"),
                verbatimTextOutput("LocaldbfilesPath")
                
              )
              )
            ),
    
    tabItem(tabName = "step2",
            h2("coming soon")),
    
    tabItem(tabName = "step3",
            h2("under construction")),
    
    tabItem(tabName = "step4",
            h2("under construction"))
    
  )),
  title = "PEcAn Data Ingest",
  skin =  "green"
)
############################################################################################################
######################################### SERVER SIDE ######################################################
############################################################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) #maximum file input size
  
  ## Setup ##
  Shared.data <- reactiveValues(downloaded=NULL)
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  
##################### DataONE Download #############################################  
  ## Create two sub-directories in the tempfile ##
  d1_tempdir <- file.path(temp, "d1_tempdir")
    dir.create(d1_tempdir, showWarnings = F)
  local_tempdir <- file.path(temp, "local_tempdir")
    dir.create(local_tempdir, showWarnings = F)

   observeEvent(input$D1Button, {
     # run dataone_download with input from id on click
     PEcAn.data.land::dataone_download(trimws(input$id), filepath = d1_tempdir) # store files in tempfile
     list_of_d1_files <<- list.files(newdir_D1)
     D1_file_df <- as.data.frame(list_of_d1_files)
        
        
      # Grab the name of the D1_file from newdir_D1
      d1_dirname <<- base::sub("/tmp/Rtmp[[:alnum:]]{6}/d1_tempdir/", "", newdir_D1) # let users create their own filenames eventually
       
      Shared.data$downloaded <- D1_file_df # Reactive Variable 
  })
  
   # Display downloaded files in data.frame
   output$identifier <- DT::renderDT({Shared.data$downloaded})
   
   # Move files to correct dbfiles location (make a custom function for this?)
   observeEvent(input$D1FinishButton, {
     # create the new directory in /dbfiles
     dir.create(paste0(PEcAn_path, d1_dirname))
     
     n <- length(list_of_d1_files) 
        for (i in 1:n){
          base::file.copy(file.path(newdir_D1, list_of_d1_files[i]), file.path(PEcAn_path, d1_dirname, list_of_d1_files[i]))
        }
     output$D1dbfilesPath <- renderText({paste0(PEcAn_path, d1_dirname)}) # Print path to data
   })
   
  ######### FileInput ########################################
  output$contents <- renderTable({
    inFile <- input$file
    n <- length(inFile$name)
    names <- inFile$name

     if (is.null(inFile))  
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
       base::file.rename(oldpath[i], file.path(temp, "local_tempdir", inFile[i, "name"])) # rename the file to include the original filename
       base::unlink(dirname(oldpath[i]), recursive = TRUE) # remove the file with the userhostile name
    }
     return(list.files(file.path(temp, "local_tempdir"))) 
  })
   
   # Move files to correct dbfiles location (make a custom function for this?)
   observeEvent(input$LocalFinishButton, {
     # create the new directory in /dbfiles
     local_dirname <- gsub(" ", "_", input$new_local_filename) # Are there any other types of breaking chatacters that I should avoid with directory naming? 
     dir.create(file.path(PEcAn_path, local_dirname))
     
     path_to_local_tempdir <- file.path(local_tempdir)
     list_of_local_files <- list.files(path_to_local_tempdir) 
    
     n <- length(list_of_d1_files)
     for (i in 1:n){
       base::file.copy(file.path(path_to_local_tempdir, list_of_local_files[i]), file.path(PEcAn_path, local_dirname, list_of_local_files[i]))
     }
     output$LocaldbfilesPath <- renderText({paste0(PEcAn_path, local_dirname)}) # Print path to dbfiles
   })
}

# Run the application
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87