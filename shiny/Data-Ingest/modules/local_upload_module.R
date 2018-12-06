localUploadUI <- function(id){
  ns <- NS(id)
  
  box(width = 4, title = h2("Upload Local Files"), solidHeader = T, status = "success",
      # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
      fileInput(
        inputId = ns("file"),
        label = "Upload Local Files", 
        accept = NULL,
        multiple = TRUE,
        placeholder = "Drag and drop files here"
      ),
      DT::DTOutput(ns("dtfiles")),
      verbatimTextOutput(ns("test")),
      hr(),
      textInput(
        ns("new_local_filename"),
        label = "Set Destination Directory (for testing only)",
        placeholder = "Enter New Directory Name Here"
      ),
      actionButton(ns("LocalFinishButton"), label = "Finish Download"),
      hr(),
      p("Location of Downloaded Files: (Testing Only)"),
      verbatimTextOutput(ns("LocaldbfilesPath"))
  )
  
}

localUpload <- function(input, output, session){
  
  observe({
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
    uploaded_local <- as.data.frame(list.files(file.path(temp, "local_tempdir")))
    names(uploaded_local) <- "Available Files"
    Shared.data$local_files <- uploaded_local 
    
  })
  
  output$dtfiles <- DT::renderDT({Shared.data$local_files}, selection = 'single', options = list(ordering = F, dom = 'tp'))
  
  observe({
    Shared.data$selected_row_local <- as.character(Shared.data$local_files[input$dtfiles_rows_selected,])
  })
  
  output$test <- renderPrint({Shared.data$selected_row_local})
  
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