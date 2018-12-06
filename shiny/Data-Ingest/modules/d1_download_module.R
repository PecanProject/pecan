d1DownloadUI <- function(id){
  ns <- NS(id)
  
  box(width = 4, title = h2("Import From DataONE"), solidHeader = TRUE, status = "success",
      textInput(
        ns("id"),
        label = "Import from dataONE",
        placeholder = "Enter doi or id here"
      ),
      p("Copy and Paste the following example data sets:"),
      p("doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87"),
      p("doi:10.6073-pasta-f31b28b912e6051bf1d383ff1ef18987"),
      actionButton(ns("D1Button"), label = "Download"),
      #  actionButton(inputId = "CancelD1Download", label = "Cancel Download"), This is WAY tricky. Not sure if I can add this functionality... 
      hr(),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(id="loadmessage",
                                HTML(paste0("<div> <h3>Download in Progress.</h3> <p>This download may take a couple of minutes.</p> <img src=\'http://www.lettersmarket.com/uploads/lettersmarket/blog/loaders/common_green/ajax_loader_green_64.gif' height=\"64\" width=\"64\"> </div>"))
                       )), 
      DT::DTOutput(ns("identifier")),
      p("Selected Row (For Testing Purposes)"),
      verbatimTextOutput(ns("rowSelection")), ## For testing only
      actionButton(ns("D1FinishButton"), label = "Finish Download"),
      hr(),
      p("Location of Downloaded files:"),
      verbatimTextOutput(ns("D1dbfilesPath"))
  )
  
}

d1Download <- function(input, output, session){
  
  ## Create two sub-directories in the tempfile ##
  d1_tempdir <- file.path(temp, "d1_tempdir")
  dir.create(d1_tempdir, showWarnings = F)
  local_tempdir <- file.path(temp, "local_tempdir")
  dir.create(local_tempdir, showWarnings = F)
  
  observeEvent(input$D1Button, {
    # run dataone_download with input from id on click
    #  PEcAn.data.land::dataone_download(trimws(input$id), filepath = d1_tempdir) # store files in tempfile
    list_of_d1_files <<- c("f1", "f2", "f3", "f4", "f5")# list.files(newdir_D1)
    D1_file_df <- as.data.frame(list_of_d1_files)
    
    names(D1_file_df) <- "Available Files"
    
    Shared.data$d1fileList <- list_of_d1_files
    
    # Grab the name of the D1_file from newdir_D1
    #  d1_dirname <<- base::sub("/tmp/Rtmp[[:alnum:]]{6}/d1_tempdir/", "", newdir_D1) # let users create their own filenames eventually
    
    Shared.data$downloaded <- D1_file_df # Reactive Variable 
  })
  
  # Display downloaded files in data.frame
  output$identifier <-  DT::renderDT({Shared.data$downloaded}, selection = 'single', options = list(ordering = F, dom = 'tp'))
  
  observe({
    Shared.data$selected_row <- as.character(Shared.data$downloaded[input$identifier_rows_selected,])
  })
  
  output$rowSelection <- renderPrint({Shared.data$selected_row})
  
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
  
  
  
}