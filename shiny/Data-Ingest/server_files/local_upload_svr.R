observe({
  inFile <- input$file
  n <- length(inFile$name)
  names <- inFile$name
  
  if (is.null(inFile))  
    return(NULL)
  
  splits <- list()
  
  for (i in 1:n) {
    splits <- base::sub("/tmp/Rtmp[[:alnum:]]{6}/", "", inFile[i, "datapath"])  # Consider making this more program agnostic?
    filenames <- list.files(temp)
    oldpath <- file.path(temp, splits[i])
    base::file.rename(oldpath[i], file.path(temp, "local_tempdir", inFile[i, "name"])) # rename the file to include the original filename
    base::unlink(dirname(oldpath[i]), recursive = TRUE) # remove the file with the userhostile name
  }
  uploaded_local <- as.data.frame(list.files(file.path(temp, "local_tempdir")))
  names(uploaded_local) <- "Available Files"
  Shared.data$local_files <- uploaded_local 
 
})

output$dtfiles <- DT::renderDT({Shared.data$local_files}, selection = 'single', options = list(ordering = F, dom = 'tp'))

observe({
  Shared.data$selected_row <- as.character(Shared.data$local_files[input$dtfiles_rows_selected,]) #_local
})

observeEvent(input$complete_ingest_lcl, {
  tryCatch({
  # create the new directory in /dbfiles
  local_dirname <- auto.name.directory(format_name = input$InputFormatName, site_id = inputsList$siteID) # Create name from format and site_id 
  dir.create(file.path(PEcAn_path, local_dirname))
  
  path_to_local_tempdir <- file.path(local_tempdir)
  list_of_local_files <- list.files(path_to_local_tempdir) 
  
  n <- length(list_of_local_files)
  for (i in 1:n){
    base::file.copy(file.path(path_to_local_tempdir, list_of_local_files[i]), file.path(PEcAn_path, local_dirname, list_of_local_files[i]))
  }
  show("local_path_out") # Message to render path to dbfiles
  output$LocaldbfilesPath <- renderText({paste0(PEcAn_path, local_dirname)}) # Print path to dbfiles
  },
  error = function(e){
    toastr_error(title = "Error in Select Local Files", conditionMessage(e))
  },
  warning = function(e){
    toastr_warning(title = "Warning in Select Local Files", conditionMessage(e))
  }
  )
})

observeEvent(input$nextFromLocal, {
  show("input_record_box")
  hide("nextFromLocal_div")
})
