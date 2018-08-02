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
  Shared.data$selected_row <- as.character(Shared.data$local_files[input$dtfiles_rows_selected,]) #_local
})

output$test <- renderPrint({Shared.data$selected_row}) #_local

observeEvent(input$nextFromLocal, {
  show("input_record_box")
  hide("nextFromLocal_div")
})
