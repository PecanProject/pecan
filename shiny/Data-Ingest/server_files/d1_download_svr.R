## Hide Download button until doi is entered ##
observe({
  if (is.null(input$id) || input$id == "") {
    shinyjs::disable("D1Button")
  } else {
    shinyjs::enable("D1Button")
  }
})

observeEvent(input$D1Button, {
  # run dataone_download with input from id on click
  PEcAn.data.land::dataone_download(trimws(input$id), filepath = d1_tempdir) # ("doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87", filepath = d1_tempdir) #store files in tempfile
  list_of_d1_files <<- list.files(newdir_D1) #  c("f1", "f2", "f3", "f4", "f5") #
  
  ## df to store href buttons for file preview ##
  d1_paths_df <- data.frame(href = NA)
  for(i in 1:length(list_of_d1_files)){
    d1_paths_df[i,1] <- as.character(a("Preview", href = file.path(newdir_D1, list_of_d1_files[i]), target = "_blank"))
  }
  
  D1_file_df <- cbind(list_of_d1_files, d1_paths_df)
  
  names(D1_file_df) <- c("Available Files", "")
  
  Shared.data$d1fileList <- list_of_d1_files
  
  # Grab the name of the D1_file from newdir_D1
  d1_dirname <<- base::sub("/tmp/Rtmp[[:alnum:]]{6}/d1_tempdir/", "", newdir_D1) # let users create their own filenames eventually
  
  Shared.data$downloaded <- D1_file_df # Reactive Variable 
})

# Display downloaded files in data.frame
output$identifier <-  DT::renderDT(datatable({Shared.data$downloaded}, escape = FALSE, selection = 'single', options = list(ordering = F, dom = 'tp')))

observe({
  Shared.data$selected_row <- as.character(Shared.data$downloaded[input$identifier_rows_selected, 1])
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


