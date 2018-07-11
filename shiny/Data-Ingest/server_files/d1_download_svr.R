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

output$rowSelection <- renderPrint(input$identifier_rows_selected)

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


