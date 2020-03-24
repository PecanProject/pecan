###################################################
####### Db Files Record ###########################
###################################################

################ MachineID ########################
updateSelectizeInput(session, "InputMachineName", choices = machines, server = TRUE)

observeEvent(input$createDBFilesRecord, {
  ## MachineID 
  dbFilesRecordList$machine <- input$InputMachineName
  dbFilesRecordList$machineID <- machines_sub %>% dplyr::filter(hostname %in% dbFilesRecordList$machine) %>% pull(id)
  
  dbFilesRecordList$filePath <- input$InputFilePath
  dbFilesRecordList$fileName <- input$InputFileName
  
  output$dbFilesRecordOut <- renderPrint({print(dbFilesRecordList)})
  
})