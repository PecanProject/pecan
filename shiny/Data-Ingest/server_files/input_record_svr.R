inputsList <- list()
dbFilesRecordList <- list()
FormatRecordList <- list()
######### Select Site ###############
updateSelectizeInput(session, "InputSiteName",  choices = sitenames, server = TRUE)

######### Select Parent ID #################
updateSelectizeInput(session, "InputParentName",  choices = sitenames, server = TRUE)

####### Select Format ##############
updateSelectizeInput(session, "InputFormatName", choices = formats, server = TRUE)

####### Print all selections for Testing ##########

observeEvent(input$createInput, {
  ## siteID
  inputsList$siteName <- input$InputSiteName
  inputsList$siteID <- sites_sub %>% dplyr::filter(sitename %in% inputsList$siteName) %>% pull(id)
  
  ## ParentID
  inputsList$parentName <- input$InputParentName
  
  ## FormatID
  inputsList$formatName <- input$InputFormatName
  inputsList$formatID <- formats_sub %>% dplyr::filter(name %in% inputsList$formatName) %>% pull(id)
  
  ## Other Info
  inputsList$Name <- input$InputName
  inputsList$StartDate <- input$InputStartDate
  inputsList$StartTime <- input$StartTimeInput
  inputsList$EndDate <- input$InputEndDate
  inputsList$EndTime <- input$EndTimeInput
  inputsList$Timezone <- input$Timezone
  inputsList$Notes <- input$InputNotes

 output$summInputs <- renderPrint({print(inputsList)})
})

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

###################################################
############# Format ID ###########################
###################################################

######### Mimetype ID ##################
updateSelectizeInput(session, "MimetypeName", choices = mimetypes, server = TRUE)

observeEvent(input$createFormatRecord, {
  ## MimetypeID
  FormatRecordList$mimetypeName <- input$MimetypeName
  FormatRecordList$mimetypeID <- mimetype_sub %>% dplyr::filter(type_string %in% FormatRecordList$mimetypeName) %>% pull(id)
  
  ## Everything else
  FormatRecordList$NewMimeType <- input$NewMimeType
  FormatRecordList$NewFormatName <- input$NewFormatName
  FormatRecordList$HeaderBoolean <- input$HeaderBoolean
  FormatRecordList$SkipLines <- input$SkipLines #This should appear only if header = TRUE
  FormatRecordList$FormatNotes <- input$FormatNotes
  
  output$FormatRecordOut <- renderPrint({print(FormatRecordList)})
  
})

