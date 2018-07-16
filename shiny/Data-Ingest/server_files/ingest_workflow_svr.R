#### Conditional Pannel to Switch between d1 and local upload ####
observeEvent(input$d1Input,{
  show("d1_ui")
  hide("lcl_ui")
})

observeEvent(input$lclUpload, {
  show("lcl_ui")
  hide("d1_ui")
})

output$d1_import_ui <- renderUI({
  source("ui_files/d1_download_ui.R", local = TRUE)
})

output$lcl_import_ui <- renderUI({
  source("ui_files/local_file_upload_ui.R", local = TRUE)
})

########### Inputs svr ############# 

## List of outputs##
inputsList <- list()

######### Select Site ###############
updateSelectizeInput(session, "InputSiteName",  choices = sitenames, server = TRUE)

######### Select Parent ID #################
updateSelectizeInput(session, "InputParentName",  choices = input_names, server = TRUE)

####### Select Format ##############
updateSelectizeInput(session, "InputFormatName", choices = formats, server = TRUE)

####### Print all selections for Testing ##########

observeEvent(input$createInput, {
  ## siteID
  inputsList$siteName <- input$InputSiteName
  inputsList$siteID <- sites %>% dplyr::filter(sitename %in% inputsList$siteName) %>% pull(id)
  
  ## ParentID
  inputsList$parentName <- input$InputParentName
  inputsList$parentID <- inputs %>% dplyr::filter(name %in% inputsList$parentName) %>% pull(id)
  
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


######### Formats Svr #############

## Output List ##
FormatRecordList <- list()

output$autoname <- renderPrint({Shared.data$selected_row_local})

######### Mimetype Name ##################
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


