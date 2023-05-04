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