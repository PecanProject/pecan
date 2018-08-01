#### Conditional Pannel to Switch between d1 and local upload ####
observeEvent(input$inputMethod,{
  if(input$inputMethod == "DataONE"){
    show("d1_ui")
    hide("lcl_ui")
  }else{
    show("lcl_ui")
    hide("d1_ui")
  }
})

observeEvent(input$lclUpload, {
  show("lcl_ui")
  hide("d1_ui")
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

observeEvent(input$createFormatRecord, {
  new_format <- input$NewFormatName
  updateSelectizeInput(session, "InputFormatName", choices = new_format, selected = tail(new_format,1), server = TRUE)
  
})

###### Select Mimetype #########
updateSelectizeInput(session, "MimetypeNameCurrent", choices = mimetypes, server = TRUE)

observeEvent(input$createFormatRecord, {
  updateSelectizeInput(session, "MimetypeNameCurrent", choices = input$MimetypeName, selected = input$MimetypeName, server = TRUE)
  
})

####### Update Text Input for fileName ######
observe({
updateTextInput(session, "InputName", value = Shared.data$selected_row)
})

####### Make Inputs List ##########

observeEvent(input$createInput, {
  ## siteID
  if(input$InputSiteName == ""){
    inputsList$siteName <<- ""
    inputsList$siteID <<- ""
  }else{
    inputsList$siteName <<- input$InputSiteName
    inputsList$siteID <<- sites %>% dplyr::filter(sitename %in% input$InputSiteName) %>% pull(id)
  }
    
  ## ParentID
  if(input$InputParentName == ""){
    inputsList$parentName <<- ""
    inputsList$parentID <<- NA
  }else{
    inputsList$parentName <<- input$InputParentName
    inputsList$parentID <<- inputs %>% dplyr::filter(name %in% input$InputParentName) %>% pull(id)
  }

  ## FormatID
  if(input$InputFormatName == ""){
    inputsList$formatName <<- ""
    inputsList$formatID <<- ""
  }else{
    inputsList$formatName <<- input$InputFormatName
   # inputsList$formatID <<- formats_sub %>% dplyr::filter(name %in% input$InputFormatName) %>% pull(id) IF Format ID is necessary, I need to redesign this line. 
  }
  
  ## Mimetype (should I find the ID as well?)##
  inputsList$Mimetype <<- input$MimetypeNameCurrent
  

  ## Other Info
  inputsList$Method <<- input$inputMethod
  inputsList$Name <<- input$InputName
  inputsList$Path <<- ifelse(inputsList$Method == "DataONE", file.path(newdir_D1, input$InputName), file.path(local_tempdir, input$InputName))
  inputsList$StartDate <<- input$InputStartDate
  inputsList$StartTime <<- input$StartTimeInput
  inputsList$EndDate <<- input$InputEndDate
  inputsList$EndTime <<- input$EndTimeInput
  inputsList$Timezone <<- input$Timezone
  inputsList$Notes <<- input$InputNotes
  
  ## Print List
  output$summInputs <- renderPrint({inputsList})
})


observeEvent(input$testBety, {
  Shared.data$input_record_df <- PEcAn.DB::dbfile.input.insert(in.path = inputsList$Path,
                                                                in.prefix = inputsList$Name,
                                                                siteid =   inputsList$siteID,
                                                                startdate = inputsList$StartDate,
                                                                enddate =   inputsList$EndDate,
                                                                mimetype = inputsList$Mimetype,
                                                                formatname = inputsList$formatName,
                                                                # parentid = inputsList$parentID,
                                                                con = bety$con
                                                                #hostname = localhost #?,
                                                                #allow.conflicting.dates#?
  )
  
  inputs_updated <<- dplyr::tbl(bety, "inputs") %>% collect()
  
})
output$input_record_df <- renderPrint({Shared.data$input_record_df})


######### Formats Svr #############
output$autoname <- renderPrint({Shared.data$selected_row}) #_local

######### Mimetype Name ##################
updateSelectizeInput(session, "MimetypeName", choices = mimetypes, server = TRUE)

observeEvent(input$createFormatRecord, {
  ## Output List ##
  FormatRecordList <<- list()
  
  ## MimetypeID
  FormatRecordList$MimetypeName <- input$MimetypeName
  FormatRecordList$NewmimetypeID <- ifelse((input$MimetypeName == ""), "", mimetype_sub %>% dplyr::filter(type_string %in% input$MimetypeName) %>% pull(id))
  
  ## Everything else
  FormatRecordList$NewFormatName <- input$NewFormatName
  FormatRecordList$HeaderBoolean <- ifelse((input$HeaderBoolean == "Yes"), "TRUE", "FALSE")
  FormatRecordList$SkipLines <- input$SkipLines #This should appear only if header = TRUE
  FormatRecordList$FormatNotes <- input$FormatNotes
  
  ## Print format record for testing
  output$FormatRecordOut <- renderPrint({print(FormatRecordList)})

  ## Insert Format Record
  PEcAn.DB::insert.format.vars(con = bety$con, 
                               header = FormatRecordList$HeaderBoolean, 
                               skip = FormatRecordList$SkipLines, 
                               mimetype_id = FormatRecordList$NewmimetypeID, 
                               format_name = FormatRecordList$NewFormatName,
                               format_notes = FormatRecordList$FormatNotes, 
                               formats_variables_df = NULL)
})


