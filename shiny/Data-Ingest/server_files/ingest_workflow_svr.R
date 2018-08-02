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

####### Update Selections of Format and Corresponding Mimetype ############
observeEvent(input$FormatRecordDone,{
  updateSelectizeInput(session, "InputFormatName", choices = c(input$NewFormatName), selected = c(input$NewFormatName), server = TRUE)
  updateSelectizeInput(session, "MimetypeNameCurrent", choices = c(input$MimetypeName), selected = c(input$MimetypeName), server = TRUE)
})


# ####### Update Selectize Input for Timezone ########### Not sure if Timezone is a necessary input
# updateSelectizeInput(session, "Timezone", choices = timezones, server = TRUE)

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
  
  inputsList$StartTime_sub <<- trimws(base::sub("[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}[ \t]", "", input$StartTimeInput))
  inputsList$EndTime_sub <<- trimws(base::sub("[0-9]{4}[.-][0-9]{2}[.-][0-9]{2}[ \t]", "", input$EndTimeInput))
  
  inputsList$StartDateTime <<- trimws(paste(input$InputStartDate, inputsList$StartTime_sub, " "))
  inputsList$EndDateTime <<- trimws(paste(input$InputEndDate, inputsList$EndTime_sub, " "))
  
  

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
                                                                startdate = inputsList$StartDateTime,
                                                                enddate =   inputsList$EndDateTime,
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

observeEvent(input$FormatRecordDone, {
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
  
  ## Make 'data.frame' for format record query 
  FormatsRecord_df <- data.frame(
    header = FormatRecordList$HeaderBoolean,
    skip = FormatRecordList$SkipLines,
    mimetype_id = FormatRecordList$NewmimetypeID,
    notes = FormatRecordList$FormatNotes,
    name = FormatRecordList$NewFormatName
  )
  ## Print format record for testing
  output$FormatRecordOut <- renderPrint({print(FormatRecordList)})

})

# ## Insert Format Record
# PEcAn.DB::insert.format.vars(con = bety$con, formats_df = FormatsRecord_df, formats_variables_df = NULL)

###### Formats Vars Server ##############
##Output list 
FormatVars <- list()

## Machine Name ##
updateSelectizeInput(session = getDefaultReactiveDomain(), "pecan_var", choices = variables, server = TRUE)

#### Show inputs on click only ####
observeEvent(input$add_variable,{
    show("formats_vars_inputs")
    hide("add_var_action_button")
})

### Create empty matrix with headers to store infinite entries ###
Shared.data$format_vars_df <- matrix(data = NA, nrow = 1, ncol = 5, dimnames = list(c(), c("variable", "name", "unit", "storage_type", "column_number")))

### Store inputs in a data.frame ###
observeEvent(input$register_variable, {
format_vars_entry <- tibble::tibble(
                                variable = input$pecan_var,
                                name = input$var_name, 
                                unit = input$var_unit, 
                                storage_type = input$storage_type,
                                column_number = input$col_num
                              )

Shared.data$format_vars_df <- rbind(format_vars_entry, Shared.data$format_vars_df)
output$format_vars_df <- DT::renderDT(datatable({Shared.data$format_vars_df}, escape = FALSE, selection = 'single', options = list(ordering = F, dom = 'tp')))
})



