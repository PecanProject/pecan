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

observeEvent(input$nextFromInput, {
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
  }else{
    inputsList$formatName <<- input$InputFormatName
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
  #output$summInputs <- renderPrint({inputsList})
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
#  output$FormatRecordOut <- renderPrint({print(FormatRecordList)})

})

# ## Insert Format Record
# PEcAn.DB::insert.format.vars(con = bety$con, formats_df = FormatsRecord_df, formats_variables_df = NULL)

###### Formats Vars Server ##############
##Output list 
FormatVars <- list()

## Variable Name ##
updateSelectizeInput(session = getDefaultReactiveDomain(), "pecan_var", choices = variables, server = TRUE)

#### Show inputs on click only ####
observeEvent(input$nextFromInput,{
    show("formats.vars_box")
    if(input$inputMethod == "DataONE"){
      show("finishButton_d1")
      }else{
        show("finishButton_lcl")
      }
})

### Create empty matrix with headers to store infinite entries ###
Shared.data$format_vars_df <- matrix(data = NA, nrow = 0, ncol = 6, dimnames = list(c(), c("BETY_variable_name", "variable_id", "name", "unit", "storage_type", "column_number")))

### Store inputs in a data.frame ###
observeEvent(input$register_variable, {
format_vars_entry <- tibble::tibble(
                                var_name = input$pecan_var, 
                                variable_id = variables_ids %>% dplyr::filter(name %in% input$pecan_var) %>% pull(id),
                                name = input$var_name, 
                                unit = input$var_unit, 
                                storage_type = input$storage_type,
                                column_number = as.numeric(input$col_num)
                              )

Shared.data$format_vars_df <- rbind(format_vars_entry, Shared.data$format_vars_df)
output$format_vars_df <- DT::renderDT(datatable({Shared.data$format_vars_df}, escape = FALSE, selection = 'single', options = list(ordering = F, dom = 'tp')))
})


observeEvent(input$complete_ingest_d1, {
  # Drop var_name column from format_vars_df
  Shared.data$format_vars_df <- Shared.data$format_vars_df %>% select(-one_of("var_name"))
  
  # 1. Create Format and the format variable records
  tryCatch({
  PEcAn.DB::insert.format.vars(con = bety$con, 
                               format_name = input$NewFormatName, 
                               mimetype_id = ifelse((input$MimetypeName == ""), "", mimetype_sub %>% dplyr::filter(type_string %in% input$MimetypeName) %>% pull(id)),
                               header = ifelse((input$HeaderBoolean == "Yes"), TRUE, FALSE),
                               skip = input$SkipLines,
                               notes = input$FormatNotes,
                               formats_variables = Shared.data$format_vars_df
                               )
    toastr_success("Successfully Created Format Record")
  },
  error = function(e){
    toastr_error(title = "Error in Creating Format & Format Variable Record", conditionMessage(e))
  },
  warning = function(e){
    toastr_warning(title = "Format & Format-Variable Warning", conditionMessage(e))
  }
  )
  tryCatch({
    #2. Create the Inputs Record and dbfiles record
    Shared.data$input_record_df <- PEcAn.DB::dbfile.input.insert(in.path = inputsList$Path,
                                                                 in.prefix = inputsList$Name,
                                                                 siteid =   inputsList$siteID,
                                                                 startdate = inputsList$StartDateTime,
                                                                 enddate =   inputsList$EndDateTime,
                                                                 mimetype = inputsList$Mimetype,
                                                                 formatname = inputsList$formatName,
                                                                 parentid = inputsList$parentID,
                                                                 con = bety$con
                                                                 #hostname = localhost #?, #default to localhost for now
                                                                 #allow.conflicting.dates#? #default to FALSE for now
    )
    toastr_success("Successfully Created Input Record")
  },
  error = function(e){
    toastr_error(title = "Error in Completing Input Record", conditionMessage(e))
  },
  warning = function(e){
    toastr_warning(title = "Input Record Warning", conditionMessage(e))
  }
)

})

observeEvent(input$complete_ingest_lcl, {
  # Drop var_name column from format_vars_df
  Shared.data$format_vars_df <- Shared.data$format_vars_df %>% select(-one_of("var_name"))
  # 1. Create Format and the format variable records
  tryCatch({
    PEcAn.DB::insert.format.vars(con = bety$con,
                                 format_name = input$NewFormatName,
                                 mimetype_id = ifelse((input$MimetypeName == ""), "", mimetype_sub %>% dplyr::filter(type_string %in% input$MimetypeName) %>% pull(id)),
                                 header = ifelse((input$HeaderBoolean == "Yes"), TRUE, FALSE),
                                 skip = input$SkipLines,
                                 notes = input$FormatNotes,
                                 formats_variables = Shared.data$format_vars_df
    )
    toastr_success("Successfully Created Format Record")
  },
  error = function(e){
    toastr_error(title = "Error in Creating Format & Format-Variable Record", conditionMessage(e))
  },
  warning = function(e){
    toastr_warning(title = "Format & Format-Variable Record Warning", conditionMessage(e))
  }
  )
  tryCatch({
    #2. Create the Inputs Record and dbfiles record
    Shared.data$input_record_df <- PEcAn.DB::dbfile.input.insert(in.path = inputsList$Path,
                                                                 in.prefix = inputsList$Name,
                                                                 siteid =   inputsList$siteID,
                                                                 startdate = inputsList$StartDateTime,
                                                                 enddate =   inputsList$EndDateTime,
                                                                 mimetype = inputsList$Mimetype,
                                                                 formatname = inputsList$formatName,
                                                                 parentid = inputsList$parentID,
                                                                 con = bety$con
                                                                 #hostname = localhost #?, #default to localhost for now
                                                                 #allow.conflicting.dates#? #default to FALSE for now
    )
    toastr_success("Successfully Created Input Record")
  },
  error = function(e){
    toastr_error(title = "Error in Creating Input Record", conditionMessage(e))
  },
  warning = function(e){
    toastr_warning(title = "Input Record Warning", conditionMessage(e))
  }
  )
})
