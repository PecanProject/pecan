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
