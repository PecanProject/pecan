inputsList <- list()
bety <- betyConnect()

######### Select Site ###############
  sites <- dplyr::tbl(bety, "sites") %>% 
    dplyr::select(one_of("sitename", "id")) %>% collect()
  sitenames <- sites$sitename ## Is this a redundant step?
  updateSelectizeInput(session, "InputSiteID", choices = sort(unique(sitenames)))
  
######### Select Parent ID #################

updateSelectizeInput(session, "InputParentID", choices = sort(unique(sitenames)))


####### Select Format ##############
  formats <- dplyr::tbl(bety, "formats") %>%
              dplyr::select(one_of("name", "id", "mimetype_id")) %>% collect()
  selectformat <- formats$name
  updateSelectizeInput(session, "InputFormatID", choices = sort(unique(selectformat)))


####### Print InputsList for Testing ##########

observeEvent(input$createInput, ignoreInit = TRUE, {
  inputsList$siteID <- input$InputSiteID
  inputsList$parentID <- input$InputParentID
  inputsList$FormatID <- input$InputFormatID
  inputsList$Name <- input$InputName
  inputsList$StartDate <- input$InputStartDate
  inputsList$StartTime <- input$StartTimeInput
  inputsList$EndDate <- input$InputEndDate
  inputsList$EndTime <- input$EndTimeInput
  inputsList$Timezone <- input$Timezone
  inputsList$Notes <- input$InputNotes
  print(inputsList)
  
  output$summInputs <- renderPrint({inputsList})
})

  