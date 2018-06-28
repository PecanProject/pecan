inputsList <- list()
######### Select Site ###############
updateSelectizeInput(session, "InputSiteID",  choices = sitenames)

######### Select Parent ID #################

####### Select Format ##############
updateSelectizeInput(session, "InputFormatID", choices = formats)

####### Print all selections for Testing ##########

observeEvent(input$createInput, {
  inputsList$SiteID <- input$InputSiteID
  inputsList$ParentID <- input$InputParentID
  inputsList$FormatID <- input$InputFormatID
  inputsList$Name <- input$InputName
  inputsList$StartDate <- input$InputStartDate
  inputsList$StartTime <- input$StartTimeInput
  inputsList$EndDate <- input$InputEndDate
  inputsList$EndTime <- input$EndTimeInput
  inputsList$Timezone <- input$Timezone
  inputsList$Notes <- input$InputNotes

 output$summInputs <- renderPrint({print(inputsList)})
})
