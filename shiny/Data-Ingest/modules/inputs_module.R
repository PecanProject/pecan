## UI #####
inputsRecordUI <- function(id){
  ns <- NS(id)
  
  box(
    title = h2("2. Input Record"), width = 4, collapsible = TRUE, solidHeader = TRUE, status = "success",
    selectizeInput(ns("InputSiteName"), label = "Site *", choices = NULL,
                   options = list(
                     placeholder = 'Please search or select a site below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    hr(),
    selectizeInput(ns("InputParentName"), label = "Parent *", choices = NULL,
                   options = list(
                     placeholder = 'Please search inputs by name or site',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    hr(),
    textInput(ns("InputName"),
              label = "Name *",
              placeholder = ""),
    verbatimTextOutput(ns("autoname")),
    hr(),
    selectizeInput(ns("InputFormatName"), label = "Choose Format *", choices = NULL,
                   options = list(
                     placeholder = 'Please search Formats by name',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    p("or"),
  #  actionButton("NewFormat", label = "Create New Format"),
    hr(),
    dateInput(
      ns("InputStartDate"),
      label = "Start Date",
      format = "yyyy-mm-dd",
      startview = "decade"
    ),
    shinyTime::timeInput(ns("StartTimeInput"),
                         label = "Start Time (Hours - Minutes)",
                         seconds = FALSE),
    dateInput(
      ns('InputEndDate'),
      label = 'End Date',
      format = 'yyyy-mm-dd',
      startview = 'decade'
    ),
    shinyTime::timeInput(ns("EndTimeInput"),
                         label = "End Time (Hours-Minutes)",
                         seconds = FALSE),
    textInput(ns("Timezone"),
              label = "Timezone (UTC)",
              placeholder = "UTC +/-"),
    hr(),
    textAreaInput(ns("InputNotes"),
                  label = "Notes",
                  height = '150px'),
    actionButton(ns("createInput"), label = "Create Input"),
    p("* Denotes a Required Field"),
    hr(),
    verbatimTextOutput(ns("summInputs"))
  )
}

### Server #####

inputsRecord <- function(input, output, session){
  ## List of outputs
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
#output$autoname <- renderPrint({Shared.data$selected_row_local})

}
