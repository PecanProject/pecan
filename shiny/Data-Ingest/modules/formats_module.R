#### UI #####
formatsRecordUI <- function(id){
  ns <- NS(id)
  
  box(title = h2("3. Format Record"), width = 3, collapsible = TRUE, collapsed = TRUE,
      hr(),
      selectizeInput(ns("MimetypeName"), label = "Mimetype *", choices = NULL,
                     options = list(
                       placeholder = 'Please search inputs by name or site',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      hr(),
      textInput(
        ns("NewMimeType"),
        label = "New Mime Type *",
        placeholder = "Create a New Mimetype"),
      hr(),
      textInput(
        ns("NewFormatName"),
        label = "New Format Name *",
        placeholder = "Create a New Format Name"),
      hr(),
      radioButtons(
        ns("HeaderBoolean"),
        label = "Is There a Header ?", 
        choices =  c("Yes", "No")
      ),
      hr(),
      textInput( # I should Render UI only if Header = TRUE
        ns("SkipLines"),
        label = "Skip",
        placeholder = "Enter number of header lines to skip."),
      hr(),
      textAreaInput(
        ns("FormatNotes"),
        label = "Notes",
        height = '150px'
      ),
      actionButton(ns("createFormatRecord"), label = "Create Format Record"),
      p("* Denotes a Required Field"),
      hr(),
      verbatimTextOutput(ns("FormatRecordOut"))
  )
  
}

#### Server #####
formatsRecord <- function(input, output, session){
  
  ## Output List ##
  FormatRecordList <- list()
  
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
  
  
  
}