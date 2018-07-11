#### UI #####
formatsRecordUI <- function(id){
  ns <- NS(id)
  
  shinyjs::hidden(
    div(id = "formatbox",
      box(title = h2("3. Create New Format"),
          width = 4, collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status = "warning",
          selectizeInput(ns("MimetypeName"), label = "Mimetype *", choices = NULL,
                         options = list(
                           placeholder = 'Please search inputs by name or site',
                           onInitialize = I('function() { this.setValue(""); }')
                         )),
          p("or"),
          # uiOutput(ns("tab")), ## Link to BETYdb
          a(id = ns("betyURL"), "Create New Mimetype", href = "https://www.betydb.org/formats/new", target = "_blank"),
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
    )
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
    # FormatRecordList$NewMimeType <- input$NewMimeType
    FormatRecordList$NewFormatName <- input$NewFormatName
    FormatRecordList$HeaderBoolean <- input$HeaderBoolean
    FormatRecordList$SkipLines <- input$SkipLines #This should appear only if header = TRUE
    FormatRecordList$FormatNotes <- input$FormatNotes
    
    output$FormatRecordOut <- renderPrint({print(FormatRecordList)})
    
    # url <- a("Google Homepage", href="https://www.google.com/")
    # output$tab <- renderUI({
    #   tagList("URL link:", url)
    # })
    
   # shinyjs::onclick("betyURL", shinyjs::
    
  })
  

}