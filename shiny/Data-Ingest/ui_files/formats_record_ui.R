box(title = h2("3. Format Record"), width = 3, collapsible = TRUE, collapsed = TRUE,
    hr(),
    selectizeInput("MimetypeName", label = "Mimetype *", choices = NULL,
                   options = list(
                     placeholder = 'Please search inputs by name or site',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    hr(),
    textInput(
      "NewMimeType",
      label = "New Mime Type *",
      placeholder = "Create a New Mimetype"),
    hr(),
    textInput(
      "NewFormatName",
      label = "New Format Name *",
      placeholder = "Create a New Format Name"),
    hr(),
    radioButtons(
      "HeaderBoolean",
      label = "Is There a Header ?", 
      choices =  c("Yes", "No")
    ),
    hr(),
    textInput( # I should Render UI only if Header = TRUE
      "SkipLines",
      label = "Skip",
      placeholder = "Enter number of header lines to skip."),
    hr(),
    textAreaInput(
      "FormatNotes",
      label = "Notes",
      height = '150px'
    ),
    actionButton("createFormatRecord", label = "Create Format Record"),
    p("* Denotes a Required Field"),
    hr(),
    verbatimTextOutput("FormatRecordOut")
)


