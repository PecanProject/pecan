fluidRow(
## 1. D1 Download or Local Upload
box(
  title = h2("1. Select Input Method"), width = 4, solidHeader = TRUE, status = "success",
  # shinyWidgets::radioGroupButtons("inputMethod", label = "Select Input Method", 
  #                                 choices = c("Download from DataONE", "Upload Local File"), status = "success", selected = NULL),
  conditionalPanel(condition = "input.d1Input",
                   uiOutput("d1_import_ui")),
  conditionalPanel(condition = "input.lclUpload",
                   uiOutput("lcl_import_ui"))

),

### 2. Inputs
box(
  title = h2("2. Input Record"), width = 4, collapsible = TRUE, solidHeader = TRUE, status = "success",
  selectizeInput("InputSiteName", label = "Site *", choices = NULL,
                 options = list(
                   placeholder = 'Please search or select a site below',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
  hr(),
  selectizeInput("InputParentName", label = "Parent *", choices = NULL,
                 options = list(
                   placeholder = 'Please search inputs by name or site',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
  hr(),
  textInput("InputName",
            label = "Name *",
            placeholder = ""),
  verbatimTextOutput("autoname"),
  hr(),
  selectizeInput("InputFormatName", label = "Choose Format *", choices = NULL,
                 options = list(
                   placeholder = 'Please search Formats by name',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
  p("or"),
 # actionButton("NewFormat", label = "Create New Format"),
  shinyWidgets::dropdownButton(circle = FALSE, label = "Create New Format", 
      box(width = 48, solidHeader = TRUE, status = "warning",
        selectizeInput("MimetypeName", label = "Mimetype *", choices = NULL, width = '350px',
                     options = list(
                       placeholder = 'Please search inputs by name or site',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      p("or"),
      a(id = "betyURL", "Create New Mimetype", href = "https://www.betydb.org/formats/new", target = "_blank"),
      hr(),
      textInput(
        "NewFormatName",
        label = "New Format Name *",
        placeholder = "Create a New Format Name"),
      radioButtons(
        "HeaderBoolean",
        label = "Is There a Header ?",
        choices =  c("Yes", "No")
      ),
      textInput( # I should Render UI only if Header = TRUE
        "SkipLines",
        label = "Skip",
        placeholder = "Enter number of header lines to skip."),
      textAreaInput(
        "FormatNotes",
        label = "Notes",
        height = '100px'
      ),
      actionButton("createFormatRecord", label = "Create Format Record"),
      p("* Denotes a Required Field"),
      hr(),
      verbatimTextOutput("FormatRecordOut")
    )
    ),
  hr(),
  dateInput(
    "InputStartDate",
    label = "Start Date",
    format = "yyyy-mm-dd",
    startview = "decade"
  ),
  shinyTime::timeInput("StartTimeInput",
                       label = "Start Time (Hours - Minutes)",
                       seconds = FALSE),
  dateInput(
    'InputEndDate',
    label = 'End Date',
    format = 'yyyy-mm-dd',
    startview = 'decade'
  ),
  shinyTime::timeInput("EndTimeInput",
                       label = "End Time (Hours-Minutes)",
                       seconds = FALSE),
  textInput("Timezone",
            label = "Timezone (UTC)",
            placeholder = "UTC +/-"),
  hr(),
  textAreaInput("InputNotes",
                label = "Notes",
                height = '100px'),
  actionButton("createInput", label = "Create Input"),
  p("* Denotes a Required Field"),
  hr(),
  verbatimTextOutput("summInputs")
  )

# 3. Formats (optional)
#shinyjs::hidden(
#  div(id = "formatbox",
      # box(title = h2("3. Create New Format"),
      #     width = 4, collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status = "warning",
      #     selectizeInput("MimetypeName", label = "Mimetype *", choices = NULL,
      #                    options = list(
      #                      placeholder = 'Please search inputs by name or site',
      #                      onInitialize = I('function() { this.setValue(""); }')
      #                    )),
      #     p("or"),
      #     # uiOutput("tab"), ## Link to BETYdb
      #     a(id = "betyURL", "Create New Mimetype", href = "https://www.betydb.org/formats/new", target = "_blank"),
      #     hr(),
      #     textInput(
      #       "NewFormatName",
      #       label = "New Format Name *",
      #       placeholder = "Create a New Format Name"),
      #     hr(),
      #     radioButtons(
      #       "HeaderBoolean",
      #       label = "Is There a Header ?",
      #       choices =  c("Yes", "No")
      #     ),
      #     hr(),
      #     textInput( # I should Render UI only if Header = TRUE
      #       "SkipLines",
      #       label = "Skip",
      #       placeholder = "Enter number of header lines to skip."),
      #     hr(),
      #     textAreaInput(
      #       "FormatNotes",
      #       label = "Notes",
      #       height = '150px'
      #     ),
      #     actionButton("createFormatRecord", label = "Create Format Record"),
      #     p("* Denotes a Required Field"),
      #     hr(),
      #     verbatimTextOutput("FormatRecordOut")
      # )
#  )
#)
)
## 4. Formats-Variables