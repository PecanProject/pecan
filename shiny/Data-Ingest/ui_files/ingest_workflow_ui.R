fluidRow(
## 1. D1 Download or Local Upload
box(
  title = h2("1. Select Files"), width = 4, solidHeader = TRUE, status = "success",
  shinyWidgets::radioGroupButtons("inputMethod", label = "Select Input Method", 
                                 choices = c("DataONE", "Local Files"), status = "success", selected = NULL),
  shinyjs::hidden(source_ui("ui_files", "d1_download_ui.R")),
  shinyjs::hidden(source_ui("ui_files", "local_file_upload_ui.R"))
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
#  hr(),
  selectizeInput("InputParentName", label = "Parent *", choices = NULL,
                 options = list(
                   placeholder = 'Please search inputs by name or site',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
#  hr(),
  textInput("InputName",
            label = "Name *",
            placeholder = ""),
  #verbatimTextOutput("autoname"),
  hr(),
  selectizeInput("InputFormatName", label = "Format *", choices = NULL,
                 options = list(
                   placeholder = 'Please search Formats by name',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
  selectizeInput("MimetypeNameCurrent", label = "Corresponding Mimetype *", choices = NULL,
               options = list(
                 placeholder = 'Please search mimetypes by name',
                 onInitialize = I('function() { this.setValue(""); }')
               )),
  p("or"),
  shinyWidgets::dropdownButton(circle = FALSE, label = "Create New Format", width = '350px',
      box(width = '350px', solidHeader = TRUE, status = "warning",
        selectizeInput("MimetypeName", label = "Mimetype *", choices = NULL,
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
  actionButton("testBety", label = "Test Bety"),
  p("* Denotes a Required Field"),
  hr(),
  verbatimTextOutput("summInputs"),
  verbatimTextOutput("input_record_df")
  )
)
## 4. Formats-Variables