fluidPage(
fluidRow(
## 1. D1 Download or Local Upload
box(
  title = h3("1. Select Files"), width = 4, solidHeader = TRUE, status = "success", collapsible = TRUE,
  shinyWidgets::radioGroupButtons("inputMethod", label = "Select Input Method", 
                                 choices = c("DataONE", "Local Files"), status = "success", selected = NULL),
  shinyjs::hidden(source_ui("d1_download_ui.R")),
  shinyjs::hidden(source_ui("local_file_upload_ui.R"))
),

### 2. Inputs
box(
  title = h2("2. Input Record"), width = 8, collapsible = TRUE, solidHeader = TRUE, status = "success",
  fluidRow(column(6,
  selectizeInput("InputSiteName", label = "Site *", choices = NULL,
                 options = list(
                   placeholder = 'Please search or select a site below',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
  ),
  column(6,
  selectizeInput("InputParentName", label = "Parent *", choices = NULL,
                 options = list(
                   placeholder = 'Please search inputs by name or site',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
  )),
#  hr(),
  textInput("InputName",
            label = "Name *",
            placeholder = ""),
  #verbatimTextOutput("autoname"),
  hr(),
  fluidRow(column(6,
  selectizeInput("InputFormatName", label = "Format *", choices = NULL,
                 options = list(
                   placeholder = 'Please search Formats by name',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
  ),
  column(6,
  selectizeInput("MimetypeNameCurrent", label = "Corresponding Mimetype *", choices = NULL,
               options = list(
                 placeholder = 'Please search mimetypes by name',
                 onInitialize = I('function() { this.setValue(""); }')
               )
  )
  )
  ),
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
        height = '75px'
      ),
      actionButton("createFormatRecord", label = "Create Format Record"),
      p("* Denotes a Required Field"),
      hr(),
      verbatimTextOutput("FormatRecordOut")
    )
    ),
  hr(),
fluidRow(column(6,
  dateInput(
    "InputStartDate",
    label = "Start Date",
    format = "yyyy-mm-dd",
    startview = "decade"
  )
  ),
  column(3,
  shinyTime::timeInput("StartTimeInput",
                       label = "Start Time (HH-MM)",
                       seconds = FALSE)
  ),
  column(3,
  textInput("Timezone",
            label = "Timezone",
            placeholder = "")
  )
  ),
fluidRow(column(6,
  dateInput(
    'InputEndDate',
    label = 'End Date',
    format = 'yyyy-mm-dd',
    startview = 'decade'
  )
  ),
  column(3,
  shinyTime::timeInput("EndTimeInput",
                       label = "End Time (HH-MM)",
                       seconds = FALSE)
  ),
column(3)), # Empty Space
  hr(),
  textAreaInput("InputNotes",
                label = "Notes",
                height = '50px'),
  actionButton("createInput", label = "Create Input"),
  actionButton("testBety", label = "Test Bety"),
  p("* Denotes a Required Field"),
  hr(),
  verbatimTextOutput("summInputs"),
  verbatimTextOutput("input_record_df")
  ),
## 4. Formats-Variables
  box(title = h2("3. Formats-Variables"), width = 4, solidHeader = TRUE, status = "success", collapsible = TRUE, collapsed = FALSE,
      div(id = "add_var_action_button", 
          actionButton("add_variable", label = "Add New Variable")
          ),
      shinyjs::hidden(
        div(id = "formats_vars_inputs",
        selectizeInput("pecan_var", choices = NULL, label = "Variable",
                       options = list(
                         placeholder = 'Please search or select a site below',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        ),
        splitLayout(
          textInput("var_name", label = "Name"),
          textInput("var_unit", label = "Unit")
        ),
        splitLayout(
          textInput("storage_type", label = "Storage Type"),
          textInput("col_num", label = "Column Number"),
          actionButton("register_variable", label = "Add Variable")
        )
      )
    ),
    DT::DTOutput("format_vars_df")
  )
  #formats.varsUI("format_vars_mod")
)#End Fluid Row
)# End Fluid Page