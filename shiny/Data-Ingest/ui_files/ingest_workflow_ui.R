fluidPage(
fluidRow(
## 1. D1 Download or Local Upload
box(
  title = h2("1. Select Files"), width = 4, solidHeader = TRUE, status = "success", collapsible = TRUE,
  shinyWidgets::radioGroupButtons("inputMethod", label = "Select Input Method", 
                                 choices = c("DataONE", "Local Files"), status = "success", selected = NULL),
  shinyjs::hidden(source_ui("d1_download_ui.R")),
  shinyjs::hidden(source_ui("local_file_upload_ui.R"))
),

### 2. Inputs
shinyjs::hidden(
  div(id = "input_record_box",
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
  textInput("InputName",
            label = "Name *",
            placeholder = ""),
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
                       placeholder = 'Please search formats by name or site',
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
      actionBttn("FormatRecordDone", label = "Done", color = "warning", size = "sm"),
      p("* Denotes a Required Field")
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
  column(3 ## Now I'm not sure if we need timezone since sites are known
   # selectizeInput("Timezone", label = "Timezone *", choices = NULL,
   #                options = list(
   #                  placeholder = 'Please search inputs by name or site',
   #                  onInitialize = I('function() { this.setValue(""); }')
   #                ))
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
fluidRow(
  column(10),
  column(2,
         actionBttn("nextFromInput", 
                    label = "Next Step", 
                    color = "success", 
                    size = "sm")
  )
),
  p("* Denotes a Required Field")
  )
)
)
),#End Fluid Row
fluidRow(
  ## 4. Formats-Variables
shinyjs::hidden(  
div(id = "formats.vars_box",  
  box(title = h2("3. Formats-Variables"), width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE, collapsed = FALSE,
      fluidRow(column(3,
                      selectizeInput("pecan_var", choices = NULL, label = "Variable",
                                     options = list(
                                       placeholder = 'Please search for a variable in PEcAn',
                                       onInitialize = I('function() { this.setValue(""); }')
                                     )
                      )
      ),
      column(3,
             textInput("var_name", label = "Name")
      ),
      column(2,
             textInput("var_unit", label = "Unit")
      ),
      column(2,
             textInput("storage_type", label = "Storage Type", placeholder = "e.g. POSIX code")
      ),
      column(2,
             textInput("col_num", label = "Column Number")
      )
      ),
      actionButton("register_variable", label = "Add Variable"),
      DT::DTOutput("format_vars_df")
  )
),
div(id = "finishButton_d1",
  fluidRow(
    column(10),
    column(2,
           actionBttn("complete_ingest_d1", 
                      label = "Complete Ingest of DataONE Files", 
                      color = "success", 
                      size = "md")
    )
  )
),
div(id = "finishButton_lcl",
    fluidRow(
      column(10),
      column(2,
             actionBttn("complete_ingest_lcl", 
                        label = "Complete Ingest of Local Files", 
                        color = "success", 
                        size = "md")
      )
    )
)
)
)
)# End Fluid Page