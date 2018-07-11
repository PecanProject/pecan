source("ui_utils.R", local = TRUE)
fluidRow(
  box(width = 4, title = h2("Upload Local Files"), solidHeader = T, status = "success",
    # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
    fileInput(
      inputId = "file",
      label = "Upload Local Files", 
      accept = NULL,
      multiple = FALSE,
      placeholder = "Drag and drop files here"
    ),
    tableOutput("contents"),
#   verbatimTextOutput("test"),
    hr(),
    textInput(
      "new_local_filename",
      label = "Set Destination Directory (for testing only)",
      placeholder = "Enter New Directory Name Here"
    ),
    actionButton(inputId = "LocalFinishButton", label = "Finish Download"),
    hr(),
    p("Location of Downloaded Files: (Testing Only)"),
    verbatimTextOutput("LocaldbfilesPath")
  ),
  ## Call Input Record UI
  inputsRecordUI("local_inputs_record"),
  ## Call dbfiles UI
#  dbfilesUI("local_dbfiles"),
  ## Call Formats Record UI
  formatsRecordUI("local_formats_record")
)