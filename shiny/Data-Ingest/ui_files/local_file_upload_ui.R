source("ui_utils.R", local = TRUE)
fluidRow(
  box(width = 3,
    # https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
    fileInput(
      inputId = "file",
      label = h2("Upload Local Files"), 
      accept = NULL,
      multiple = FALSE,
      placeholder = "Drag and drop files here"
    ),
    tableOutput("contents"),
#   verbatimTextOutput("test"),
    hr(),
    textInput(
      "new_local_filename",
      label = h4("Set Destination Directory"),
      placeholder = "Enter New Directory Name Here"
    ),
    actionButton(inputId = "LocalFinishButton", label = "Finish Download"),
    hr(),
    p("Location of Downloaded Files:"),
    verbatimTextOutput("LocaldbfilesPath")
    
  ),
  ## Call Input Record UI
  source(file.path("ui_files", "create_input_record_ui.R"), local = TRUE)$value,
  ## Call dbfiles UI
  source(file.path("ui_files", "dbfiles_record_ui.R"), local = TRUE)$value,
  # source_ui("dbfiles_record_ui.R"),
  ## Call Formats Record UI
  source(file.path("ui_files", "formats_record_ui.R"), local = TRUE)$value
  # source_ui("formats_record_ui.R")
)