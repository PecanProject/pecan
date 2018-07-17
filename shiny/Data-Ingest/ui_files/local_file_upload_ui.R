div(id = "lcl_ui",
  tagList(
      fileInput(
        inputId = "file",
        label = "Upload Local Files", 
        accept = NULL,
        multiple = TRUE,
        placeholder = "Drag and drop files here"
      ),
      DT::DTOutput("dtfiles"),
      verbatimTextOutput("test"),
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
    )
  )