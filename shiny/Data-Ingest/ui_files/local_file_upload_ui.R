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
     # verbatimTextOutput("test"),
      hr(),
      textInput(
        "new_local_filename",
        label = "Set Destination Directory (for testing only)",
        placeholder = "Enter New Directory Name Here"
      ),
      actionButton(inputId = "LocalFinishButton", label = "Finish Download"),
     fluidRow(
       column(8),
       column(4,
              div(id = "nextFromLocal_div",
                  actionBttn(inputId = "nextFromLocal", 
                             label = "Next Step", 
                             size = "sm", 
                             color = "success")
              )
       )
     ),
      hr(),
      p("Location of Downloaded Files: (Testing Only)"),
      verbatimTextOutput("LocaldbfilesPath")
    )
  )