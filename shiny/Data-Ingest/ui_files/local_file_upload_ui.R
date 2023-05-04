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
     shinyjs::hidden(
       div(id = "local_path_out",
           hr(),
      p("Location of Downloaded Files:"),
      verbatimTextOutput("LocaldbfilesPath")
       )
     )
    )
  )