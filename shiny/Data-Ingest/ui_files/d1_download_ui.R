div(id = "d1_ui",
tagList(
    textInput(
      "id",
      label = "Import from dataONE",
      placeholder = "Enter doi or id here"
    ),
    actionBttn(inputId = "D1Button", label = "Download", size = "sm", color = "success"),
    #  actionButton(inputId = "CancelD1Download", label = "Cancel Download"), This is WAY tricky. Not sure if I can add this functionality... 
    hr(),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(id="loadmessage",
                              HTML(paste0("<div> <h3>Download in Progress.</h3> <p>This download may take a couple of minutes.</p> <img src=\'http://www.lettersmarket.com/uploads/lettersmarket/blog/loaders/common_green/ajax_loader_green_64.gif' height=\"64\" width=\"64\"> </div>"))
                     )), 
    DT::DTOutput("identifier"),
    # p("Selected Row (For Testing Purposes)"),
    # verbatimTextOutput("rowSelection"), ## For testing only
    div(id = "nextFromD1_div",
    fluidRow(
      column(8),
      column(4,
        actionBttn(inputId = "nextFromD1", label = "Next Step", size = "sm", color = "success")
        )
      )
    ),
    hr(),
    p("Location of Downloaded files:"),
    verbatimTextOutput("D1dbfilesPath")
)
)