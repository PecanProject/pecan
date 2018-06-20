fluidRow(
  box(width = 8,
    textInput(
      "id",
      label = h2("Import From DataONE"),
      placeholder = "Enter doi or id here"
    ),
    p("Copy and Paste the following example data sets:"),
    p("doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87"),
    p("doi:10.6073-pasta-f31b28b912e6051bf1d383ff1ef18987"),
    actionButton(inputId = "D1Button", label = "Download"),
    #  actionButton(inputId = "CancelD1Download", label = "Cancel Download"), This is WAY tricky. Not sure if I can add this functionality... 
    hr(),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(id="loadmessage",
                              HTML(paste0("<div> <h3>Download in Progress.</h3> <p>This download may take a couple of minutes.</p> <img src=\'http://www.lettersmarket.com/uploads/lettersmarket/blog/loaders/common_green/ajax_loader_green_64.gif' height=\"64\" width=\"64\"> </div>"))
                     )), 
    DTOutput("identifier"), 
    actionButton(inputId = "D1FinishButton", label = "Finish Download"),
    hr(),
    p("Location of Downloaded files:"),
    verbatimTextOutput("D1dbfilesPath")
  )
)