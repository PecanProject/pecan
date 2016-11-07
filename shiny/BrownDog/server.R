library(shiny)


# Define server logic
server <- shinyServer(function(input, output, session) {

#  start <- reactive(input$start_date)
#  end <- reactive(input$end_date)
#  gsub("", "2001", input$start_date)
#  gsub(input$end_date, "", "2001")
#  start_date <- input$start_date
#  if (input$start_date == "") start_date <- "2001"
  output$xmlexample <- renderText({
    paste0("<input>
  <type>", input$type, "</type>
  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>
  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>
</input>")
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { "example.xml" },
    content = function(file) {
      writeLines(c("<input>", paste0("  <type>", input$type, "</type>"), 
        paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
        paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
        "</input>"), file)
    }
  )
})

# runApp(port=????, launch.browser=FALSE)
