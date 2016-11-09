library(shiny)
library(leaflet)

latitude<-c(35.94077, 35.83770, 35.84545, 35.81584, 35.79387, 36.05600)
longitude<-c(-78.58010, -78.78084, -78.72444, -78.62568, -78.64262, -78.67600)

ids<-c("a", "b", "c", "d", "e", "f")

# Define server logic
server <- shinyServer(function(input, output, session) {

#  start <- reactive(input$start_date)
#  end <- reactive(input$end_date)
#  gsub("", "2001", input$start_date)
#  gsub(input$end_date, "", "2001")
#  start_date <- input$start_date
#  if (input$start_date == "") start_date <- "2001"
  map = createLeafletMap(session, 'map')
  session$onFlushed(once=T, function(){
    
    map$addMarker(lat = latitude, lng = longitude, 
                        layerId=ids)
  })        
  
  observe({
    click<-input$map_marker_click
    if(is.null(click))
      return()
    text<-paste(click$id)
    text2<-paste("You've selected point ", click$id)
    map$clearPopups()
    map$showPopup( click$lat, click$lng, text)
    output$xmlexample <- renderText({
      paste(c("<input>", paste0("  <type>", input$type, "</type>"), 
              paste0("  <site>", click$id, "</site>"),
              paste0("  <lat>", click$lat, "</lat>"),
              paste0("  <lon>", click$lng, "</lon>"),
              paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
              paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
              "</input>"), collapse="\n")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { "example.xml" },
      content = function(file) {
        writeLines(c("<input>", paste0("  <type>", input$type, "</type>"), 
                     paste0("  <site>", click$id, "</site>"),
                     paste0("  <lat>", click$lat, "</lat>"),
                     paste0("  <lon>", click$lng, "</lon>"),
                     paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
                     paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
                     "</input>"), file)
      }
    )
    
  })
  # default site 
  output$xmlexample <- renderText({
    paste(c("<input>", paste0("  <type>", input$type, "</type>"), 
            "  <site>US-Dk3</site>",
            "  <lat>35.9782</lat>",
            "  <lon>-79.0942</lon>",
                 paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
                 paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
                 "</input>"), collapse="\n")
  })

  
  output$downloadData <- downloadHandler(
    filename = function() { "example.xml" },
    content = function(file) {
      writeLines(c("<input>", paste0("  <type>", input$type, "</type>"), 
                   "  <site>US-Dk3</site>",
                   "  <lat>35.9782</lat>",
                   "  <lon>-79.0942</lon>",
        paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
        paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
        "</input>"), file)
    }
  )
})

# runApp(port=????, launch.browser=FALSE)
