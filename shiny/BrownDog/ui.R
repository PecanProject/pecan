library(shiny)
library(leaflet)

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Generate xml file"),
  
  sidebarLayout(
    sidebarPanel(
    uiOutput("typeSelector"),      
    uiOutput("agreementUI"),
      textInput("start_date", "Start year", placehold="2001"),
      textInput("end_date", "End year", placehold="2001"),
      textInput("token", "BrownDog Token"),
      conditionalPanel(
        condition = "input.token != ''", uiOutput("modelSelector")
      )
    ),
    mainPanel(
      fluidRow(
        leafletMap(
          "map", "100%", 400,
          initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
          options=list(
            center = c(37.45, -93.85),
            zoom = 4,
            maxBounds = list(list(17, -180), list(59, 180)))
        ),
        style='padding:10px;'
      ),
      verbatimTextOutput("xmltext"),
      fluidRow(
        column(3,
               conditionalPanel(
                 condition = "input.agreement == true",
                 downloadButton('downloadXML', 'Download XML')
               )),
        column(9,
               conditionalPanel(
                 condition = "input.token != '' && input.agreement == true",
                 downloadButton("downloadData", "Download Data")
               )
        )
      )
    )
  )
))

