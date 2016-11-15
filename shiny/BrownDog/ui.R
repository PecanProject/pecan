library(shiny)
library(leaflet)

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Generate xml file"),
  fluidRow(
    leafletMap(
      "map", "100%", 400,
      initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
      options=list(
        center = c(37.45, -93.85),
        zoom = 4,
        maxBounds = list(list(17, -180), list(59, 180))))),

  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Type", c("Ameriflux", "NARR")),
      textInput("start_date", "Start year", placehold="2001"),
      textInput("end_date", "End year", placehold="2001")
    ),
    mainPanel(
      verbatimTextOutput("xmlexample"),
      downloadButton('downloadData', 'Download')
    )
  )
 )
)
