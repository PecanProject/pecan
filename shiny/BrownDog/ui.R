library(shiny)

# Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Generate xml file"),

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
))
