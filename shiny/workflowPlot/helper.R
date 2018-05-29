# Helper function which checks and downloads required packages
checkAndDownload<-function(packageNames) {
  for(packageName in packageNames) {
    if(!isInstalled(packageName)) {
      install.packages(packageName,repos="http://lib.stat.cmu.edu/R/CRAN") 
    } 
    library(packageName,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
isInstalled <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}
# checkAndDownload(c('plotly','scales','dplyr'))

# Stashing Code for file upload to shiny app 
# Based on https://shiny.rstudio.com/gallery/file-upload.html

# ui.R 
# tags$hr(),
# fileInput('file1', 'Choose CSV File to upload data',
#           accept=c('text/csv', 
#                    'text/comma-separated-values,text/plain', 
#                    '.csv')),
# checkboxInput('header', 'Header', TRUE),
# radioButtons('sep', 'Separator',
#              c(Comma=',',
#                Semicolon=';',
#                Tab='\t'),
#              ','),
# radioButtons('quote', 'Quote',
#              c(None='',
#                'Double Quote'='"',
#                'Single Quote'="'"),
#              ''),
# textInput("inputRecordID", "Input Record ID for file", "1000011260"),
# textInput("formatID", "Format ID for file (Default CSV)", "5000000002"),
# actionButton("load_data", "Load External Data")

# server.R 
# loadExternalData <-eventReactive(input$load_data,{
#   inFile <- input$file1
#   if (is.null(inFile))
#     return(data.frame())
#  # output$info1 <- renderText({
#  #   # paste0(nrow(externalData))
#  #   paste0(inFile$datapath)
#  # })
#   externalData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
#                            quote=input$quote)
#   return(externalData)
# })  
