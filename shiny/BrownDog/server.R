library(shiny)
library(leaflet)
library(RPostgreSQL)
library(PEcAn.DB)


# Define server logic
server <- shinyServer(function(input, output, session) {
  output$agreementUI <- renderUI({
    if (is.null(input$type))
      return()
    
    # Depending on input$type, we'll generate a different lincense agreement
    switch(input$type,
           "AmeriFlux" =  checkboxInput("agreement", paste0("I am agree to AmeriFlux license."), value = FALSE, width = NULL),
           "NARR" =  checkboxInput("agreement", paste0("I am agree to NARR license."), value = FALSE, width = NULL),
           "FLUXNET" =  checkboxInput("agreement", paste0("I am agree to FLUXNET license."), value = FALSE, width = NULL)
    )
  })
  
  observeEvent(input$type, {
    #get all sites name, lat and lon by sitegroups
    dbparams <- list(user = "bety", dbname = "bety", password = "bety", host = "localhost")
    con <- db.open(dbparams)
    on.exit(db.close(con))
    sites <- db.query(paste0("SELECT sitename, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry))
                            AS lat FROM sites, sitegroups_sites where sites.id = sitegroups_sites.site_id
                            and sitegroups_sites.sitegroup_id in ( select id from sitegroups  where
                            sitegroups.name like '", input$type, "');"),con)
    
    ids<-sites$sitename
    latitude<-sites$lat
    longitude<-sites$lon
    
    # fake sites to run on local (without DB)
    # latitude<-c(35.94077, 35.83770, 35.84545, 35.81584, 35.79387, 36.05600)
    # longitude<-c(-78.58010, -78.78084, -78.72444, -78.62568, -78.64262, -78.67600)
    # ids<-c("a", "b", "c", "d", "e", "f")
    
    #add markers on the map
    map = createLeafletMap(session, 'map')
    session$onFlushed(once=TRUE, function(){
      
      map$addMarker(lat = latitude, lng = longitude, 
                    layerId=ids)
    })   
  })
  
  defaultsite <- reactive({
    if(!is.null(input$agreement) && input$agreement){
      paste(c("<input>", paste0("  <type>", input$type, "</type>"), 
              "  <site>US-Dk3</site>",
              "  <lat>35.9782</lat>",
              "  <lon>-79.0942</lon>",
              paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
              paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
              "</input>"), collapse="\n")
    }
    else{ "Please check the agreement. "}
  })
  
  
  
  observe({
    click<-input$map_marker_click
    #default site is US-Dk3
    if(is.null(click)) {
      click <- list(id = "US-Dk3", lat = 35.9782, lng = -79.0942)
    }
    else{
      text<-paste(click$id)
      map$clearPopups()
      map$showPopup( click$lat, click$lng, text)
    }
    
    selectedsite <- reactive({
      if(!is.null(input$agreement) && input$agreement){
        paste(c("<input>", paste0("  <type>", input$type, "</type>"), 
                paste0("  <site>", click$id, "</site>"),
                paste0("  <lat>", click$lat, "</lat>"),
                paste0("  <lon>", click$lng, "</lon>"),
                paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
                paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
                "</input>"), collapse="\n")
      }
      else{ "Please check the agreement. "}
    })
    
    output$xmltext <- renderText({
      selectedsite()
    })
    
    output$downloadXML <- downloadHandler(
      filename = function() { "example.xml" },
      content = function(file) {
        if(!is.null(input$agreement) && input$agreement){
          writeLines(c("<input>", paste0("  <type>", input$type, "</type>"), 
                       paste0("  <site>", click$id, "</site>"),
                       paste0("  <lat>", click$lat, "</lat>"),
                       paste0("  <lon>", click$lng, "</lon>"),
                       paste0("  <start_date>", if (input$start_date != "") input$start_date else "2001", "-01-01 00:00:00</start_date>"),
                       paste0("  <end_date>", if (input$end_date != "") input$end_date else "2001", "-12-31 23:59:59</end_date>"),
                       "</input>"), file)
        }
      }
    )
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        switch(input$model,
               "pecan.zip" =  "example.pecan.zip",
               "clim" =  "example.clim"
        )
      },
      
      content = function(file) {
        input_filename<-"/tmp/example.xml"
        fileConn<-file(input_filename)
        writeLines(selectedsite(), fileConn)
        
        # download the converted file from bdapi
        # TODO:use browndog.convert in bd.r after we have the library. 
        library(RCurl)
        url <- "https://bd-api.ncsa.illinois.edu"
        output<- input$model
        convert_api <- paste0(url,"/dap/convert/", output, "/") 
        httpheader <- c(Accept="text/plain", Authorization = input$token)
        curloptions <- list(httpheader = httpheader)
        result_bds <- postForm(convert_api,"file"= fileUpload(input_filename),.opts = curloptions)
        print(result_bds)
        url <- gsub('.*<a.*>(.*)</a>.*', '\\1', result_bds)
        wait <- 800
        browndog.download(url[1], file, token, wait)
      }
    )
  })
  
})

