
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinytoastr)
library(highcharter)
library(leaflet)
source(file.path('Utilities','uihelp.R'),local = T)$value
source(file.path('Utilities','Math_Utility.R'),local = T)$value
suppressPackageStartupMessages(require(leaflet.extras,quietly = T))
library(visNetwork)
library(dplyr)
library(purrr)
library(plotly)
library(ggplot2)
library(igraph)
library(geosphere)
library(sf)
library(RColorBrewer)
library(devtools)
options(shiny.maxRequestSize=30*1024^2)


# Setting up the maps
ugly_map <- "https://api.mapbox.com/styles/v1/para2x/ciwiaedba003y2qliqe3lt5vj/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoicGFyYTJ4IiwiYSI6ImNpcTJraDAzZDAxNTVmc25uNnkycTF4dHcifQ.fnJLW8J0XIIsG0Wv3HXO7A"
pretty_map <- "https://api.mapbox.com/styles/v1/para2x/cja2s4zhq10si2ul5btb1q6w9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicGFyYTJ4IiwiYSI6ImNpcTJraDAzZDAxNTVmc25uNnkycTF4dHcifQ.fnJLW8J0XIIsG0Wv3HXO7A"
mb_attribution <- "<a href='https://pecanproject.github.io/index.html' target='_blank'>PEcAn</a>"

shinyServer(function(input, output, session) {
  #Data holders
  values <- reactiveValues(MultiSite=NULL, dead_data=NULL, Multisite.info=NULL, Selected.Site=NULL)
  
  #----- First shiny modal ----
  showModal(
    modalDialog(
    title = "Help me find the SDA data",
    tagList(
      tabsetPanel(
        tabPanel('Submit a SDA job',
            br(),
                   HTML(
                     '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
                     <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">Settings <!--Padding is optional-->
                     </span>
                     </div>'
                   ),br(),
                   fluidRow(
                     column(12,
                            fileInput('xmlfileinput2',h4('Pecan xml settings file:'),width = "100%"))
                   ),
            HTML(
              '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
              <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">Pecan assim.sequential package <!--Padding is optional-->
              </span>
              </div>'
            ),br(),
            fluidRow(
              column(4,textInput('gitrepo',h4('Github repo:'),width = "100%", value="para2x/pecan")),
              column(4,textInput('gitb',h4('Github ref/branch:'),width = "100%", value="MultiSite_SDA")),
              column(4,textInput('gitsdir',h4('Github subdir:'),width = "100%", value="modules/assim.sequential"))
              
            ),HTML(
              '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
              <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">Observe data <!--Padding is optional-->
              </span>
              </div>'
            ),br(),
            fluidRow(
              column(8,
                     fileInput('obsfileinput2',h4('Observed Rdata:'),width = "100%")),
              column(4,textInput('pathsda2',h4('Input ID:'),width = "100%", value=""))
            ),HTML(
              '<div style="width: 100%; border-top: 2px solid black; text-align: center"></div>'),
            fluidRow(
              column(3),
              column(6,br(),actionButton('submitjob',h4('Submit'), width = "100%", class="btn-primary")),
              column(3)
            )
        ),
        tabPanel('Load an SDA output', 
                 fluidRow(
                   column(12,
                          fileInput('sdainputf',h4('SDA output file:'),width = "100%"))
                 ),
                 HTML(
                   '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
                   <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">OR <!--Padding is optional-->
                   </span>
                   </div>'
    ),br(),
    fluidRow(
      column(12,
             fileInput('xmlfileinput',h4('Pecan xml settings file:'),width = "100%"))
    ),
    HTML(
      '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
      <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">OR <!--Padding is optional-->
      </span>
      </div>'
    ),br(),
    fluidRow(
      column(12,textInput('pathsda',h4('Path to the SDA folder:'),width = "100%"))
    ),
    HTML(
      '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
      <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">OR <!--Padding is optional-->
      </span>
      </div>'
    ),br(),
    fluidRow(
      column(12,textInput('workflowid.inp',h4('Workflow ID:'),width = "100%"))
    ),
    HTML(
      '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
      <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;">Options <!--Padding is optional-->
      </span>
      </div>'
    ),br(),
    fluidRow(
      column(6,selectInput('machinid',h4('Machine'),c(), width = "100%")),
      column(6,awesomeCheckboxGroup(
        inputId = "options_input",
        label = h4("Options"), 
        choices = c("Active run ?","Multi-Site ?"),
        selected = c("Multi-Site ?"),
        inline = TRUE, 
        status = "primary"
      ))
    ),HTML(
      '<div style="width: 100%; border-top: 2px solid black; text-align: center">'),
    fluidRow(
      column(3),
      column(6,br(),actionButton('loadinput',h4('Load'), width = "100%", class="btn-primary")),
      column(3)
    ))
      )
    ),
    footer = NULL,
    size = 'm'
  )
  )

  
  
  
  #-- 
  observeEvent(input$submitjob,{
    
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   browser()
                   # lets first try to minimaly install the version of pecan assim that user is interested in 
                   tryCatch(
                     {
                       tmpd<-tempdir()
                       withr::with_libpaths(
                         new=tmpd,
                         devtools::install_github(
                           repo = input$gitrepo,
                           ref = input$gitb,
                           subdir = input$gitsdir,
                           dependencies = FALSE,
                           quick = TRUE,
                           reload=TRUE,
                           force = TRUE,
                           upgrade_dependencies = FALSE
                         )
                       )
                       incProgress(1/15, message="Finished installing packages")
                       library(PEcAn.assim.sequential, lib.loc =tmpd)
                        # Use a promise here to send the job and then close the dialog
                       toastr_success("PEcAn.assim.sequential package was installed successfully.")
                     },
                     error = function(e) {
                       toastr_error(title = "There is an error in installing assimilation package.", conditionMessage(e))
                     }
                   )
                 
                   
                   
                   })

  })
  
  #-- load function----
  observeEvent(input$loadinput,{
    
    tryCatch(
      {
        # I create a new env and then load the data into that and then convert it to list
        load_data.env <- new.env(parent = baseenv())
        load(input$sdainputf$datapath, load_data.env)
        load_data.list <- as.list(load_data.env)
        values$dead_data <-load_data.list
        values$MultiSite <- ifelse(any(grepl("Multi-Site",input$options_input)),TRUE,NULL)

        #if it's a multi site lets read in the basics
        if (values$MultiSite) values$Multisite.info <-list(Sites=values$dead_data$FORECAST[[1]] %>% attr('Site') %>% unique(),
                                                    Variables=values$dead_data$FORECAST[[1]] %>% colnames() %>% unique()
                                                    )
        removeModal()
        toastr_success(paste0("<h4>",input$sdainputf$name," was successfully imported. </h4>"), newestOnTop=T,position = "top-right")
      },
      error = function(e) {
        toastr_error(title = "Loading error", conditionMessage(e))
      }
    )
  })
  
  
  # Load the UI -----
  output$mainui <-renderUI({
    req(values$MultiSite)
    # if we wanted have Multisite UI
    ML.site<-tagList(      
  
      bs_accordion(id = "meet_the_beatles") %>%
        bs_append(title = "Setup",
                  content = tagList(fluidRow(
                    column(12,leafletOutput("mymap",height = "800px",width = "100%"),
                           # Shiny versions prior to 0.11 should use class = "modal" instead.
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                         draggable = TRUE, top = 1, left = "auto", right = 15, bottom = "auto",
                                         #style="z-index:15000;",
                                         width = 250, height = "auto",
                                         
                                                
                                                  column(12,
                                                         fluidRow(
                                                           column(12,  HTML(
                                                             '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
                                                             <span style="font-size: 20px; background-color: #FFFFFF; padding: 0 10px;">Description <!--Padding is optional-->
                                                             </span>
                                                             </div>'
                                                           ),br(),
                                                           HTML(paste0('<table class="table" style="padding: 0px;">
                                                                       <tbody>
                                                                       <tr>
                                                                       <th><b><h4># of sites:</h4></b></th>
                                                                       <td align="right"><h4><span class="label label-primary"> ',values$Multisite.info$Sites %>% length,' </span></h4></td>
                                                                       </tr>
                                                                       <tr>
                                                                       <th><h4># of state variables:</h4></th>
                                                                       <td align="right"><h4><span class="label label-',ifelse(TRUE,'danger','success'),'"> ',values$Multisite.info$Variables %>% length,' </span></h4></td>
                                                                       </tr>
                                                                       <tr>
                                                                       <th><h4># of observations:</h4></th>
                                                                       <td align="right"><h4><span class="label label-',ifelse(TRUE,'danger','success'),'">',values$dead_data$t,'</span></h4></td>
                                                                       </tr>
                                                                       <tr><td colspan=2>
                                                                       </td></tr>
                                                                       </tbody>
                                                                       </table>')),
                                                           
                                                           HTML(
                                                             '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
                                                             <span style="font-size: 20px; background-color: #FFFFFF; padding: 0 10px;">Options <!--Padding is optional-->
                                                             </span>
                                                             </div>'
                                                           ))
                                                           ),
                                                         br(),
                                                      
                                                        selectizeInput('boverlay','Background overlay',
                                                                       choices= list('None',
                                                                                     'Eco-region L1 overlay'='e1',
                                                                                     'Eco-region L2 overlay'='e2')),
                                                        sliderInput('tileop','Background Opacity',value = 0.25,min=0.1,max=1,step = 0.05),
                                                        prettyCheckboxGroup(
                                                             inputId = "AllmapOpt",
                                                             label = "Analysis overlay :", 
                                                             choices = list("Forecast Spatial Correlation"='spcor',
                                                                            "Raster Map"='rastMap'),
                                                             inline = TRUE, 
                                                             status = "success",
                                                             fill = TRUE
                                                           ),
                                                         conditionalPanel(
                                                           condition = "input.AllmapOpt !=''",
                                                           sliderInput('TFAll','Time Frame',value = 1,min=1,max=16,step = 1,  animate = animationOptions(interval = 2000, loop = F))
                                                         )
                                                       
                                              
                                                )
                                                         ))
                  ))
                  ),
      br()
    )
    
   if (values$MultiSite==T) return(ML.site)
  })
  
  # Map -------------------
  output$mymap <- renderLeaflet({
    
    
   #aoi_boundary_HARV <- sf::st_read(file.path("Utilities", "eco-region.json"))
    providers <- c("Stamen.TonerLite", "Stamen.Watercolor", "CartoDB.Positron", "Acetate.terrain")
    #browser()

    # maping
    leaflet() %>% 
      enableTileCaching() %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark theme")%>%
      addTiles(urlTemplate = pretty_map, attribution = mb_attribution, group = "Defualt",layerId='tile') %>%
      addLayersControl(position = c("bottomleft"),
        baseGroups = c("Defualt","Dark theme"))%>%
      addScaleBar(position="bottomleft")%>% 
      addFullscreenControl() %>%
      #addMeasure(position="bottomleft") %>%
      addMarkers(values$dead_data$site.locs[,1] %>% as.numeric(),
                 values$dead_data$site.locs[,2] %>% as.numeric(),
                 label=row.names(values$dead_data$site.locs), group ='cities'
      ) %>%
     #addResetMapButton() %>%
      # addSearchFeatures(
      #   targetGroups = 'cities',
      #   options = searchFeaturesOptions(
      #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,textPlaceholder="Search a site id",
      #     autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
       addControl("<P><B>Help:</B>  <br>Please select a site. </P>",
                  position='bottomright',layerId='help')
  })
  # Map event ----------
  observeEvent(input$mymap_marker_click,{
    #
    cdata <- session$clientData
    # Finding the site ID based on selected lat long
    site_id <- row.names(values$dead_data$site.locs)[which(values$dead_data$site.locs[,2]==input$mymap_marker_click$lat & values$dead_data$site.locs[,1]==input$mymap_marker_click$lng)]
    
    All.my.data <-list(
      FORECAST=values$dead_data$FORECAST,
      ANALYSIS=values$dead_data$ANALYSIS
    )
    showModal(
      modalDialog(
        size='l',
        easyClose = T,
        fluidRow(
          column(12,h3(paste0("SiteID:",site_id," - You can either choose a time or click on Play !")),
                 shiny::sliderInput("timelaps", NULL,
                                    min = 1, max = values$dead_data$t,
                                    value = 1, step = 1, width = "100%",
                                    animate = animationOptions(interval = 1500, loop = F))
          )
        ),
        fluidRow(
          column(12, 
                 
                   tabsetPanel(
                     tabPanel(h4("Time Series"),br(),plotlyOutput('tsplotly',height = "auto")),
                     tabPanel(h4("Forecast Var-Cov"),br(),
                              fluidRow(
                                column(12,visNetworkOutput("network",height = "600px"))
                              )
                     )
                     
                   )
          )
        )
      )
    )
    
    values$Selected.Site <- site_id  
    # time series
     output$tsplotly <-renderPlotly({
       generate_colors_sda()
       out.ts <- ts.producer.FA(All.my.data, site_id, 1)
       #out.ts %>% filter(Variable=="AbvGrndWood") ->out.ts
       
       p<-out.ts %>%
         ggplot(aes(x=Time,y=Means,ymax=Upper,ymin=Lower))+
         geom_ribbon(aes(fill=Type),color="white")+
         geom_line(aes(y=Means, color=Type),lwd=1.02,linetype=2)+
         #geom_point(aes(y=Means, color=Type),size=3,alpha=0.75)+
         scale_fill_manual(values = c(alphapink,alphagreen,alphablue),name="")+
         scale_color_manual(values = c(alphapink,alphagreen,alphablue),name="")+
         theme_bw(base_size = 17)+
         labs(y="")+
          facet_wrap(~Variable,scales = "free",ncol=2)+
         theme(legend.position = "top",
               strip.background = element_blank(),
               panel.background = element_rect(fill="#242F39"),
               panel.grid.major = element_line(color="#2E3740"),
               panel.grid.minor = element_line(color="#2E3740"))
       
       ggplotly(p,height=850) %>%
         layout(
           showlegend = FALSE,
           margin = list(r = 10, l=0)
         )
       
      
     })
    
    #Network and matrix in one----------------------------------------
    output$network <- renderVisNetwork({


      # sites_orders <- map_chr(colnames(values$dead_data[["enkf.params"]][[1]][["Pf"]]), function(j) gsub("[\\(\\)]", 
      #                                                         "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      # cols <- which(sites_orders == site_id)
      # 
      # rows <-which(sites_orders == site_id)
      
      
      values$dead_data$enkf.params %>% map(~.x$Pf)->p
      p<-p[[input$timelaps]]

      
      # if nothing found
     
      g <- igraph::graph.adjacency(p,
                                   mode="min",
                                   diag = F) # For directed networks
      g<-igraph::simplify(g)
      igraph::V(g)$label.cex <-0.59
      igraph::V(g)$label.color <- rgb(0, 0, .2, .8)
      
      
      data <- toVisNetworkData(g)
      # Adding group
      data$nodes$group <- map_chr(data$nodes$id, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      data$edges$groupf <- map_chr(data$edges$from, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      data$edges$groupt <- map_chr(data$edges$to, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      
      # # filter network
      # 
      data$edges <- data$edges %>% filter(groupt ==site_id | groupf ==site_id)
      data$nodes <- data$nodes %>% filter(group %in% c(data$edges$groupf,data$edges$groupt))
      
      if (nrow(data$nodes)==0){
        toastr_warning("No corrolation exists !")
        return(NULL)
      }
      #
 
      visNetwork(nodes = data$nodes, edges = data$edges) %>%
       # visNetworkEditor()%>%
        visGroups() %>%
        visOptions(selectedBy = list(variable="group", selected=site_id),#
                   collapse = TRUE,
                   highlightNearest = TRUE)%>%
  
        visExport() %>%
        visPhysics(solver = "forceAtlas2Based", 
                   forceAtlas2Based = list(gravitationalConstant = -150))
    })

    toastr_success(paste0(site_id ," was selected."))
  })
  
  # timelaps slider -----
  observeEvent(input$timelaps,{
    generate_colors_sda()
    site_id<- isolate(values$Selected.Site )
    
    All.my.data <-list(
      FORECAST=values$dead_data$FORECAST,
      ANALYSIS=values$dead_data$ANALYSIS
    )
    
    # Time series plotly proxy
    out.ts <- ts.producer.FA(All.my.data, site_id, input$timelaps)
    
    p<-out.ts %>%
      ggplot(aes(x=Time,y=Means,ymax=Upper,ymin=Lower))+
      geom_ribbon(aes(fill=Type),color="white")+
      geom_line(aes(y=Means, color=Type),lwd=1.02,linetype=2)+
      geom_point(aes(y=Means, color=Type),size=3,alpha=0.75)+
      scale_fill_manual(values = c(alphapink,alphagreen,alphablue),name="")+
      scale_color_manual(values = c(alphapink,alphagreen,alphablue),name="")+
      theme_bw(base_size = 17)+
      labs(y="")+
      facet_wrap(~Variable,scales = "free",ncol=2)+
      theme(legend.position = "top",
            strip.background = element_blank(),
            panel.background = element_rect(fill="#242F39"),
            panel.grid.major = element_line(color="#2E3740"),
            panel.grid.minor = element_line(color="#2E3740"))
    
    plotlygg <-ggplotly(p,height=850) %>%
      layout(
        showlegend = FALSE,
        margin = list(r = 10, l=0)
      )
    
    plotlygg$x$layout$legend <-NULL
    plotlygg$x$layout$showlegend <-FALSE
  
    
    plotlyProxy("tsplotly", session) %>%
      #plotlyProxyInvoke("deleteTraces", list(as.integer(0))) %>%
      plotlyProxyInvoke("addTraces", plotlygg$x$data)  %>%
      plotlyProxyInvoke("relayout", plotlygg$x$layout) 
    

  })
  
  
  # Map proxy spatial correlation ----------------
  observe({

    # if the spatial corrlation was not checked 
    if ("spcor" %in% input$AllmapOpt) {

      site_id<- isolate(values$Selected.Site)
      
      values$dead_data$enkf.params %>% map(~.x$Pf)->p
      p<-p[[input$TFAll]]
      
      
      # if nothing found
      g <- igraph::graph.adjacency(p,
                                   mode="min",
                                   diag = F) # For directed networks
      g<-igraph::simplify(g)
      igraph::V(g)$label.cex <-0.59
      igraph::V(g)$label.color <- rgb(0, 0, .2, .8)
      
      
      data <- toVisNetworkData(g)
      # Adding group
      data$nodes$group <- map_chr(data$nodes$id, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      data$edges$groupf <- map_chr(data$edges$from, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      data$edges$groupt <- map_chr(data$edges$to, function(j)gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]]))
      alllines <- data$edges %>% filter(groupf != groupt)
      
      if (nrow(alllines)==0) return(NULL)
      
      
      p1<-map_df(alllines$groupt, function(x){
        values$dead_data$site.locs[which(row.names(values$dead_data$site.locs)==x),c(1,2)] %>% t %>%as.data.frame()
      })
      
      p2<-map_df(alllines$groupf, function(x){
        values$dead_data$site.locs[which(row.names(values$dead_data$site.locs)==x),c(1,2)]%>% t %>%as.data.frame()
      })
      
      
      # Estimating the tickness based on value of cov
      tmp.covs<-alllines %>%
        left_join(reshape2::melt(p) %>%
                    mutate(from=as.character(Var1),
                           to=as.character(Var2))) %>% 
        dplyr::select(value) %>%
        unlist()%>%
        as.numeric()
      
      lwds <- tmp.covs %>%
        scales::rescale(to=c(2,30)) %>%
        round(3)
      
      
      leafletProxy("mymap", data=gcIntermediate(p1,
                                                p2,
                                                n=100, 
                                                addStartEnd=TRUE,
                                                sp=TRUE)) %>%
        removeShape(layerId = paste0("line",1:1000))%>%
        #clearShapes()%>%
        addPolylines(popup=paste0("<h4>",alllines$from," & ",alllines$to,"<br>",tmp.covs %>% round(2),"</h4>"),
                     weight = lwds, color="#cc0033", layerId = paste0("line",seq_along(alllines$to)))%>%
        removeControl('help') %>%
        addControl("<P><B>Help:</B>  <br> The thickness of the lines shows the strength of the correlation.</P>",
                   position='bottomright',layerId='help')
      
    }
  


    if (!("spcor" %in% input$AllmapOpt)){
      # clear the map
      leafletProxy("mymap") %>%
        removeShape(layerId = paste0("line",1:1000))%>%
        removeControl('help')
    }

    
  })
  #background overlay --------------------------
  observeEvent(c(input$boverlay,input$tileop),{
    
    #making colors 
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors/2, rownames(qual_col_pals)))
    
        #cleaning 
    if (input$boverlay=='None'){
      # clear the map
      leafletProxy("mymap") %>%
        clearShapes()%>%
        removeControl('help')
    }else if(input$boverlay=="e1"){
      #Reading eco layer
      aoi_boundary_HARV <-  sf::read_sf("Utilities/l1.json")

      aoi_boundary_HARV %>%
        st_set_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>%
        st_transform("+proj=longlat +datum=WGS84") %>%
      leafletProxy("mymap", data=.) %>%
        clearShapes()%>%
        addPolygons(stroke = FALSE, fillOpacity = input$tileop, smoothFactor = 0.5,
                    fillColor = col_vector, label=~NA_L1KEY %>%map(~HTML(sprintf("<h5><b> %s </b></h5>",.x)))
        )
      
    }else if(input$boverlay=="e2"){
      #Reading eco layer
      aoi_boundary_HARV <-  sf::read_sf("Utilities/l2.json")
      
      aoi_boundary_HARV %>%
        st_set_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>%
        st_transform("+proj=longlat +datum=WGS84") %>%
      leafletProxy("mymap", data=.) %>%
        clearShapes()%>%
        addPolygons(stroke = FALSE, fillOpacity = input$tileop, smoothFactor = 0.5,
                    fillColor = col_vector, label=~NA_L2KEY %>%map(~HTML(sprintf("<h5><b> %s </b></h5>",.x)))
        )
      
    }
    
    
    
  })
  
})
