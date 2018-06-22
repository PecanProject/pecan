optionsDT_fixe <- list(paging = FALSE, searching = FALSE, bInfo = FALSE, search.caseInsensitive = TRUE)


shinyServer(function(input, output, session) {
    observe({
        input$GOPackage
        isolate({
            # print(input$Pack)
            if (length(input$packages) > 0) {
                data <- Pck.load.to.vis(input$packages)
                
                func <- c(input$packages)
                # print(func)
                
                
                nb.func.slave = NULL
                nb.func.master = NULL
                for (i in 1:length(func)) {
                  
                  id.call <- as.numeric(as.character(data$Nomfun$id[which(func[i] == data$Nomfun$label)]))
                  
                  id.call.slave <- as.numeric(as.character(data$fromto$from[which(id.call == data$fromto$to)]))
                  id.call.master <- as.numeric(as.character(data$fromto$from[which(id.call == data$fromto$from)]))
                  
                  nb.call <- length(as.character(data$Nomfun$label[id.call.slave]))
                  nb.func.slave[i] = nb.call
                  
                  nb.call <- length(as.character(data$Nomfun$label[id.call.master]))
                  nb.func.master[i] = nb.call
                  
                }
                
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('tabledep').style.width = '400px';}")
                ## Output first graph
                df <- data.frame(Package = func, Import = nb.func.master, `Imported by` = nb.func.slave)
                
                
                
                output$tabledep <- renderDataTable({
                  df
                }, options = optionsDT_fixe)
                
                output$main_plot <- renderVisNetwork({
                  data$fromto%>%filter(title%>%gsub('<p>','',.)%>%gsub('</p>','',.)%in%input$variablesp)->data$fromto
                  if (nrow(data$fromto)){
                                      net <- plot(data, block = TRUE)
                  
                  # add legend
                  data_legend <- unique(data$fromto[, c("title", "color")])
                  data_legend$label <- gsub("<p>", "", data_legend$title, fixed = TRUE)
                  data_legend$label <- gsub("</p>", "", data_legend$label, fixed = TRUE)
                  data_legend$title <- NULL
                  data_legend$arrows <- "to"
                  
                  net %>%
                    visLegend(addEdges = data_legend, useGroups = FALSE, width = 0.1)
                  }

                  
                })
                curentd1 <<- data
                output$titledatatabel <- renderText({
                  "Dependencies between package(s)"
                })
                
            }
        })
    })
    
    
    observe({
        current.package <- input$main_plot_selected
        current.package <- as.character(curentd1$Nomfun[as.numeric(current.package), "label"])
        updateSelectizeInput(session, "package", NULL, choices = installed.packages()[, 1]%>%grep("^PEcAn",.,value = T,ignore.case = T), selected = current.package)
    })
    
    observe({
        input$GOFunc2
        isolate({
            if (input$package != "" && input$GOFunc2 > 0) {
                
                func <- input$package
                # print(func)
                func
                
                if (!func %in% installed.packages()[, 1]) {
                  install.packages(func)
                }
                library(func, character.only = TRUE)
                dep1 <- envirDependencies(paste0("package:", func))
                nb.fun <- length(dep1$Nomfun$label)
                
                
                updateTabsetPanel(session, "Tabsetpan", selected = "Functions")
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('datatable2').style.width = '100px';}")
                output$datatable2 <- renderDataTable(data.frame(Number.of.functions = nb.fun), options = optionsDT_fixe)
                
                output$zoomin <- renderText(paste("Zoom on package ", func))
                output$info <- renderText(paste("Information on package ", func))
                curentd3 <<- func
                
                output$main_plot1 <- renderVisNetwork({
                  plot(dep1, block = TRUE)
                })
                curentd2 <<- dep1
            }
        })
    })
    
    observe({
        input$GOFunc1
        isolate({
            if (!is.null(input$main_plot_selected) && input$main_plot_selected != "" && input$GOFunc1 > 0) {
                
                func <- as.character(curentd1$Nomfun$label[input$main_plot_selected == curentd1$Nomfun$id])
                # print(func)
                func
                
                if (!func %in% installed.packages()[, 1]) {
                  install.packages(func)
                }
                library(func, character.only = TRUE)
                dep1 <- envirDependencies(paste0("package:", func))
                nb.fun <- length(dep1$Nomfun$label)
                
                
                updateTabsetPanel(session, "Tabsetpan", selected = "Functions")
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('datatable2').style.width = '100px';}")
                output$datatable2 <- renderDataTable(data.frame(Number.of.functions = nb.fun), options = optionsDT_fixe)
                
                output$zoomin <- renderText(paste("Zoom on package : ", func))
                output$info <- renderText(paste("Information on : ", func))
                curentd3 <<- func
                
                output$main_plot1 <- renderVisNetwork({
                  plot(dep1, block = TRUE)
                })
                curentd2 <<- dep1
            }
        })
    })
    
    ### chossefunction
    
    observe({
        input$chargedf
        isolate({
            input$packageslist
            sapply(input$packageslist, function(x) {
                library(x, character.only = TRUE)
            })
            allFun <- unique(unlist(sapply(input$packageslist, function(x) {
                allFunctionEnv(paste0("package:", x))
            })))
            
            updateSelectizeInput(session, inputId = "functionlist", choices = allFun)
        })
    })
    
    output$chossefunctionplot <- renderVisNetwork({
        input$makegraph
        
        isolate({
            if (input$makegraph >= 1) {
                
                dep<-my.allDepFunction(input$packageslist, unlist(strsplit(input$functionlist, split = ";")))
                #- lets exlude the ones in the base or more widely used ones
               # which(dep[["Nomfun"]]$label%in%c('papply','stop','warning','logger.warn','logger.error','logger.debug','logger.severe','logger.info'))->excludes
              #  dep[["fromto"]]<-(dep[["fromto"]])%>%dplyr::filter(!(from%in%excludes))%>%dplyr::filter(!(to%in%excludes))
                #- nolink
               # which(!(dep[["Nomfun"]]$id%in%as.numeric(unlist(dep[["fromto"]]))))->nolinks
              #  dep[["Nomfun"]]<-dep[["Nomfun"]]%>%dplyr::filter(!(id%in%c(excludes,nolinks)))
                #-- plotting
                 visNetwork(dep[[1]], dep[[2]])%>%
                       visGroups()%>%
                       visOptions(selectedBy = "group",
                                  collapse = TRUE,
                                  highlightNearest = TRUE, 
                                  nodesIdSelection = list(enabled = TRUE))%>%
                   visExport() %>%
                   visPhysics(stabilization=list(iterations=100))%>%
                   visEdges(arrows =list(from = list(enabled = TRUE)),
                            color = list(color = "lightblue", highlight = "red")) %>%
                   visLegend(zoom = T,width = 0.1)->netw
                 if (input$igraphcheck)
                   netw%>%visIgraphLayout()->netw
                 return(netw)
                 
            }
        })
        
    })
    
    observeEvent(input$showsourcebtn,{
      if (length(isolate(input$packageslist))>0){
        dep<-my.allDepFunction(isolate(input$packageslist), unlist(strsplit(input$functionlist, split = ";")))
        showModal(
          modalDialog(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Source code", fluidRow( column(12,verbatimTextOutput("console")))),
                        tabPanel("Variable definitions timelines",  plotOutput('VDT',width = "100%",height = "500px"))

            ),
            size='l',
            easyClose = T
          )
        )
        
      }
      
      #----------- finding and printing the source code
      srcode<-getAnywhere( dep[["Nomfun"]][input$chossefunctionplot_selected,2]%>%as.character())
      #readScript(txt= paste(deparse(body()%>%as.character())))
      
      srcode$objs[[1]]%>%
        body(.)%>%
        as.list()%>%
        #deparse(.)%>%
        paste(.,collapse = " \n ")%>%
        readScript(txt=.)->sc
      
      output$console<-renderPrint({
        srcode
      })
      output$consoleH<-renderPrint({
        help( dep[["Nomfun"]][input$chossefunctionplot_selected,2]%>%as.character())->shelp
        utils:::print.help_files_with_topic(shelp)
      })
      #Variable definitions timelines
      output$VDT<-renderPlot({
        
        dtm = getDetailedTimelines(sc, getInputs(sc))
        #-plotting
        dtm$start<-ifelse(dtm$defined,dtm$step,NA)
        dtm$end<-ifelse(dtm$used,dtm$step,NA)
        dtm%>%filter(!is.na(start) | !is.na(end))->dtm
        
        
        dtm%>%
          ggplot()+
          geom_point(aes(x=start,y=var,color="Start"),shape=16,size=5)+
          geom_point(aes(x=end,y=var,color="End"),size=5,shape=2)+
          scale_color_manual(values=c("red","blue"),name="")+
          theme_minimal(base_size = 16)+
          labs(x="Steps",y="variable")+
          theme(legend.position = "top")
        
      })
    })
    
    observe({
        
        if (!is.null(input$main_plot1_selected) && input$main_plot1_selected != "") {
            isolate({
                pck <- curentd3
                
                # print(pck)
                
                func <- as.character(curentd2$Nomfun$label[input$main_plot1_selected == curentd2$Nomfun$id])
                # print(func)
                try(add.html.help(pck, func), TRUE)
                
                if (length(htmlTreeParse(paste0(getwd(), "/temp.html"))$children$html) > 0) {
                  output$help <- renderUI(includeHTML(paste0(getwd(), "/temp.html")))
                  
                } else {
                  output$help <- renderUI("Not available help for this function")
                }
            })
        } else {
            
            output$help <- renderUI("Select a function")
        }
        
    })
    
    observe({
        
        if (!is.null(input$main_plot_selected) && input$main_plot_selected != "") {
            
            func <- as.character(curentd1$Nomfun$label[input$main_plot_selected == curentd1$Nomfun$id])
            
            output$Groupebutton <- renderUI({
                
                div(hr(), actionButton("GOFunc1", paste0("Launch zoom on : ", func), icon = icon("line-chart")), align = "center")
                
            })
        } else {
            output$Groupebutton <- renderUI({
                NULL
            })
        }
        
    })
    
    
    
    observe({
        
        input$GObott
        # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name', 'size',
        # 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
        
        inFile <- input$file1
        
        if (!is.null(inFile)) {
            dep <- data.graph.script(inFile$datapath)
            output$plotscript <- renderVisNetwork({
                plot(dep, block = TRUE)
            })
        }
    })
})
