# db.query query statement
cmd <-  paste0("SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, attributes.value," ,
               "CONCAT(coalesce(sites.id, -99), ' / ', coalesce(sites.sitename, ''), ', ', ', ') AS sitename, " ,
               "CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) AS modelname, modeltypes.name " ,
               "FROM workflows " ,
               "LEFT OUTER JOIN sites on workflows.site_id=sites.id " ,
               "LEFT OUTER JOIN models on workflows.model_id=models.id " ,
               "LEFT OUTER JOIN modeltypes on models.modeltype_id=modeltypes.id " ,
               "LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' ")


observeEvent(input$workflowclassrand, {
  tryCatch({
    history <- PEcAn.DB::db.query(cmd, dbConnect$bety$con)
    workflow_id <- strsplit(input$workflowselected, "_")[[1]]
    workflow_id <- trimws(workflow_id[2])
    val.jason <- history$value[history$id == workflow_id]
    fld <- history$folder[history$id == workflow_id]
    
    if (!is.na(val.jason)) {
      # server and ui for the listviewer
      output$jsed <- renderJsonedit({
        jsonedit(jsonlite::fromJSON(val.jason))
        
      })
      
      showModal(modalDialog(
        title = "Details",
        tabsetPanel(
          tabPanel("Info", br(),
                   jsoneditOutput("jsed", height = "500px")
          )),
        easyClose = TRUE,
        footer = NULL,
        size = 'l'
      ))
    }
  },
  error = function(e){
    toastr_error(title = "Error", conditionMessage(e))
  })
})

observeEvent(input$workflow_explor_classrand, {
  tryCatch({
    #history <- PEcAn.DB::db.query(cmd, dbConnect$bety$con)
    workflow_id <- strsplit(input$workflows_explor_selected, "_")[[1]]
   
    workflow_id <- trimws(workflow_id[1])
    
    updateSelectizeInput(session,
                         "all_workflow_id",
                         choices = c(input$all_workflow_id, workflow_id),
                         selected = c(input$all_workflow_id, workflow_id))

  },
  error = function(e){
    toastr_error(title = "Error", conditionMessage(e))
  })
})


observeEvent(input$submitInfo, {
  tryCatch({
    history <- PEcAn.DB::db.query(cmd, dbConnect$bety$con)
    output$historyfiles <- DT::renderDT(
      DT::datatable(history %>%
                      dplyr::select(-value, -modelname) %>%
                      mutate(id = id %>% as.character()) %>%
                      mutate(id=paste0("<button id=\"workflowbtn_",id, " \" type=\"button\" class=\"btn btn-primary btn-sm btn-block action-button workflowclass\" width=\"100%\">",id,"</button>"),
                             Action= paste0('<div class="btn-group">
                                            <button type="button" class="btn btn-primary ">Action</button>
                                            <button type="button" class="btn btn-primary dropdown-toggle" data-toggle="dropdown">
                                            <span class="caret"></span>
                                            <span class="sr-only">Toggle Dropdown</span>
                                            </button>
                                            <ul class="dropdown-menu" role="menu">
                                            <li><a href="#" class="expanclass" id=',paste0(history$id,"_btn3"),'>Explore</a></li>
                                            <li class="divider"></li>
                                            <li><a href="#" class="labclass" id=',paste0(history$id,"_lab4"),'>Delete</a></li>
                                            </ul></div>')
                             
                             )%>%
                      dplyr::rename(model=name),
                    escape = F,
                    filter = 'top',
                    selection="none",
                    style='bootstrap',
                    rownames = FALSE,
                    options = list(
                      autowidth = TRUE,
                      columnDefs = list(list(width = '90px', targets = -1)),  #set column width for action button 
                      dom = 'ftp',
                      pageLength = 10,
                      scrollX = FALSE,
                      scrollCollapse = FALSE,
                      initComplete = DT::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                    )
                      )
      
                      )
    toastr_success("Generate history runs")
  },
  error = function(e) {
    toastr_error(title = "Error in History Runs Page", message = ""
                 #, conditionMessage(e)
                 )
  })

})
