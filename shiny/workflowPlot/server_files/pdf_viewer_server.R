# data table that lists file names
observe({
  req(input$all_workflow_id)
  w_ids <- input$all_workflow_id
  folder.path <- c()
  for(w_id in w_ids){
    folder.path <- c(folder.path, workflow(dbConnect$bety, w_id) %>% collect() %>% pull(folder))
  }

  output$files <- DT::renderDT(
    DT::datatable(list.files(folder.path,"*.pdf") %>% 
                    as.data.frame()%>%
                    `colnames<-`(c("File name")),
                  escape = F,
                  selection="single",
                  style='bootstrap',
                  rownames = FALSE,
                  options = list(
                    dom = 'ft',
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollCollapse = TRUE,
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  )
    )
    
  )
})


# displays pdf views
observeEvent(input$files_cell_clicked, {  
  req(input$all_workflow_id)
  w_ids <- input$all_workflow_id
  folder.path <- c()
  for(w_id in w_ids){
    folder.path <- c(folder.path, workflow(dbConnect$bety, w_id) %>% collect() %>% pull(folder))
  }
  
  if (length(input$files_cell_clicked) > 0) {
    # File needs to be copied to the www folder
    if(file.access("www", 2) == 0){         #check write permission
      for(i in length(folder.path)){
        file.copy(file.path(folder.path[i], input$files_cell_clicked$value),
                  "www",
                  overwrite = T)
      }
    }else{
      print("Pdf files cannot not be copied to www folfer. Do not have write permission.")
    }

    output$pdfview <- renderUI({
      tags$iframe(style = "height:800px; width:100%; border: 1px grey solid;",
                  src = input$files_cell_clicked$value)
    })
  }
  
})
