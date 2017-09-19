##### Benchmarking
observeEvent(input$load,{
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  button <- FALSE
  if(nrow(ids_DF) == 1){
    
    ens_id <- dplyr::tbl(bety, 'runs') %>% dplyr::filter(id == ids_DF$runID) %>% dplyr::pull(ensemble_id)
    ens_wf <- dplyr::tbl(bety, 'ensembles') %>% dplyr::filter(id == ens_id) %>%
      dplyr::rename(ensemble_id = id) %>%
      dplyr::left_join(.,tbl(bety, "workflows") %>% dplyr::rename(workflow_id = id), by="workflow_id") %>% dplyr::collect()
    
    settingsXML <- file.path(ens_wf$folder,"pecan.CHECKED.xml")
    
    # Automatically creates a new pecan.xml I think. Need to fix this. 
    clean <- PEcAn.settings::clean.settings(inputfile = settingsXML,write=FALSE)
    # Remove database & host information
    clean$database <- NULL 
    clean$host <- NULL
    clean$info <- NULL
    clean$outdir <- NULL
    clean$meta.analysis <- NULL
    clean$ensemble <- NULL
    str(clean)
    settings_xml <- toString(PEcAn.utils::listToXml(clean, "pecan"))
    ref_run <- db.query(paste0(" SELECT * from reference_runs where settings = '", settings_xml,"'"), bety$con)
    
    if(length(ref_run) == 0){
      bm_message <- sprintf("Would you like to save this run (run id = %.0f, ensemble id = %0.f) as a reference run?", ids_DF$runID, ens_id) 
      button <- TRUE
    }else if(dim(ref_run)[1] == 1){
      bm_message <- sprintf("This run has been registered as a reference run")
    }else if(dim(ref_run)[1] > 1){ # There shouldn't be more than one reference run per run
      bm_message <- ("There is more than one reference run in the database for this run. Review for duplicates.")
    }
  }else if(nrow(ids_DF) > 1){
    bm_message <- "Benchmarking currently only works when one run is selected."
  }else{
    bm_message <- "Cannot do benchmarking"
  }
  
  output$bm_message <- renderText({bm_message})
  
  output$button_BRR <- renderUI({
    if(button){actionButton("create_bm", "Create Benchmarking Reference Run")}
  })
  
})

observeEvent(input$create_bm,{
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  ens_id <- dplyr::tbl(bety, 'runs') %>% dplyr::filter(id == ids_DF$runID) %>% dplyr::pull(ensemble_id)
  ens_wf <- dplyr::tbl(bety, 'ensembles') %>% dplyr::filter(id == ens_id) %>%
    dplyr::rename(ensemble_id = id) %>%
    dplyr::left_join(.,tbl(bety, "workflows") %>% dplyr::rename(workflow_id = id), by="workflow_id") %>% dplyr::collect()
  BRR <- PEcAn.benchmark::create_BRR(ens_wf, con = bety$con)
  bm_message <- paste("This run has been successfully registered as a reference run")
  button <- FALSE
  output$bm_message <- renderText({bm_message})
  output$button_BRR <- renderUI({
    if(button){actionButton("create_bm", "Create Benchmarking Reference Run")}
  })
})