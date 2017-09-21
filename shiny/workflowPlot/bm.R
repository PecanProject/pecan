##### Benchmarking

bm <- reactiveValues()
# log_con <- file(paste("bench",format(Sys.time(), "%Y-%m-%d_%H:%S"), "log", sep = "."))


observeEvent(input$load,{
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  button <- FALSE
  if(nrow(ids_DF) == 1){
    
    # Check to see if the run has been saved as a reference run 
    # (note this is straight from the beginning of create.BRR and could 
    # potentially be turned in to its own mini function)
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
      # If not registered, button appears with option to run create.BRR
      brr_message <- sprintf("Would you like to save this run (run id = %.0f, ensemble id = %0.f) as a reference run?", ids_DF$runID, ens_id) 
      button <- TRUE
    }else if(dim(ref_run)[1] == 1){
      bm$BRR <- ref_run %>% rename(.,reference_run_id = id)
      bm$BRR
      brr_message <- sprintf("This run has been registered as a reference run (id = %.0f)", bm$BRR$reference_run_id)
    }else if(dim(ref_run)[1] > 1){ # There shouldn't be more than one reference run per run
      brr_message <- ("There is more than one reference run in the database for this run. Review for duplicates.")
    }
  }else if(nrow(ids_DF) > 1){
    brr_message <- "Benchmarking currently only works when one run is selected."
  }else{
    brr_message <- "Cannot do benchmarking"
  }
  
  # This is redundant but better for debugging
  bm$brr_message <- brr_message
  bm$button_BRR <- button
  bm$ens_wf <- ens_wf
  
})

# When button to register run is clicked, create.BRR is run and the button is removed.
observeEvent(input$create_bm,{
  req(input$all_run_id)
  bm$BRR <- PEcAn.benchmark::create_BRR(bm$ens_wf, con = bety$con)
  bm$brr_message <- sprintf("This run has been successfully registered as a reference run (id = %.0f)", bm$BRR$reference_run_id)
  bm$button_BRR <- FALSE
})

observeEvent({
  bm$brr_message
  bm$button_BRR
},{
  output$brr_message <- renderText({bm$brr_message})
  output$button_BRR <- renderUI({
    if(bm$button_BRR){actionButton("create_bm", "Create Benchmarking Reference Run")}
  })
})


##### Setup benchmarks
observeEvent(input$load_data,{
  req(input$all_input_id)
  
  bm$metrics <- dplyr::tbl(bety,'metrics') %>% dplyr::select(one_of("id","name","description")) %>% collect()
  bm$vars_list <- c("NPP", "LAI")
  
  # Need to write warning message that can only use one input id
  inputs_df <- getInputs(bety,c(input$all_site_id)) %>% dplyr::filter(input_selection_list == input$all_input_id)
  format <- PEcAn.DB::query.format.vars(bety = bety, input.id = inputs_df$input_id)
  # Are there more human readable names?
  bm$vars <- format$vars[-grep("%",format$vars$storage_type), c("variable_id", "pecan_name")]
  
  
  #This will be a longer set of conditions
  bm$ready <- !is.null(bm$BRR)
})

observeEvent({
  bm$ready
  bm$metrics_list
  bm$plots_list
  bm$vars_list
},{
  
  plot_ind <- grep("_plot",bm$metrics$name)
  
  output$bm_inputs <- renderUI({
    if(bm$ready){
      list(
        column(4, wellPanel(
          checkboxGroupInput("vars", label = h3("Variables"),
                             choiceNames = bm$vars$pecan_name,
                             choiceValues = bm$vars$variable_id),
          # actionButton("selectall.var","Select /Deselect all variables"),
          label=h6("Label")
        )),
        column(4, wellPanel(
          checkboxGroupInput("metrics", label = h3("Numerical Metrics"), 
                             choiceNames = bm$metrics$description[-plot_ind],
                             choiceValues = bm$metrics$id[-plot_ind]),
          # actionButton("selectall.num","Select/Deselect all numerical metrics") ,
          label=h6("Label")
        )),
        column(4, wellPanel(
          checkboxGroupInput("plots", label = h3("Plot Metrics"),
                             choiceNames = bm$metrics$description[plot_ind],
                             choiceValues = bm$metrics$id[plot_ind]),
          # actionButton("selectall.plot","Select/Deselect all plot metrics"),
          label=h6("Label")
        ))
      )
    }
  })
  if(bm$ready){bm$calc_bm_message <- sprintf("Please select at least one variable and one metric")}
})

observeEvent({
  input$vars
  input$metrics
  input$plots
  },{
  v <- ifelse(is.null(input$vars),0,length(input$vars))
  n <- ifelse(is.null(input$metrics),0,length(input$metrics))
  p <- ifelse(is.null(input$plots),0,length(input$plots))
  m <- n + p
  output$report <- renderText(sprintf("Number of vars: %0.f, Number of metrics: %0.f", v,m))
  if(v > 0 & m > 0){
    output$calc_bm_button <- renderUI({actionButton("calc_bm", "Calculate Benchmarks")})
    bm$bm_vars <- input$vars
    bm$bm_metrics <- c()
    if(n > 0) bm$bm_metrics <- c(bm$bm_metrics, input$metrics)
    if(p > 0) bm$bm_metrics <- c(bm$bm_metrics, input$plots)
  }
  
}, ignoreNULL = FALSE)

observeEvent(input$calc_bm,{
  req(input$all_input_id)
  req(input$all_site_id)
  req(bm)
  bm$calc_bm_message <- sprintf("Setting up benchmarks")
  output$reportvars <- renderText(paste(bm$bm_vars, seq_along(bm$bm_vars)))
  output$reportmetrics <- renderText(paste(bm$bm_metrics))
  
  inputs_df <- getInputs(bety,c(input$all_site_id)) %>% 
    dplyr::filter(input_selection_list == input$all_input_id)
  output$inputs_df_table <- renderTable(inputs_df)
  
  config.list <- PEcAn.utils::read_web_config("../../web/config.php")
  output$config_list_table <- renderTable(as.data.frame.list(config.list))
  
  bm$bm_settings$info <- list(userid = 1000000003) # This is my user id. I have no idea how to get people to log in to their accounts through the web interface and right now the benchmarking code has sections dependent on user id - I will fix this. 
  bm$bm_settings$database <- list(
    bety = list(
      user = config.list$db_bety_username,
      password = config.list$db_bety_password,
      host = config.list$db_bety_hostname,
      dbname = config.list$db_bety_database,
      driver = config.list$db_bety_type,
      write = TRUE
    ),
    dbfiles = config.list$dbfiles_folder
  )
  bm$bm_settings$benchmarking <- list(
    ensemble_id = bm$ens_wf$ensemble_id,
    new_run = FALSE
  )
  
  for(i in seq_along(bm$bm_vars)){
    benchmark <- list(
      input_id = inputs_df$input_id,
      variable_id = bm$bm_vars[i],
      site_id = inputs_df$site_id,
      metrics = list()
    )
    for(j in seq_along(bm$bm_metrics)){
      benchmark$metrics = append(benchmark$metrics, list(metric_id = bm$bm_metrics[j]))
    }
    bm$bm_settings$benchmarking <- append(bm$bm_settings$benchmarking,list(benchmark = benchmark))
  }
  
  output$calc_bm_button <- renderUI({})
  output$print_bm_settings <- renderPrint(bm$bm_settings)
  
  basePath <- dplyr::tbl(bety, 'workflows') %>% dplyr::filter(id %in% bm$ens_wf$workflow_id) %>% dplyr::pull(folder)
  
  settings_path <- file.path(basePath, "pecan.BENCH.xml")
  saveXML(PEcAn.utils::listToXml(bm$bm_settings,"pecan"), file = settings_path)
  bm$settings_path <- settings_path
  
  bm$calc_bm_message <- sprintf("Benchmarking settings have been saved here: %s", bm$settings_path)
  
})

# This sources the benchmarking workflow, which at the moment seems simpler than pulling functions out of it.
# observeEvent(bm$settings_path,{ #Mostly so this is in a separate section which makes it asier to debug.
#   settings <- PEcAn.settings::read.settings(bm$settings_path)
#   source(system.file("scripts/benchmark.workflow.R",package = "PEcAn.benchmark"))
#   if(!is.null(results)){
#     bm$calc_bm_message <- sprintf("Calculating benchmarks complete")
#     output$results_table <- renderTable(results[[1]]$bench.results)
#   }
# })

observeEvent(bm$calc_bm_message,{
  output$calc_bm_message <- renderText({bm$calc_bm_message})
})

################################################
# Action buttons to select all variables/metrics but that currently aren't working
# 
# 
# observeEvent(input$selectall.var,{
#   clicks <- as.numeric(input$selectall.var)
#   output$actionclickCount <- renderText({
#     paste('Action Button Clicks =',clicks)
#   })
#   if (clicks%%2 == 0){
#     updateCheckboxGroupInput(session = session, inputId = "vars",
#                              choices = bm$vars_list)
#     shinyjs::html("labelText", "Label")
#   }else{
#     updateCheckboxGroupInput(session = session, inputId = "vars",
#                              choices = bm$vars_list,
#                              selected = bm$vars_list)
#     shinyjs::html("labelText", "Label")
#   }
# })
# 
#   # Numerical metrics
#   if(input$selectall.num == 0){
#     return(NULL)
#   }else if (input$selectall.num%%2 == 0){
#     updateCheckboxGroupInput(session = session, inputId = "metrics",
#                              choices = bm$metrics$description[-grep("_plot",bm$metrics$name)])
#     shinyjs::html("labelText", "Label")
#   }else{
#     updateCheckboxGroupInput(session = session, inputId = "metrics",
#                              choices = bm$metrics$description[-grep("_plot",bm$metrics$name)],
#                              selected = bm$metrics$description[-grep("_plot",bm$metrics$name)])
#     shinyjs::html("labelText", "Label")
#   }
#   
#   # Plot metrics
#   if(input$selectall.plot == 0){
#     return(NULL)
#   }else if (input$selectall.plot%%2 == 0){
#     updateCheckboxGroupInput(session = session, inputId = "plots",
#                              choices = bm$metrics$description[grep("_plot",bm$metrics$name)])
#     shinyjs::html("labelText", "Label")
#   }else{
#     updateCheckboxGroupInput(session = session, inputId = "plots",
#                              choices = bm$metrics$description[grep("_plot",bm$metrics$name)],
#                              selected = bm$metrics$description[grep("_plot",bm$metrics$name)])
#     shinyjs::html("labelText", "Label")
#   }
# })
