##### Benchmarking

# Create reactive value
bm <- reactiveValues()

##----------------------------------------------------------------------------##
## Observe when the model run is loaded and check to see if it is registered 
## as a reference run. If not, create the record upon button click

observeEvent(input$load_model,{
  tryCatch({
    req(input$all_run_id)
    ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    button <- FALSE
    if(nrow(ids_DF) == 1){
      
      # Check to see if the run has been saved as a reference run 
      ens_id <- dplyr::tbl(dbConnect$bety, 'runs') %>% dplyr::filter(id == ids_DF$runID) %>% dplyr::pull(ensemble_id)
      ens_wf <- dplyr::tbl(dbConnect$bety, 'ensembles') %>% dplyr::filter(id == ens_id) %>%
        dplyr::rename(ensemble_id = id) %>%
        dplyr::left_join(.,tbl(dbConnect$bety, "workflows") %>% dplyr::rename(workflow_id = id), by="workflow_id") %>% dplyr::collect()
      bm$model_vars <- var_names_all(dbConnect$bety,ids_DF$wID,ids_DF$runID)
      
      clean <- PEcAn.benchmark::clean_settings_BRR(inputfile = file.path(ens_wf$folder,"pecan.CHECKED.xml"))
      settings_xml <- toString(PEcAn.settings::listToXml(clean, "pecan"))
      ref_run <- PEcAn.benchmark::check_BRR(settings_xml, dbConnect$bety$con)
      
      if(length(ref_run) == 0){
        # If not registered, button appears with option to run create.BRR
        brr_message <- sprintf("Would you like to save this run (run id = %.0f, ensemble id = %0.f) as a reference run?", ids_DF$runID, ens_id) 
        button <- TRUE
      }else if(dim(ref_run)[1] == 1){
        bm$BRR <- ref_run %>% dplyr::rename(.,reference_run_id = id)
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
    bm$ready <- 0
    #Signaling the success of the operation
    toastr_success("Check for reference run")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})

# When button to register run is clicked, create.BRR is run and the button is removed.
observeEvent(input$create_bm,{
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   bm$BRR <- PEcAn.benchmark::create_BRR(bm$ens_wf, con = dbConnect$bety$con)
                   incProgress( 10/ 15)
                   bm$brr_message <- sprintf("This run has been successfully registered as a reference run (id = %.0f)", bm$BRR$reference_run_id)
                   bm$button_BRR <- FALSE
                   bm$ready <- bm$ready + 1
                   incProgress(5/15) 
                 })
    #Signaling the success of the operation
    toastr_success("Registered reference run")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
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

##----------------------------------------------------------------------------##
## Observe when the external data is loaded and check to see if any benchmarks 
## have already been run. In addition, setup and run new benchmarks.

observeEvent(input$load_data,{
  tryCatch({
    req(input$all_input_id)
    req(input$all_site_id)
    
    bm$metrics <- dplyr::tbl(dbConnect$bety,'metrics') %>% dplyr::select(one_of("id","name","description")) %>% collect()
    
    # Need to write warning message that can only use one input id
    bm$input <- getInputs(dbConnect$bety,c(input$all_site_id)) %>% 
      dplyr::filter(input_selection_list == input$all_input_id)
    format <- PEcAn.DB::query.format.vars(bety = dbConnect$bety, input.id = bm$input$input_id)
    
    # If format has Null time.row and NAs for var$column_number, skip loading
    if(is.null(format$time.row) & is.na(format$var$column_number)){
      print("File_format has Null time.row and NAs for var$column_number, skip loading")
      toastr_warning("This file format cannot do benchmarking")
    }else{
      # Are there more human readable names?
      bm$vars <- dplyr::inner_join(
        data.frame(read_name = names(bm$model_vars), 
                   pecan_name = bm$model_vars, stringsAsFactors = FALSE),
        format$vars[-grep("%",format$vars$storage_type),
                    c("variable_id", "pecan_name")],
        #format$vars[c("variable_id", "pecan_name")],  #for AmeriFlux.level2.h.nc, format$vars$storage_type is NA
        by = "pecan_name")
      
      #This will be a longer set of conditions
      bm$ready <- bm$ready + 1
      #Signaling the success of the operation
      toastr_success("Check for benchmarks")
    }
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})

observeEvent(bm$ready,{
  if(bm$ready > 0){
    bm$load_results <- 0
    bm$results_message <- "No benchmarks have been calculated yet"
    bm$blarg_message   <- "No benchmarks have been calculated yet"
    
    
    if(exists("output$results_table")) output$results_table<- NULL
    if(exists("output$bm_plots")) output$bm_plots <- NULL
    if(exists("output$bmPlot")) output$bmPlot <- NULL
    
    bm.path <- file.path(bm$ens_wf$folder, "benchmarking", as.integer(bm$input$input_id))
    bench.out <- grep("benchmarking.output.Rdata", 
                      dir(bm.path, full.names = TRUE) , value = TRUE)  # Look for benchmarking directory
    if(length(bench.out) == 1){
      bm$load_results <- bm$load_results + 1
      bm$results_message <- "Benchmarks have already been calculated for this combination of model output and external data. <br/>
      To see the results, look at the Benchmarking Scores and Benchmarking Plots tabs. <br/>
      To calculate more benchmarks, select variables and metrics below. <br/>"
    }else{
      bm$load_results <- 0
      bm$results_message <- "No benchmarks have been calculated yet"
    }
  }
})


observeEvent({
  bm$ready
  bm$metrics
  bm$vars
},{
  
  plot_ind <- grep("_plot",bm$metrics$name)
  
  variable_choices <- bm$vars$variable_id
  names(variable_choices) <- bm$vars$read_name
  metrics_choices <- bm$metrics$id[-plot_ind]
  names(metrics_choices) <- bm$metrics$description[-plot_ind]
  plot_choices <- bm$metrics$id[plot_ind]
  names(plot_choices) <- bm$metrics$description[plot_ind]
  
  output$bm_inputs <- renderUI({
    if(bm$ready > 0){
      wellPanel(
        fluidRow(
          column(4, 
                 pickerInput("vars", "Variables", 
                             choices = variable_choices,
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE, `dropup-auto` = FALSE))
          ),
          column(4, 
                 pickerInput("metrics", "Numerical Metrics", 
                             choices = metrics_choices,
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE, `dropup-auto` = FALSE))
          ),
          column(4,
                 pickerInput("plots", "Plot Metrics", 
                             choices  = plot_choices,
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE, `dropup-auto` = FALSE))
          )
        )
      )
    }
  })
  
  output$calc_bm <- renderUI({
    if(bm$ready > 0){
      fluidRow(
        column(5),
        column(2,
               shinyjs::disabled(
                 actionButton('calc_bm_button', "Calculate", icon = icon("calculator"),
                              width = "100%", class="btn-primary")
                 )
               ),
        column(5)
      )
    }
  })
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
  #output$report <- renderText(sprintf("Number of vars: %0.f, Number of metrics: %0.f", v,m))
  if(v > 0 & m > 0){
    shinyjs::enable("calc_bm_button")
    bm$bm_vars <- input$vars
    bm$bm_metrics <- c()
    if(n > 0) bm$bm_metrics <- c(bm$bm_metrics, input$metrics)
    if(p > 0) bm$bm_metrics <- c(bm$bm_metrics, input$plots)
  }
  
}, ignoreNULL = FALSE)

observeEvent(input$calc_bm_button,{
  tryCatch({
    req(input$all_input_id)
    req(input$all_site_id)
    req(input$host)
    req(input$user)
    req(input$password)
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{

                   inputs_df <- getInputs(dbConnect$bety,c(input$all_site_id)) %>% 
                     dplyr::filter(input_selection_list == input$all_input_id)
                   output$inputs_df_title <- renderText("Benchmark Input Data")
                   output$inputs_df_table <- DT::renderDataTable(
                     DT::datatable(inputs_df,
                                   rownames = FALSE,
                                   options = list(
                                     dom = 't',
                                     scrollX = TRUE,
                                     initComplete = DT::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#404040', 'color': '#fff'});",
                                       "}")))
                   )
      
                   
                   # config.list <- PEcAn.utils::read_web_config("../../web/config.php")
                   # output$config_list_table <- renderTable(as.data.frame.list(config.list))
         
                   
                   bm$bm_settings$info <- list(userid = 1000000003) # This is my user id. I have no idea how to get people to log in to their accounts through the web interface and right now the benchmarking code has sections dependent on user id - I will fix this.
                   # bm$bm_settings$database <- list(
                   #   bety = list(
                   #     user = config.list$db_bety_username,
                   #     password = config.list$db_bety_password,
                   #     host = config.list$db_bety_hostname,
                   #     dbname = config.list$db_bety_database,
                   #     driver = config.list$db_bety_type,
                   #     write = TRUE
                   #   ),
                   #   dbfiles = config.list$dbfiles_folder
                   # )
                   
                   bm$bm_settings$database <- list(
                     bety = list(
                       user = input$user,
                       password = input$password,
                       host = input$host,
                       dbname = "bety",
                       driver = "pgsql",
                       write = TRUE
                     ),
                     dbfiles = "/home/carya/output/dbfiles"
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
                 
              
                   disable("calc_bm_button")
                   output$print_bm_settings <- renderPrint(print(bm$bm_settings))
                
                   
                   basePath <- dplyr::tbl(dbConnect$bety, 'workflows') %>% dplyr::filter(id %in% bm$ens_wf$workflow_id) %>% dplyr::pull(folder)
                   
                   settings_path <- file.path(basePath, "pecan.BENCH.xml")
                   saveXML(PEcAn.settings::listToXml(bm$bm_settings,"pecan"), file = settings_path)
                   bm$settings_path <- settings_path
                   
                   output$settings_path <- renderText({
                     sprintf("Benchmarking settings have been saved here: %s", bm$settings_path)
                     })
                   incProgress(1/2) 
                   
                   ##############################################################################
                   # Run the benchmarking functions
                   # The following seven functions are essentially 
                   # "the benchmarking workflow" in its entirety  
                   
                   settings <- PEcAn.settings::read.settings(bm$settings_path)
                   bm.settings <- PEcAn.benchmark::define_benchmark(settings,dbConnect$bety)
                   settings <- PEcAn.benchmark::add_workflow_info(settings,dbConnect$bety)
                   
                   settings$benchmarking <- PEcAn.benchmark::bm_settings2pecan_settings(bm.settings)
                   settings <- PEcAn.benchmark::read_settings_BRR(settings)
                   
                   # This is a hack to get old runs that don't have the right pecan.CHECKED.xml data working
                   if(is.null(settings$settings.info)){
                     settings$settings.info <- list(
                       deprecated.settings.fixed = TRUE,
                       settings.updated = TRUE,
                       checked = TRUE
                     )
                   }
                   
                   settings <- PEcAn.settings::prepare.settings(settings)
                   settings$host$name <- "localhost" # This may not be the best place to set this, but it isn't set by any of the other functions. Another option is to have it set by the default_hostname function (if input is NULL, set to localhost)
                  # browser()
                   #results <-calc_benchmark(settings, bety = dbConnect$bety)
                   # results <- PEcAn.settings::papply(settings, function(x) calc_benchmark(x, bety, start_year = input$start_year, end_year = input$end_year))
                   results <- PEcAn.settings::papply(settings, function(x) 
                     calc_benchmark(settings = x, bety = dbConnect$bety))
                   bm$load_results <- bm$load_results + 1
                   
                   incProgress(1/2)
                 })
    #Signaling the success of the operation
    toastr_success("Calculate benchmarks")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
 
})

observeEvent(bm$results_message,{
  output$results_message <- renderText({bm$results_message})
})

observeEvent(bm$load_results,{
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   if(bm$load_results > 0){
                     load(file.path(bm$ens_wf$folder,"benchmarking",bm$input$input_id,"benchmarking.output.Rdata"))
                     incProgress(1/3) 
                     
                     bm$bench.results <- result.out$bench.results
                     bm$aligned.dat <- result.out$aligned.dat
                     plots_used <- grep("plot", result.out$bench.results$metric)
                     output$results_df_title <- renderText("Benchmark Scores")
                     output$results_table <- DT::renderDataTable(
                       DT::datatable(bm$bench.results[-plots_used,],
                                     rownames = FALSE,
                                     options = list(dom = 'ft',
                                                    initComplete = JS(
                                                      "function(settings, json) {",
                                                      "$(this.api().table().header()).css({'background-color': '#404040', 'color': '#fff'});",
                                                      "}")
                                                    )))
                     incProgress(1/3) 
                     
                     if(length(plots_used) > 0){
                       plot_list <- apply(
                         result.out$bench.results[plots_used,c("variable", "metric")],
                         1, paste, collapse = " ")
                       selection <- as.list(as.numeric(names(plot_list)))
                       names(selection) <- as.vector(plot_list)
                       output$plots_tilte <- renderText("Benchmark Plots")
                       output$bm_plots <-  renderUI({
                         selectInput("bench_plot", label = NULL, multiple = FALSE,
                                     choices = selection)
                       })
                       output$plotlybars <- renderUI({
                         div(
                           id = "plot-container",
                           div(
                             class = "plotlybars-wrapper",
                             div(
                               class = "plotlybars",
                               div(class = "plotlybars-bar b1"),
                               div(class = "plotlybars-bar b2"),
                               div(class = "plotlybars-bar b3"),
                               div(class = "plotlybars-bar b4"),
                               div(class = "plotlybars-bar b5"),
                               div(class = "plotlybars-bar b6"),
                               div(class = "plotlybars-bar b7")
                             ),
                             div(class = "plotlybars-text",
                                 p("Updating the plot. Hold tight!"))
                           )
                         )
                       })
                     }
                     incProgress(1/3) 
                   }
                   incProgress(1) 
                 })
    #Signaling the success of the operation
    toastr_success("Calculate Scores")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})

observeEvent(input$bench_plot,{
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   var <- bm$bench.results[input$bench_plot,"variable"]
                   metric_dat = bm$aligned.dat[[var]]
                   names(metric_dat)[grep("[.]m", names(metric_dat))] <- "model"
                   names(metric_dat)[grep("[.]o", names(metric_dat))] <- "obvs"
                   names(metric_dat)[grep("posix", names(metric_dat))] <- "time"
                   incProgress(2 / 15)
                   
                   fcn <- get(paste0("metric_",bm$bench.results[input$bench_plot,"metric"]), asNamespace("PEcAn.benchmark"))
                   # fcn <- paste0("metric_",bm$bench.results[input$bench_plot,"metric"])
                   args <- list(
                     metric_dat = metric_dat,
                     var = var,
                     filename = NA,
                     draw.plot = TRUE
                   )
                   p <- do.call(fcn, args)
                   incProgress(9 / 15)
                   
                   output$bmPlot <- renderPlotly({
                     plotly::ggplotly(p)%>%
                       layout(height = "100%", width = "100%")
                   })
                   incProgress(4 / 15)
                 })
    #Signaling the success of the operation
    toastr_success("Generate Plots")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
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
