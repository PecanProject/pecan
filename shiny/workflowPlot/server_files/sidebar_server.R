# The sidebar is where both the model output and external data are loaded

# Loading Model Output(s) -----------------------------------------------------#


# Update workflow ids
observe({
  tryCatch({
    # get_workflow_ids function (line 137) in db/R/query.dplyr.R takes a flag to check
    # if we want to load all workflow ids.
    # get_workflow_id function from query.dplyr.R
    all_ids <- get_workflow_ids(dbConnect$bety, query, all.ids=TRUE)
    selectList <- as.data.table(all_ids)
    
    updateSelectizeInput(session,
                         "all_workflow_id",
                         choices = all_ids,
                         server = TRUE)
    # Get URL prameters
    query <- parseQueryString(session$clientData$url_search)
    
    # Pre-select workflow_id from URL prams
   if(length(query)>0) updateSelectizeInput(session, "all_workflow_id", selected = query[["workflow_id"]])
    #Signaling the success of the operation
    toastr_success("Update workflow IDs")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})

# Update run ids
all_run_ids <- reactive({
  # Retrieves all run ids for seleted workflow ids
  # Returns ('workflow ',w_id,', run ',r_id)
  req(input$all_workflow_id)
  w_ids <- input$all_workflow_id
  # Will return a list
  run_id_list <- c()
  for(w_id in w_ids){
    # For all the workflow ids
    r_ids <- get_run_ids(dbConnect$bety, w_id)
    for(r_id in r_ids){
      # Each workflow id can have more than one run ids
      # ',' as a separator between workflow id and run id
      list_item <- paste0('workflow ', w_id,', run ', r_id)
      run_id_list <- c(run_id_list, list_item)
    }
  }
  return(run_id_list)
})
# Update all run_ids ('workflow ',w_id,', run ',r_id)
observeEvent(input$all_workflow_id,{
  tryCatch({
    updateSelectizeInput(session, "all_run_id", choices = all_run_ids())
    # Get URL parameters
    query <- parseQueryString(session$clientData$url_search)
    # Make the run_id string with workflow_id
    url_run_id <- paste0('workflow ', query[["workflow_id"]],', run ', query[["run_id"]])
    # Pre-select run_id from URL params
    updateSelectizeInput(session, "all_run_id", selected = url_run_id)
    #Signaling the success of the operation
    toastr_success("Update run IDs")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})


# Loads data for all workflow and run ids after the load button is pressed.
# All information about a model is contained in 'all_run_id' string
# Wrapper over 'load_data_single_run' in PEcAn.db::query.dplyr
# Model data different from observations data
load.model <- eventReactive(input$load_model,{
  req(input$all_run_id)
  # Get IDs DF from 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  globalDF <- map2_df(ids_DF$wID, ids_DF$runID,
                      ~tryCatch({
                        load_data_single_run(dbConnect$bety, .x, .y)
                      },
                      error = function(e){
                        toastr_error(title = paste("Error in WorkflowID", .x), 
                                     conditionMessage(e))
                        return()
                      }))
  print("Yay the model data is loaded!")
  print(head(globalDF))
  globalDF$var_name <- as.character(globalDF$var_name)
  globalDF$run_id <- as.factor(as.character(globalDF$run_id))
  return(globalDF)
})

# Update all variable names
observeEvent(input$load_model, {
  req(input$all_run_id)
  # All information about a model is contained in 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  var_name_list <- c()
  for(row_num in 1:nrow(ids_DF)){
    var_name_list <- c(var_name_list, 
                       tryCatch({
                         var_names_all(dbConnect$bety, ids_DF$wID[row_num], ids_DF$runID[row_num])
                       },
                       error = function(e){
                         return(NULL)
                       }))
  }
  updateSelectizeInput(session, "var_name_model", choices = var_name_list)
})

observeEvent(input$load_model,{
  # Retrieves all site ids from multiple seleted run ids when load button is pressed
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  site_id_list <- c()
  for(row_num in 1:nrow(ids_DF)){
    settings <- 
      tryCatch({
        getSettingsFromWorkflowId(dbConnect$bety,ids_DF$wID[row_num])
      },
      error = function(e){
        return(NULL)
      })
    site.id <- c(settings$run$site$id)
    site_id_list <- c(site_id_list,site.id)
  }
  updateSelectizeInput(session, "all_site_id", choices=site_id_list)
})
# Update input id list as (input id, name)
observe({
  req(input$all_site_id)
  inputs_df <- getInputs(dbConnect$bety, c(input$all_site_id))
  formats_1 <- dplyr::tbl(dbConnect$bety, 'formats_variables') %>%
    dplyr::filter(format_id %in% inputs_df$format_id)
  if (dplyr.count(formats_1) == 0) {
    logger.warn("No inputs found. Returning NULL.")
    return(NULL)
  } else {
    formats_sub <- formats_1 %>%
      dplyr::pull(format_id) %>%
      unique()
    inputs_df <- inputs_df %>% dplyr::filter(format_id %in% formats_sub) # Only data sets with formats with associated variables will show up
    updateSelectizeInput(session, "all_input_id", choices=inputs_df$input_selection_list)
  }
})

load.model.data <- eventReactive(input$load_data, {

  req(input$all_input_id)
  
  inputs_df <- getInputs(dbConnect$bety,c(input$all_site_id))
  inputs_df <- inputs_df %>% dplyr::filter(input_selection_list == input$all_input_id)
  
  input_id <- inputs_df$input_id
  # File_format <- getFileFormat(bety,input_id)
  File_format <- PEcAn.DB::query.format.vars(bety = dbConnect$bety, input.id = input_id)
  start.year <- as.numeric(lubridate::year(inputs_df$start_date))
  end.year <- as.numeric(lubridate::year(inputs_df$end_date))
  File_path <- inputs_df$filePath

  # TODO There is an issue with the db where file names are not saved properly.
  # To make it work with the VM, uncomment the line below
  #File_path <- paste0(inputs_df$filePath,'.csv')
  site.id <- inputs_df$site_id
  site <- PEcAn.DB::query.site(site.id,dbConnect$bety$con)
  
  observations <- PEcAn.benchmark::load_data(
    data.path = File_path, format = File_format, time.row = File_format$time.row,
    site = site, start_year = start.year, end_year = end.year)
  # Manually select variables to deal with the error
  # observations <- PEcAn.benchmark::load_data(
  #   data.path = File_path, format = File_format,
  #   site = site, start_year = start.year, end_year = end.year,
  #   vars.used.index = c(1,2,3,5,6,7,9,10,12,13,14,15,16,19))
  print("Yay the observational data is loaded!")
  print(head(observations))
  return(observations)
})


# Update all variable names
observeEvent(input$load_data, {
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   model.df <- load.model()
                   incProgress(7 / 15)
                   obvs.df <- load.model.data()
                   incProgress(7 / 15)
                   updateSelectizeInput(session, "var_name_modeldata",
                                        choices = intersect(model.df$var_name, names(obvs.df)))
                   incProgress(1 / 15)
                 })
    #Signaling the success of the operation
    toastr_success("Update variable names")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})

# These are required for shinyFiles which allows to select target folder on server machine
volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
shinyDirChoose(input, "regdirectory", roots = volumes, session = session, restrictions = system.file(package = "base"))


output$formatPreview <- DT::renderDT({
  req(input$format_sel_pre)
  tryCatch({
    Fids <-
      PEcAn.DB::get.id("formats",
                       "name",
                       input$format_sel_pre,
                       dbConnect$bety$con) %>%
      as.character()
    
    if (length(Fids) > 1)
      toastr_warning(title = "Format Preview",
                     message = "More than one id was found for this format. The first one will be used.")
    
    mimt<-tbl(dbConnect$bety$con,"formats") %>%
      left_join(tbl(dbConnect$bety$con,"mimetypes"), by=c('mimetype_id'='id'))%>%
      dplyr::filter(id==Fids[1]) %>%
      dplyr::pull(type_string)
    
    output$mimt_pre<-renderText({
      mimt
    })
    
    DT::datatable(
      tbl(dbConnect$bety$con, "formats_variables")  %>%
        dplyr::filter(format_id == Fids[1]) %>%
        dplyr::select(-id, -format_id,-variable_id,-created_at,-updated_at) %>%
        dplyr::filter(name != "") %>%
        collect(),
      escape = F,
      filter = 'none',
      selection = "none",
      style = 'bootstrap',
      rownames = FALSE,
      options = list(
        autowidth = TRUE,
        columnDefs = list(list(
          width = '90px', targets = -1
        )),
        #set column width for action button
        dom = 'tp',
        pageLength = 10,
        scrollX = TRUE,
        scrollCollapse = FALSE,
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      )
    )
    
    
    
  },
  error = function(e) {
    toastr_error(title = "Error in format preview", message = conditionMessage(e))
  })
})  

# Register external data
observeEvent(input$register_data,{
  #browser()
  req(input$all_site_id)
  
  showModal(
    modalDialog(
      title = "Register External Data",
      tabsetPanel(
        tabPanel("Register",
                 br(),
                 fluidRow(
                   column(6,
                          fileInput("Datafile", "Choose CSV/NC File",
                                    width = "100%",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",
                                      ".nc")
                          )),
                   column(6,br(), 
                          shinyFiles::shinyDirButton("regdirectory", "Choose your target dir", "Please select a folder")
                   ),
                   tags$hr()
                 ),
                 fluidRow(
                   column(6, dateInput("date3", "Start Date:", value = Sys.Date()-10)),
                   column(6, dateInput("date4", "End Date:", value = Sys.Date()-10) )
                 ),tags$hr(),
                 fluidRow(
                   column(6, shinyTime::timeInput("time2", "Start Time:", value = Sys.time())),
                   column(6, shinyTime::timeInput("time2", "End Time:", value = Sys.time()))
                 ),tags$hr(),
                 fluidRow(
                   column(6, selectizeInput("format_sel", "Format Name", tbl(dbConnect$bety,"formats") %>%
                                              pull(name) %>% 
                                              unique()
                                            ) ),
                   column(6)
                 )
                 ),
        tabPanel("Fromat Preview", br(),
                 fluidRow(
                   column(6,selectizeInput("format_sel_pre", "Format Name", tbl(dbConnect$bety,"formats") %>%
                                             pull(name) %>% unique())),
                   column(6, h5(shiny::tags$b("Mimetypes")), textOutput("mimt_pre"))
                 ),
                 fluidRow(
                   column(12,
                          DT::dataTableOutput("formatPreview") 
                          )
                   
                 )
                 )
      ),
      footer = tagList(
        actionButton("register_button", "Register", class="btn-primary"),
        modalButton("Cancel")
      ),
      size = 'l'
    )
  )
})

# register input file in database
observeEvent(input$register_button,{
  tryCatch({
    inFile <- input$Datafile
    dir.name <- gsub(".[a-z]+", "", inFile$name)
    dir.create(file.path(parseDirPath(volumes, input$regdirectory), dir.name))
    file.copy(inFile$datapath,
              file.path(parseDirPath(volumes, input$regdirectory), dir.name, inFile$name),
              overwrite = T)

    mt <- tbl(dbConnect$bety,"formats") %>%
      left_join(tbl(dbConnect$bety,"mimetypes"), by = c("mimetype_id" = "id")) %>%
      filter(name == input$format_sel) %>%
      pull(type_string)

    PEcAn.DB::dbfile.input.insert(in.path = file.path(parseDirPath(volumes, input$regdirectory), dir.name),
                                  in.prefix = inFile$name,
                                  siteid =   input$all_site_id, # select box
                                  startdate = input$date3,
                                  enddate =   input$date4,
                                  mimetype = mt,
                                  formatname = input$format_sel,
                                  #parentid = input$parentID,
                                  con = dbConnect$bety$con
                                  #hostname = localhost #?, #default to localhost for now
                                  #allow.conflicting.dates#? #default to FALSE for now
    )
    removeModal()
    toastr_success("Register External Data")
  },
  error = function(e){
    toastr_error(title = "Error", conditionMessage(e))
  })
})

# update input id list when register button is clicked
observeEvent(input$register_button,{
  req(input$all_site_id)
  inputs_df <- getInputs(dbConnect$bety, c(input$all_site_id))
  formats_1 <- dplyr::tbl(dbConnect$bety, 'formats_variables') %>%
    dplyr::filter(format_id %in% inputs_df$format_id)
  if (dplyr.count(formats_1) == 0) {
    logger.warn("No inputs found. Returning NULL.")
    return(NULL)
  } else {
    formats_sub <- formats_1 %>%
      dplyr::pull(format_id) %>%
      unique()
    inputs_df <- inputs_df %>% dplyr::filter(format_id %in% formats_sub) # Only data sets with formats with associated variables will show up
    updateSelectizeInput(session, "all_input_id", choices=inputs_df$input_selection_list)
  }
})
