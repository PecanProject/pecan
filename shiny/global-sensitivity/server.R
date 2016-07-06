library(shiny)
library(ncdf4)
library(ggplot2)

source("helper.R")
source("plotEnsemble.R")
source("load_ensemble.R")

# Define server logic
server <- shinyServer(function(input, output, session) {
    bety <- betyConnect()
    logger.setLevel("OFF")
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    print("RESTART")
    
    # set the workflow id(s)
    ids <- get_workflow_ids(bety, session)
    default_workflow_id <- 1000001488    # Working default
    updateSelectInput(session, "workflow_id", choices=ids, selected=default_workflow_id)
    workflow_id <- reactive({
        req(input$workflow_id)
        workflow_id <- input$workflow_id
    })
    
    run_ids <- reactive(get_run_ids(bety, workflow_id()))
    
    var_names <- reactive({
        run_ids <- get_run_ids(bety, workflow_id())
        var_names <- get_var_names(bety, workflow_id(), run_ids[1])
        var_names <- var_names[!grepl("pool", var_names, ignore.case = TRUE)]  ## Ignore "poolnames" and "carbon pools" variables
        return(var_names)
    })
    
    current_workflow <- reactive({
        id <- workflow_id()
        current_workflow <- collect(workflow(bety, id))
        return(current_workflow)
    })
    
    # Update variable names based on current workflow ID
    observe({
        workflow_dir <- current_workflow()$folder
        load(file.path(workflow_dir, "samples.Rdata"))
        pft.names <- names(ensemble.samples) %>% .[. != "env"]
        param_names <- paste0(pft.names, ".", colnames(ensemble.samples[[1]]))
        updateSelectInput(session, "x_variable", choices=param_names)
        updateSelectInput(session, "y_variable", choices=var_names())
    })
    
    ensemble.out <- reactive({
        workflow <- current_workflow()
        if(nrow(workflow) > 0) {
            workflow_dir <- workflow$folder
            output_dir <- file.path(workflow_dir, "out")
            settings <- xmlToList(xmlParse(file.path(workflow_dir, "pecan.xml")))
            # Load ensemble samples
            ensemble.out <- load_ensemble(workflow_dir = workflow_dir, settings = settings, variable = var_names())
            return(ensemble.out)
        } else {
            return(NA)
        }
    })
    
    output$ensemble_plot <- renderPlot({
        plotEnsemble(ensemble.out(), input$x_variable, input$y_variable)
    })
    
})  # End shinyServer


# runApp(port=5658, launch.browser=FALSE)
