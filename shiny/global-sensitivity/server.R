library(PEcAn.DB)
library(PEcAn.visualization)
library(ncdf4)

source("plotEnsemble.R")
source("load_ensemble.R")

message("Debugging!")
message("Starting shiny server...")
# Define server logic
server <- shinyServer(function(input, output, session) {
    message("Trying to connect to BETY...")  
    bety <- betyConnect()
    # set the workflow id(s)
    message("Getting workflow IDs...")
    ids <- get_workflow_ids(bety, session)
    updateSelectInput(session, "workflow_id", choices=ids)
    workflow_id <- reactive({
        req(input$workflow_id)
        workflow_id <- input$workflow_id
    })
    
    message("Getting run IDs...")
    run_ids <- reactive(get_run_ids(bety, workflow_id()))
    
    var_names <- reactive({
        run_ids <- get_run_ids(bety, workflow_id())
        var_names <- get_var_names(bety, workflow_id(), run_ids[1])
        return(var_names)
    })
    
    param_names <- reactive({
        workflow_dir <- current_workflow()$folder
        load(file.path(workflow_dir, "samples.Rdata"))
        pft.names <- names(ensemble.samples) %>% .[. != "env"]
        param_names <- paste0(pft.names, ".", colnames(ensemble.samples[[1]]))
        return(param_names)
    })
    
    current_workflow <- reactive({
        id <- workflow_id()
        current_workflow <- collect(workflow(bety, id))
        return(current_workflow)
    })
    
    # Update variable and parameter names based on current workflow ID
    observe({
        updateSelectInput(session, "parameter", choices=param_names())
        updateSelectInput(session, "variable", choices=var_names())
    })
    
    message("Loading ensemble output...")
    ensemble.out <- reactive({
        req(current_workflow())
        workflow <- current_workflow()
        if(nrow(workflow) > 0) {
            workflow_dir <- workflow$folder
            output_dir <- file.path(workflow_dir, "out")
            settings <- XML::xmlToList(XML::xmlParse(file.path(workflow_dir, "pecan.CHECKED.xml")))
            # Load ensemble samples
            ensemble.out <- load_ensemble(workflow_dir = workflow_dir, settings = settings, variable = var_names())
            return(ensemble.out)
        } else {
            return(NA)
        }
    })
    
    output$ensemble_plot <- renderPlot({
        req(ensemble.out())
        plot_cols <- 3
        if(input$output_type == "Pairwise"){
            plotEnsemble(ensemble.out(), input$parameter, input$variable)
        } else if (input$output_type == "All parameters"){
            plotAllParams(ensemble.out(), input$variable, param_names(), plot_cols=plot_cols)
        } else if (input$output_type == "All variables"){
            plotAllVars(ensemble.out(), input$parameter, var_names(), plot_cols=plot_cols)
        }
        add_icon()
    })
    
    lm_fit <- reactive({
        req(ensemble.out())
        fitSummary(ensemble.out(), input$parameter, input$variable)
    })
    
    output$coef_table <- renderTable(lm_fit()$coefs, digits = 4, display = c("s", rep("g", 4)))
    
    output$r2 <- renderText({
        with(lm_fit(), sprintf("R2 = %.3g, Adj. R2 = %.3g", r2, adjr2))
    })
})  # End shinyServer


# runApp(port=5658, launch.browser=FALSE)
