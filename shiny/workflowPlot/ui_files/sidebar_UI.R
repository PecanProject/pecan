tagList(
  h4("Load Model Output"),
  wellPanel(
    p("Please select the workflow IDs to continue. You can select multiple IDs"),
    selectizeInput("all_workflow_id", "Mutliple Workflow IDs", c(), multiple=TRUE),
    p("Please select the run IDs. You can select multiple IDs"),
    selectizeInput("all_run_id", "Mutliple Run IDs", c(), multiple=TRUE),
    fluidRow(
      column(6,
               actionButton("NewRun", "New Run", icon = icon("plus"),
                            width = "120%", class="btn-primary")
         
             ),
      column(6,
                  actionButton("load_model", "Load", icon = icon("download"), width = "100%")
             )
    )
  ),
  h4("Load External Data"),
  wellPanel(
    selectizeInput("all_site_id", "Select Site ID", c()),
    # If loading multiple sites in future
    # selectizeInput("all_site_id", "Select Site ID", c(), multiple=TRUE),
    selectizeInput("all_input_id", "Select Input ID", c()),
    fluidRow(
     column(6,
             actionButton("register_data", "Register", icon = icon("upload"),
                          width = "120%", class="btn-primary")
             ),
      column(6,
             actionButton("load_data", "Load", icon = icon("download"), width = "100%")
             )
    )
  )
)
