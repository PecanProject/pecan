sidebarPanel(
  h3("Load Model Output"),
  wellPanel(
    p("Please select the workflow IDs to continue. You can select multiple IDs"),
    selectizeInput("all_workflow_id", "Mutliple Workflow IDs", c(), multiple=TRUE),
    p("Please select the run IDs. You can select multiple IDs"),
    selectizeInput("all_run_id", "Mutliple Run IDs", c(), multiple=TRUE),
    actionButton("load_model", "Load Model outputs")
  ),
  h3("Load External Data"),
  wellPanel(
    selectizeInput("all_site_id", "Select Site ID", c()),
    # If loading multiple sites in future
    # selectizeInput("all_site_id", "Select Site ID", c(), multiple=TRUE),
    selectizeInput("all_input_id", "Select Input ID", c()),
    actionButton("load_data", "Load External Data")
  )
)