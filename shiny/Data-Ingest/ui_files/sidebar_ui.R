sidebarMenu(
  # menuItem(
  #   "Select Data Acquisition Method",
  #   tabName = "importDataONE",
  #   icon = icon("file")
  # ),
  menuItem(
    "Import from DataONE",
    tabName = "importDataONE",
    icon = icon("download", lib = "font-awesome")
  ),
  menuItem(
    "Upload Local Files",
    tabName = "uploadLocal",
    icon = icon("upload", lib = "font-awesome")
  ),
  menuItem(
    "Step 2 -- New Input",
    tabName = "step2",
    icon = icon("database", lib = "font-awesome")
  ),
  menuItem(
    "Step 3 -- format record",
    tabName = "step3",
    icon = icon("cog")
  ),
  menuItem("Step 4 -- etc.", 
           tabName = "step4", 
           icon = icon("cog")
  )
)
