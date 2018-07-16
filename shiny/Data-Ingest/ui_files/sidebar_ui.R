sidebarMenu(
  menuItem(
    "Homepage",
    tabName = "Home",
    icon = icon("home", lib = "font-awesome")
  ),
  menuItem(
    "Ingest Workflow",
    tabName = "ingestWorkflow",
    icon = icon("database", lib = "font-awesome")
  ),
  menuItem(
    actionBttn("d1Input", label = "Import from DataONE",
               icon = icon("download", lib = "font-awesome"),
               size = "sm", color = "success")
  ),
  menuItem(
    actionBttn("lclUpload", label = "Upload Local Files",
               icon = icon("upload", lib = "font-awesome"),
               size = "sm", color = "success")
  )
)
