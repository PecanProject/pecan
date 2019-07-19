tabPanel(
  "PDF Viewer",
  br(),
  column(
    4,
    DT::DTOutput("files")
  ),
  column(
    8,
    uiOutput("pdfview")
  )
)