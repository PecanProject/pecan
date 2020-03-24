tabPanel(
  "PDF Viewer",
  br(),
  column(
    3,
    DT::DTOutput("files")
  ),
  column(
    9,
    uiOutput("pdfview")
  )
)
