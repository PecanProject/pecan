box(
  title = h2("1. Input Record"), width = 3, collapsible = TRUE, 
  hr(),
  selectizeInput("InputSiteName", label = "Site *", choices = NULL,
    options = list(
      placeholder = 'Please search or select a site below',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  hr(),
  selectizeInput("InputParentName", label = "Parent", choices = NULL,
    options = list(
      placeholder = 'Please search inputs by name or site',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  hr(),
  textInput("InputName",
            label = "Name *",
            placeholder = ""),
  hr(),
  selectizeInput("InputFormatName", label = "Format *", choices = NULL,
    options = list(
      placeholder = 'Please search Formats by name',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  hr(),
  dateInput(
    "InputStartDate",
    label = "Start Date",
    format = "yyyy-mm-dd",
    startview = "decade"
  ),
  shinyTime::timeInput("StartTimeInput",
                       label = "Start Time (Hours - Minutes) *",
                       seconds = FALSE),
  dateInput(
    'InputEndDate',
    label = 'End Date',
    format = 'yyyy-mm-dd',
    startview = 'decade'
  ),
  shinyTime::timeInput("EndTimeInput",
                       label = "End Time (Hours-Minutes) *",
                       seconds = FALSE),
  textInput("Timezone",
            label = "Timezone (UTC) *",
            placeholder = "UTC +/-"),
  hr(),
  textAreaInput("InputNotes",
                label = "Notes",
                height = '150px'),
  actionButton("createInput", label = "Create Input"),
  # Not sure if I want this here or only available when all forms are filled in.
  p("* Denotes a Required Field"),
  hr(),
  verbatimTextOutput("summInputs")
  )