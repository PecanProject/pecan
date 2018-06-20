fluidRow(title = "New Input",
   box(title = h2("New Input"), width = 4,
     hr(),
     selectizeInput("InputSiteID", label = "Site ID (under construction)", names(States)),
     hr(),
     selectizeInput("InputParentID", label = "Parent ID (under construction)", names(States)),
     hr(),
     textInput(
       "InputName",
       label = "Name",
       placeholder = "Not sure what goes here yet"
     ),
     hr(),
     selectizeInput("InputFormatID", label = "Format ID (under construction)", names(States))
   ),
   box(title = h2("DbFiles Record"), width = 4,
       hr(),
       selectizeInput("InputMachineID", label = "Machine ID (under construction)", names(States))
       ),
   box(title = h2("Format ID"), width = 4,
       hr(),
       selectizeInput("MimetypeID", label = "Mimetype ID (under construction)", names(States))
       )
  
 )

