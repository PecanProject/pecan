dashboardPage(
  dashboardHeader(title = "PEcAn dependencies graphs"),
  dashboardSidebar(sidebarMenu(id = "Tabsetpan",
                               menuItem("Packages", tabName = "Packages", icon = icon("archive")),
                               conditionalPanel(condition = "input.Tabsetpan === 'Packages'",
                                                selectInput('packages', "Package(s) :", choices = installed.packages()[,1]%>%grep("^PEcAn",.,value = T,ignore.case = T), multiple = T, width = "100%"),
                                                div(actionButton("GOPackage", "Go !",icon = icon("line-chart")), align = "center")
                               ),
                               menuItem("Functions", tabName = "Functions", icon = icon("code")),
                               conditionalPanel(condition = "input.Tabsetpan === 'Functions'",
                                                selectInput('package', "Package : ", choices = installed.packages()[,1]%>%grep("^PEcAn",.,value = T,ignore.case = T), multiple = FALSE, width = "100%"),
                                                div(actionButton("GOFunc2", "Go !",icon = icon("line-chart")), align = "center")
                               ),
                               menuItem("Custom", tabName = "Custom", icon = icon("th"))
                               
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css')),
    tabItems(
      # First tab content
      tabItem(tabName = "Packages",
              # fluidRow(
              #   column(3, div(h3('Package(s) selection :'), align = "center")),
              #   column(6, br(), selectInput('packages', NULL, choices = installed.packages()[,1], multiple = T, width = "100%")),
              #   column(3, br(), div(actionButton("GOPackage", "Launch",icon = icon("line-chart")), align = "center"))
              # ),
              # hr(),
              
              fluidRow(
                box(
                  solidHeader = TRUE, collapsible = TRUE, title = "Dependencies between package(s)",
                  status = "primary",
                  checkboxGroupInput("variablesp", "Dependencies to show:",
                                     c("Imports" = "Imports",
                                       "Suggests" = "Suggests",
                                       "Depends" = "Depends"),selected = "Depends"),
                  visNetworkOutput("main_plot", width = "100%",height = "750px"),
                  br()
                  ,width = 12
                ),
                box(
                  solidHeader = TRUE, collapsible = TRUE, title = "Informations",
                  status = "primary",
                  div(
                    dataTableOutput("tabledep"),
                    uiOutput("Groupebutton"),
                    align="center"
                  ), 
                  width=12)
              )
              
      ),
      tabItem(tabName = "Functions",
              fluidRow(
                box(
                  solidHeader = TRUE, collapsible = TRUE, title = "Dependencies between functions",
                  status = "primary",
                  div(h4(textOutput("zoomin")), align = "center"),
                  visNetworkOutput("main_plot1", width = "100%",height = "750px"),
                  br()
                  ,width = 12
                ),
                box(
                  solidHeader = TRUE, collapsible = TRUE, title = "Informations",
                  status = "primary",
                  div(
                    # h4(textOutput("info")),
                    dataTableOutput("datatable2")
                    ,align="center"
                  ),
                  width=12)
              ),
              
              fluidRow(
                box(
                  uiOutput("help"),width = 12
                )
              )
      ),
      #                 tabPanel("Script",
      #                          
      #                          
      #                          fluidRow(
      #                            box(
      #                              fileInput('file1', 'Choose R File',
      #                                        accept=NULL),
      #                              visNetworkOutput("plotscript", width = "100%",height = "700px")
      #                              ,width = 12)
      #                          )
      #                 ),
      
      tabItem(tabName = "Custom",
              
              
              fluidRow(
                box(
                  fluidRow(
                    column(width=4,
                           pickerInput(inputId = "packageslist" , "Package(s) :", choices = installed.packages()[,1]%>%grep("^PEcAn",.,value = T,ignore.case = T), multiple = TRUE,
                                       options = list(
                                         `actions-box` = TRUE,
                                         `live-search` = TRUE)
                           )
                    ),
                    column(width=2, 
                           br(), div(actionButton("chargedf", "Find functions", style = "padding: 8px 20px 8px 20px;"),align="center")
                    ),
                    column(width=4,
                           #selectizeInput(inputId = "functionlist" , "Function(s) :", choices = NULL, multiple = TRUE),
                           pickerInput(
                             inputId = "functionlist",
                             label = "Function(s) :", 
                             choices = NULL,
                             options = list(
                               `actions-box` = TRUE,
                               `live-search` = TRUE), 
                             multiple = TRUE
                           )
                    ),
                    column(width=2, 
                           br(), div(actionButton("makegraph", "Make graph", style = "padding: 8px 20px 8px 20px;"),align = "center")
                    )
                  ),
                  fluidRow(
                    column(4,checkboxInput('igraphcheck','Igraph Layout (more stable layout)',value = F)),
                    column(4, actionButton("showsourcebtn", "Show the source", style = "padding: 8px 20px 8px 20px;")),
                    column(4)
                    
                  ),
                 
                  hr(),
                  visNetworkOutput("chossefunctionplot", width = "100%",height = "750px"),
                  br(),
                  width = 12)
              )
      )
      
      
    )
  )
)



