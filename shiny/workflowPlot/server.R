# Load PEcAn specific packages, does this need to be so specific?
lapply(c("PEcAn.visualization",
         "PEcAn.DB",
         "PEcAn.settings",
         "PEcAn.benchmark",
         "PEcAn.utils"),function(pkg){
           library(pkg,character.only = TRUE,quietly = TRUE)
         }
      )


# Shiny and plotting packages
lapply(c( "shiny",
          "plotly",
          "highcharter",
          "shinyjs",
          "dplyr",
          "plyr",
          "stringr",
          "XML",
          "xts",
          "purrr",
          "lubridate",
          "listviewer",
          "shinythemes",
          "shinytoastr",
          "shinyFiles",
          "data.table",
          "shinyWidgets"
          ),function(pkg){
            if (!(pkg %in% installed.packages()[,1])){
                  install.packages(pkg)
               }
                library(pkg,character.only = TRUE,quietly = TRUE)
            }
      )


# Maximum size of file allowed to be uploaded: 100MB
options(shiny.maxRequestSize=100*1024^2)

# Port forwarding
# options(shiny.port = 6438)
# options(shiny.launch.browser = 'FALSE')

# Define server logic
server <- shinyServer(function(input, output, session) {

  dbConnect <- reactiveValues(bety = NULL)

  # Try `betyConnect` function.
  # If it breaks, ask user to enter user, password and host information
  # then use the `db.open` function to connect to the database
  tryCatch({
    #dbConnect$bety <- betyConnect()
    #For betyConnect to break to test shiny modal
    dbConnect$bety <- betyConnect(".")
  },
  error = function(e){

    #---- shiny modal----
    showModal(
      modalDialog(
        title = "Connect to Database",
        fluidRow(column(12,textInput('user', h4('User:'), width = "100%", value = "bety"))),
        fluidRow(column(12,textInput('password', h4('Password:'), width = "100%", value = "bety"))),
        fluidRow(column(12,textInput('host', h4('Host:'), width = "100%", value = "psql-pecan.bu.edu"))),
        fluidRow(
          column(3),
          column(6,br(),actionButton('submitInfo', 'Submit', width = "100%", class="btn-primary")),
          column(3)
        ),
        footer = NULL,
        size = 's'
      )
    )

    # --- connect to database ---
    observeEvent(input$submitInfo,{
      tryCatch({

          dbConnect$bety <- PEcAnDB::db.open(
            params = list(
            driver = "Postgres",
              dbname ='bety' ,
              host =input$host,
              user = input$user,
              password = input$password
            )
          )

          # For testing reactivity of bety connection
          #dbConnect$bety <- betyConnect()

          removeModal()
          toastr_success("Connect to Database")
        },
        error = function(e) {
          toastr_error(title = "Error", conditionMessage(e))
        }
      )
    })
  })


  # Hiding the animation and showing the application content
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  showElement("app")

  # Source Extra Funtions
  source("workflowPlot_fcns.R", local = TRUE) # Load all functions that need to be defined for this script

  # Sidebar
  source("server_files/sidebar_server.R", local = TRUE)

  # Page 1: Select Data
  source("server_files/select_data_server.R", local = TRUE)

  # Page 2: History Runs
  source("server_files/history_server.R", local = TRUE)

  # Page 3: Exploratory Plots
  source("server_files/model_plots_server.R", local = TRUE)
  source("server_files/model_data_plots_server.R", local = TRUE)
  source("server_files/pdf_viewer_server.R", local = TRUE)

  # Page 4: Benchmarking
  observeEvent(input$load_model,{
    req(input$all_run_id)
    ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    button <- FALSE
    print(nrow(ids_DF))
    if(nrow(ids_DF) == 1){
      source("server_files/benchmarking_server.R", local = TRUE)
    }else if(nrow(ids_DF) > 1){
      brr_message <- "Benchmarking currently only works when one run is selected."
    }else{
      brr_message <- "Cannot do benchmarking"
    }
  })

  }) # Shiny server closes here
