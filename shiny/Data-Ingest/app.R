 library(PEcAn.data.land)
 library(PEcAn.visualization)
 library(PEcAn.utils)
 library(shinydashboard)
 library(dataone)
 library(stringr)
 library(DT)
 library(shiny)
 
 source("ui_utils.R", local = TRUE)

#############################################################################
################################## UI #######################################
#############################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Data Ingest Workflow"), 
  dashboardSidebar(
    source_ui("sidebar_ui.R")
  ),
  dashboardBody(
    tabItems(
    ## Tab 1 -- DataONE download
    tabItem(tabName = "importDataONE",
            source_ui("d1_download_ui.R")
            ),
    ## Tab 2 -- Local File Upload
    tabItem(tabName = "uploadLocal",
            source_ui("local_file_upload_ui.R")
            ),
    ## Next Steps
    tabItem(tabName = "step2",
            source_ui("input_record_ui.R")
            ),
    
    tabItem(tabName = "step3",
            h2("under construction")),
    
    tabItem(tabName = "step4",
            h2("under construction"))
    
  )),
  title = "PEcAn Data Ingest",
  skin =  "green"
)
#######################################################################################################
######################################### SERVER ######################################################
#######################################################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2) #maximum file input size
  
  ## Setup ##
  Shared.data <- reactiveValues(downloaded=NULL)
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  
  ##################### DataONE Download #################################
  source("server_files/d1_download_svr.R", local = TRUE)
  
  ######### FileInput ########################################  source("server_files/local_upload_svr.R", local = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87