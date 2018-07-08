 library(PEcAn.data.land)
 library(PEcAn.visualization)
 library(PEcAn.utils)
 library(shinydashboard)
 library(dataone)
 library(stringr)
 library(DT)
 library(shiny)
 
 source("ui_utils.R", local = TRUE)
 
 ##### Bety Calls ######
 bety <- betyConnect()
 
 sites <- dplyr::tbl(bety, "sites") %>% dplyr::select(sitename, id) %>% dplyr::arrange(sitename)
 sitenames <- sites %>% pull(sitename)
 
 inputs <- dplyr::tbl(bety, "inputs") %>% dplyr::select(name, id) %>% dplyr::arrange(name)
 input_names <- inputs %>% pull(name)
 
 formats <- dplyr::tbl(bety, "formats") %>% distinct(name) %>% dplyr::arrange(name) %>% pull(name)
 formats_sub <- dplyr::tbl(bety, "formats") %>% dplyr::select(name, id) %>% dplyr::arrange(name)
 
 machines <- dplyr::tbl(bety, "machines") %>% distinct(hostname) %>% dplyr::arrange(hostname)%>% pull(hostname)
 machines_sub <- dplyr::tbl(bety, "machines") %>% dplyr::select(hostname, id) %>% dplyr::arrange(hostname)
 
 mimetypes <- dplyr::tbl(bety, "mimetypes") %>% distinct(type_string) %>% dplyr::arrange(type_string) %>% pull(type_string)
 mimetype_sub <- dplyr::tbl(bety, "mimetypes") %>% dplyr::select(type_string, id) %>% dplyr::arrange(type_string)


#############################################################################
################################## UI #######################################
#############################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Data Ingest Workflow"), 
  dashboardSidebar(
    source_ui("ui_files", "sidebar_ui.R")
  ),
  dashboardBody(
    tabItems(
    ## Tab 1 -- Landing Page
    tabItem(tabName = "Home",
            source_ui("ui_files", "homepage_ui.R")
            ),
    ## Tab 2 -- DataONE download
    tabItem(tabName = "importDataONE",
            source_ui("ui_files", "d1_download_ui.R")
            ),
    ## Tab 3 -- Local File Upload
    tabItem(tabName = "uploadLocal",
            source_ui("ui_files", "local_file_upload_ui.R")
            )
    # ## Next Steps
    # tabItem(tabName = "step2",
    #         h2("Modularization in progress. This tab will eventually be deprecated.")
    #        # source_ui("ui_files", "input_record_ui.R")
    #         )
  )),
  title = "PEcAn Data Ingest",
  skin =  "green"
)
####################################################################################
################################ SERVER ############################################
####################################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2) #maximum file input size
  
  ## Setup ##
  Shared.data <- reactiveValues(downloaded=NULL)
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  
  ## Create lists called in inputs, dbfiles, and formats record svr ##
  inputsList <- list()
  dbFilesRecordList <- list()
  FormatRecordList <- list()
  
  ##################### DataONE Download #####################
  source("server_files/d1_download_svr.R", local = TRUE)

  ######### FileInput ########################################
  source("server_files/local_upload_svr.R", local = TRUE)

  ######### Input Record #####################################
 # source('server_files/input_record_svr.R', local = TRUE)

  ##### Input Record only svr file
  source("server_files/create_input_record_svr.R", local = TRUE)
   
  #### formats record only server file
  source("server_files/formats_record_svr.R", local = TRUE)
  
  #### dbfiles record only server file
  source("server_files/dbfiles_record_svr.R", local = TRUE)

  
}

# Run the application
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87