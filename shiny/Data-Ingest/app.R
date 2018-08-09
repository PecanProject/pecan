 library(PEcAn.data.land)
 library(PEcAn.visualization)
 library(PEcAn.utils)
 library(PEcAn.DB)
 library(shinydashboard)
 library(dataone)
 library(stringr)
 library(DT)
 library(shiny)
 library(shinyjs)
 library(shinyWidgets)
 library(shinytoastr)
 
 source("ui_utils.R", local = TRUE)
 source("helper.R", local = TRUE)
 
 
 ##### Bety Calls ######
 bety <- betyConnect()
 
 sites <- dplyr::tbl(bety, "sites") %>% dplyr::select(sitename, id) %>% dplyr::arrange(sitename)
 sitenames <- sites %>% pull(sitename)
 
 inputs <- dplyr::tbl(bety, "inputs") %>% dplyr::select(name, id) %>% dplyr::arrange(name)
 input_names <- inputs %>% pull(name)
 
 formats <- dplyr::tbl(bety, "formats") %>% distinct(name) %>% dplyr::arrange(name) %>% pull(name)
 formats_sub <- dplyr::tbl(bety, "formats") %>% dplyr::select(name, id) %>% dplyr::arrange(name)
 
 variables_ids <- dplyr::tbl(bety, "variables") %>% dplyr::select(id, name) %>% dplyr::arrange(name)
 variables <- variables_ids %>% pull(name)
  
 # machines <- dplyr::tbl(bety, "machines") %>% distinct(hostname) %>% dplyr::arrange(hostname)%>% pull(hostname)
 # machines_sub <- dplyr::tbl(bety, "machines") %>% dplyr::select(hostname, id) %>% dplyr::arrange(hostname)
 
 mimetypes <- dplyr::tbl(bety, "mimetypes") %>% distinct(type_string) %>% dplyr::arrange(type_string) %>% pull(type_string)
 mimetype_sub <- dplyr::tbl(bety, "mimetypes") %>% dplyr::select(type_string, id) %>% dplyr::arrange(type_string)


#############################################################################
################################## UI #######################################
#############################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Data Ingest Workflow"), 
  dashboardSidebar(collapsed = TRUE,
    source_ui("sidebar_ui.R")
  ),
  dashboardBody(
    useToastr(), # Call error handling package
    useShinyjs(), #Include shinyjs
    tabItems(
    ## Tab 1 -- Ingest Workflow
    tabItem(tabName = "ingestWorkflow",
              source_ui("ingest_workflow_ui.R")
      ),
    ## Tab 2 -- About
    tabItem(tabName = "About",
            source_ui("homepage_ui.R")
            )
 
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
  Shared.data <- reactiveValues(downloaded = NULL, selected_row = NULL, 
                                local_files = NULL, selected_row_local = NULL, 
                                new_format = NULL, input_record_df = NULL, 
                                format_vars_df = NULL, input_method = NULL)
  
  Shared.data$variables_rd <- variables
  
  temp <- tempdir() 
  PEcAn_path <- PEcAn.utils::read_web_config("../../web/config.php")$dbfiles_folder
  
  ## Create two sub-directories in the tempfile ##
  d1_tempdir <<- file.path(temp, "d1_tempdir")
  dir.create(d1_tempdir, showWarnings = F)
  local_tempdir <<- file.path(temp, "local_tempdir")
  dir.create(local_tempdir, showWarnings = F)
  
  ##################### DataONE Download #####################
  source("server_files/d1_download_svr.R", local = TRUE)

  ######### FileInput ########################################
  source("server_files/local_upload_svr.R", local = TRUE)
  
  ######### Ingest Workflow ##############################
  source("server_files/ingest_workflow_svr.R", local = TRUE)
  
  
}

# Run the application... = 
shinyApp(ui = ui, server = server)

# example data: doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87, doi:10.6073-pasta-f31b28b912e6051bf1d383ff1ef18987
