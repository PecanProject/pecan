##----------------------------------------------------------------------------##
return_DF_from_run_ID <- function(diff_ids){
  # Called by function parse_ids_from_input_runID
  # which is a wrapper of this function
  # Returns a DF for a particular run_id
  split_string <- strsplit(diff_ids,',')[[1]]
  # Workflow id is the first element. Trim leading and ending white spaces. Split by space now
  wID <- as.numeric(strsplit(trimws(split_string[1],which = c("both")),' ')[[1]][2])
  # Run id is the second element
  runID <- as.numeric(strsplit(trimws(split_string[2],which = c("both")),' ')[[1]][2])
  return(data.frame(wID,runID))
}

##----------------------------------------------------------------------------##
# Wrapper over return_DF_from_run_ID
# @param list of multiple run ids
# run_id_string: ('workflow' workflow_ID, 'run' run_id)
# @return Data Frame of workflow and run ids
parse_ids_from_input_runID <- function(run_id_list){
  globalDF <- data.frame()
  for (w_run_id in run_id_list) {
    globalDF <- rbind(globalDF,return_DF_from_run_ID(w_run_id))
  }
  return(globalDF)
}

##----------------------------------------------------------------------------##
# Allows to load actual data (different from model output) following the tutorial
# https://github.com/PecanProject/pecan/blob/develop/documentation/tutorials/AnalyzeOutput/modelVSdata.Rmd
# @params: bety,settings,File_path,File_format
# loadObservationData <- function(bety,settings,File_path,File_format){
loadObservationData <- function(bety,inputs_df){
  input_id <- inputs_df$input_id
  # File_format <- getFileFormat(bety,input_id)
  File_format <- PEcAn.DB::query.format.vars(bety = bety, input.id = input_id)
  start.year <- as.numeric(lubridate::year(inputs_df$start_date))
  end.year <- as.numeric(lubridate::year(inputs_df$end_date))
  File_path <- inputs_df$filePath
  # TODO There is an issue with the db where file names are not saved properly. 
  # To make it work with the VM, uncomment the line below
  # File_path <- paste0(inputs_df$filePath,'.csv')
  site.id <- inputs_df$site_id
  site <- PEcAn.DB::query.site(site.id,bety$con)
  observations <- PEcAn.benchmark::load_data(
    data.path = File_path, format = File_format, time.row = File_format$time.row,  
    site = site, start_year = start.year, end_year = end.year) 
  return(observations)
}

getSettingsFromWorkflowId <- function(bety,workflowID){
  basePath <- dplyr::tbl(bety, 'workflows') %>% 
    dplyr::filter(id %in% workflowID) %>% dplyr::pull(folder)
  configPath <- file.path(basePath, 'pecan.CONFIGS.xml')
  settings <- PEcAn.settings::read.settings(configPath)
  return(settings)
}

##----------------------------------------------------------------------------##
# Get input id from selected site id. Returns inputs_df which is used to load observation data
getInputs <- function(bety,site_Id){
  # Subsetting the input id list based on the current (VM) machine
  my_hostname <- PEcAn.remote::fqdn()
  my_machine_id <- dplyr::tbl(bety, 'machines') %>% 
    dplyr::filter(hostname == my_hostname) %>% dplyr::pull(id)
  # Inner join 'inputs' table with 'dbfiles' table
  # inputs_df would contain all the information about the site and input id required for
  # the tutorial mentioned above to compare model run with actual observations
  inputs_df <- dplyr::tbl(bety, 'dbfiles') %>% 
    dplyr::filter(container_type == 'Input', machine_id == my_machine_id) %>%
    dplyr::inner_join(tbl(bety, 'inputs') %>% dplyr::filter(site_id %in% site_Id), 
                      by = c('container_id' = 'id')) %>% dplyr::collect()
  # Order by container id (==input id)
  inputs_df <- inputs_df[order(inputs_df$container_id),]
  # Mutate column as (input id, name) to be shown to the user
  inputs_df <- inputs_df %>% 
    dplyr::mutate(input_selection_list = paste(inputs_df$container_id, inputs_df$name),
                  filePath = paste0(inputs_df$file_path,'/', inputs_df$file_name)) %>%
    dplyr::select(input_id = container_id, filePath, input_selection_list, 
                  start_date, end_date, site_id,name, machine_id, file_name, file_path, format_id)
  return(inputs_df)
}

##----------------------------------------------------------------------------##
