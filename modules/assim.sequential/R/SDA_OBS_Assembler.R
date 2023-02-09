#' Assembler for preparing obs.mean and obs.cov for the SDA workflow
#'
#' @param settings the settings object created by Create_Multi_settings.R script.
#'
#' @return list of obs.mean and obs.cov
#' @export
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#'
#' @examples
#' \dontrun{
#' settings_dir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/IC/pecan.xml"
#' settings <- PEcAn.settings::read.settings(settings_dir)
#' OBS <- SDA_OBS_Assembler(settings)
#' }
#' 
SDA_OBS_Assembler <- function(settings){
  #extract Obs_Prep object from settings.
  Obs_Prep <- settings$state.data.assimilation$Obs_Prep
  
  #prepare site_info offline, because we need to submit this to server remotely, which might not support the Bety connection.
  site_info <- list(site_id = settings %>% 
                      purrr::map(~.x[['run']] ) %>% 
                      purrr::map('site') %>% 
                      purrr::map('id') %>% 
                      unlist() %>% 
                      as.character(),
                    lat = settings %>% 
                      purrr::map(~.x[['run']] ) %>% 
                      purrr::map('site') %>% 
                      purrr::map('lat') %>% 
                      unlist() %>% 
                      as.numeric(),
                    lon = settings %>% 
                      purrr::map(~.x[['run']] ) %>% 
                      purrr::map('site') %>% 
                      purrr::map('lon') %>% 
                      unlist() %>% 
                      as.numeric(),
                    site_name = rep("name", length(settings %>% 
                                                     purrr::map(~.x[['run']] ) %>% 
                                                     purrr::map('site') %>% 
                                                     purrr::map('lat') %>% 
                                                     unlist() %>% 
                                                     as.numeric())))
  
  #convert from timestep to time points
  if (length(Obs_Prep$timestep)>0){
    time_points <- PEcAnAssimSequential::obs_timestep2timepoint(Obs_Prep$start.date, Obs_Prep$end.date, Obs_Prep$timestep)
    if (time_points) return(0)
    diff_dates <- FALSE
  }else{
    diff_dates <- TRUE
  }
  
  #We need to keep the order from var_name to the actual obs.mean and obs.cov
  OBS <- list()
  var <- c()
  if (diff_dates) time_points_all <- list()
  #test for loop
  for (i in seq_along(Obs_Prep)) {
    #detect if current section is for different variable preparation function or not.
    if (names(Obs_Prep)[i] %in% c("timestep", "start.date", "end.date", "obs_outdir")){
      next
    }else{
      fun_name <- names(Obs_Prep)[i]
    }
    
    #if we are dealing with different timestep for different variables.
    if (diff_dates){
      timestep <- Obs_Prep[[i]]$timestep
      if (!exists("timestep")){
        PEcAn.logger::logger.error(paste0("Please provide timestep under each variable if you didn't provide timestep under Obs_Prep section!"))
        return(0)
      }
      time_points <- PEcAnAssimSequential::obs_timestep2timepoint(Obs_Prep$start.date, Obs_Prep$end.date, timestep)
    } 
    obs_prep_fun <- getExportedValue("PEcAn.data.remote", paste0(fun_name, "_prep"))
    
    #grab function argument names
    fun_args <- methods::formalArgs(obs_prep_fun)
    
    #create args list given function argument names
    args <- list()
    #fill in args with what we have so far.
    Ind_list_match <- c()
    for (j in seq_along(fun_args)) {
      if (!is.character(try(variable <- get(fun_args[j]), silent = T))){
        if (typeof(variable)!="closure"){
          args[[fun_args[j]]] <- variable
          Ind_list_match <- c(Ind_list_match, j)
        }
      }
    }
    
    #fill in more args in the Obs_Prep object.
    Temp_unlist <- unlist(Obs_Prep)
    for (j in seq_along(fun_args)) {
      #if we already assigned then jump to the next.
      if (j %in% Ind_list_match){
        next
      }
      
      #store complex string operation into temp variables.
      fun_args_temp <- stringr::str_replace_all(fun_args[j], "[^[:alnum:]]", "")
      obj_exist_temp <- stringr::str_replace_all(names(Temp_unlist), "[^[:alnum:]]", "")
      
      #match current names of the unlisted array from Obs_Prep list 
      #with the string pattern of the names of the function argument
      Ind_single_match <- grep(fun_args_temp, obj_exist_temp, ignore.case = T)
      
      #if we have multiple matches, then search the string pattern by the fun_name.
      funName_args_temp <- stringr::str_replace_all(paste0(fun_name, fun_args[j]), "[^[:alnum:]]", "")
      if (length(Ind_single_match)>1){
        Ind_single_match <- grep(funName_args_temp, obj_exist_temp, ignore.case = T)
      }
      args[[fun_args[j]]] <- as.character(Temp_unlist[Ind_single_match])
    }
    #clean list (remove any item with length of zero)
    cleaned_args <- list()
    for (j in seq_along(args)) {
      if (length(args[[j]])!=0){
        cleaned_args[[j]] <- args[[j]]
      }
    }
    names(cleaned_args) <- names(args)[seq_along(cleaned_args)]
    #function calls
    Temp <- do.call(obs_prep_fun, cleaned_args)
    OBS[[i]] <- Temp[[1]]
    time_points_all[[i]] <- Temp[[2]]
    var <- c(var, Temp[[3]])
  }
  
  #combine different time points from different variables together
  if (diff_dates){
    time_points <- sort(unique(do.call("c", time_points_all)))
  }
  
  #Create obs.mean and obs.cov
  obs.mean <- obs.cov <- list()
  new_diag <- function(vec){
    if (length(vec) == 1){
      return(vec)
    }else{
      return(diag(vec))
    }
  }
  rm_na_diag <- function(mat){
    mat[-which(is.na(rowSums(mat))), -which(is.na(colSums(mat)))]
  }
  
  #over time
  for (i in seq_along(time_points)) {
    t <- time_points[i]
    dat_all_var <- sd_all_var <- matrix(NA, length(site_info$site_id), length(var)) %>% `colnames<-`(var)
    #over variable
    for (j in seq_along(OBS)) {
      if (paste0(t, "_", var[j]) %in% colnames(OBS[[j]])){
        dat_all_var[,j] <- OBS[[j]][,paste0(t, "_", var[j])]
        sd_all_var[,j] <- OBS[[j]][,paste0(t, "_SD")]^2 #convert from SD to var
      }else{
        dat_all_var[,j] <- NA
        sd_all_var[,j] <- NA
      }
    }
    #over site
    site_dat_var <- site_sd_var <- list()
    for (j in 1:dim(dat_all_var)[1]) {
      site_dat_var[[j]] <- dat_all_var[j,] %>% matrix(1,length(var)) %>% data.frame %>% `colnames<-`(var)
      site_sd_var[[j]] <- new_diag(sd_all_var[j,])
    }
    obs.mean[[i]] <- site_dat_var %>% purrr::set_names(site_info$site_id)
    obs.cov[[i]] <- site_sd_var %>% purrr::set_names(site_info$site_id)
  }
  names(obs.mean) <- gsub("-", "/", time_points)#not sure if I have to do this.
  names(obs.cov) <- gsub("-", "/", time_points)
  
  #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  for (i in seq_along(obs.mean)) {
    for (j in seq_along(obs.mean[[i]])) {
      if (sum(is.na(obs.mean[[i]][[j]]))){
        obs.mean[[i]][[j]] <- obs.mean[[i]][[j]][-which(is.na(obs.mean[[i]][[j]]))]
        obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-which(is.na(rowSums(obs.cov[[i]][[j]]))), -which(is.na(colSums(obs.cov[[i]][[j]])))]
      }
    }
  }
  
  save(obs.mean, file = file.path(Obs_Prep$obs_outdir, "obs.mean.Rdata"))
  save(obs.cov, file = file.path(Obs_Prep$obs_outdir, "obs.cov.Rdata"))
  list(obs.mean = obs.mean, obs.cov = obs.cov)
}