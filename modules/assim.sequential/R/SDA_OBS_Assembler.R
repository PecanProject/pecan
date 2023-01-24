#' Assembler for preparing obs.mean and obs.cov for the SDA workflow
#'
#' @param settings_dir the path of settings.xml object.
#' @param var_name Variable name, currently support: SMP, AGB, and LAI.
#' @param outdir the path to store obs.mean and obs.cov
#' @param Obs_Prep if your settings object doesn't contain Obs_Prep, you can import it separately (details see L17-18).
#' @param skip_buffer flag to skip calculating min var based on buffer area for agb data.
#'
#' @return list of obs.mean and obs.cov
#' @export
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' settings_dir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/IC/pecan.xml"
#' outdir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/test_OBS"
#' var_name <- c("SMP", "LAI", "AGB")
#' OBS <- SDA_OBS_Assembler(settings_dir, var_name, outdir)
#' }


SDA_OBS_Assembler <- function(settings_dir, var_name, outdir, Obs_Prep = NULL, skip_buffer = TRUE){
  #export special operator
  `%m+%` <- as.function(lubridate::`%m+%`)
  
  #read settings
  settings <- PEcAn.settings::read.settings(settings_dir)
  
  # I prefer to include Obs_Prep info in the settings object, that will make life easier!
  # if you don't like it, you can provide the Obs_Prep object 
  # followed strictly by L76 - L82 in "/pecan/modules/assim.sequential/inst/MultiSite-Exs/SDA/Create_Multi_settings.R"
  if(!is.null(settings$state.data.assimilation$Obs_Prep)){
    Obs_Prep <- settings$state.data.assimilation$Obs_Prep
  }else if(is.null(Obs_Prep)){
    PEcAn.logger::logger.info("Please provide info of Obs_Prep object!")
    return(0)
  }
  
  #prepare site_info offline, because we need to submit this to GEO, which doesn't support Bety connection.
  
  site_info <- list(site_id = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('id') %>% unlist() %>% as.character(),
                    lat = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lat') %>% unlist() %>% as.numeric(),
                    lon = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lon') %>% unlist() %>% as.numeric(),
                    site_name = rep("name", length(settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lat') %>% unlist() %>% as.numeric())))
  
  #collect time points
  #we need to know which var we want to proceed 
  #cause for every variable we assigned the same timestep object, we only need to grab it from any of what we have here.
  #here we grab the first var to calculate the time step.
  #time operations
  if(Obs_Prep$timestep$unit == "year"){
    years <- seq(0, (lubridate::year(Obs_Prep$end.date) - lubridate::year(Obs_Prep$start.date)), as.numeric(Obs_Prep$timestep$num))#how many years between start and end date
    time_points <- as.Date(Obs_Prep$start.date) %m+% lubridate::years(years)
  }else if(Obs_Prep$timestep$unit == "day"){
    days <- seq(0, (lubridate::yday(Obs_Prep$end.date) - lubridate::yday(Obs_Prep$start.date)), as.numeric(Obs_Prep$timestep$num))#how many days between start and end date
    time_points <- as.Date(Obs_Prep$start.date) %m+% lubridate::days(days)
  }else{
    PEcAn.logger::logger.error("The Obs_prep functions only support year or day as timestep units!")
    return(0)
  }
  
  #We need to keep the order from var_name to the actual obs.mean and obs.cov
  OBS <- list()
  new_var <- c()
  #test for loop
  for (i in seq_along(var_name)) {
    #grab function based on the variable name
    var <- var_name[i]
    
    #grab function name by searching var_name inside each variable section.
    for (j in seq_along(Obs_Prep)) {
      Error <- try(if(Obs_Prep[[j]]$var_name==var){
        fun_name <- names(Obs_Prep)[j]
        break
      }, silent = T)
      if(is.character(Error)){
        PEcAn.logger::logger.error("Please provide consistent function name in the settings!")
        return(0)
      }
    }
    obs_prep_fun <- getExportedValue("PEcAn.data.remote", paste0(fun_name, "_prep"))
    
    #grab function argument names
    fun_args <- methods::formalArgs(obs_prep_fun)
    
    #create args list given function argument names
    args <- list()
    #fill in args with what we have so far.
    Ind_list_match <- c()
    for (j in seq_along(fun_args)) {
      if(!is.character(try(variable <- get(fun_args[j]), silent = T))){
        if(typeof(variable)!="closure"){
          args[[fun_args[j]]] <- variable
          Ind_list_match <- c(Ind_list_match, j)
        }
      }
    }
    
    #fill in more args in the Obs_Prep object.
    Temp_unlist <- unlist(Obs_Prep)
    for (j in seq_along(fun_args)) {
      if(j %in% Ind_list_match) next #if we already assigned then jump to the next.
      Ind_single_match <- grep(stringr::str_replace_all(fun_args[j], "[^[:alnum:]]", ""), 
                               stringr::str_replace_all(names(Temp_unlist), "[^[:alnum:]]", ""),
                                  ignore.case = T)
      if(length(Ind_single_match)>1){
        Ind_single_match <- grep(stringr::str_replace_all(paste0(fun_name, fun_args[j]), "[^[:alnum:]]", ""), 
                                 stringr::str_replace_all(names(Temp_unlist), "[^[:alnum:]]", ""),
                                 ignore.case = T)
      }
      args[[fun_args[j]]] <- as.character(Temp_unlist[Ind_single_match])
    }
    #function calls
    OBS[[i]] <- do.call(obs_prep_fun, args)[[1]]
    new_var <- c(new_var, var)
  }
  
  #Create obs.mean and obs.cov
  obs.mean <- obs.cov <- list()
  new_diag <- function(vec){
    if(length(vec)==1){
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
    dat_all_var <- sd_all_var <- matrix(NA, length(site_info$site_id), length(new_var)) %>% `colnames<-`(new_var)
    #over variable
    for (j in seq_along(OBS)) {
      if(paste0(t, "_", var_name[j]) %in% colnames(OBS[[j]])){
        dat_all_var[,j] <- OBS[[j]][,paste0(t, "_", var_name[j])]
        sd_all_var[,j] <- OBS[[j]][,paste0(t, "_SD")]^2 #convert from SD to var_name
      }else{
        dat_all_var[,j] <- NA
        sd_all_var[,j] <- NA
      }
    }
    #over site
    site_dat_var <- site_sd_var <- list()
    for (j in 1:dim(dat_all_var)[1]) {
      site_dat_var[[j]] <- dat_all_var[j,] %>% matrix(1,length(new_var)) %>% data.frame %>% `colnames<-`(new_var)
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
      if(sum(is.na(obs.mean[[i]][[j]]))){
        obs.mean[[i]][[j]] <- obs.mean[[i]][[j]][-which(is.na(obs.mean[[i]][[j]]))]
        obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-which(is.na(rowSums(obs.cov[[i]][[j]]))), -which(is.na(colSums(obs.cov[[i]][[j]])))]
      }
    }
  }
  
  save(obs.mean, file = file.path(outdir, "obs.mean.Rdata"))
  save(obs.cov, file = file.path(outdir, "obs.cov.Rdata"))
  list(obs.mean = obs.mean, obs.cov = obs.cov)
}