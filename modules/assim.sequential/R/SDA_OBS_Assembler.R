#' Assembler for preparing obs.mean and obs.cov for the SDA workflow
#'
#' @param settings_dir the path of settings.xml object.
#' @param Var Variable name, currently support: SMP, AGB, and LAI.
#' @param OutDir the path to store obs.mean and obs.cov
#' @param Obs_Prep if your settings object doesn't contain Obs_Prep, you can import it separately (details see L17-18).
#' @param skip_buffer flag to skip calculating min var based on buffer area for agb data.
#'
#' @return list of obs.mean and obs.cov
#' @export
#' @author Dongchen Zhang
#'
#' @examples

SDA_OBS_Assembler <- function(settings_dir, Var, OutDir, Obs_Prep = NULL, skip_buffer = TRUE){
  #export special operator
  `%>%` <- magrittr::`%>%` 
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
  
  Site_Info <- list(site_id = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('id') %>% unlist() %>% as.character(),
                    lat = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lat') %>% unlist() %>% as.numeric(),
                    lon = settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lon') %>% unlist() %>% as.numeric(),
                    site_name = rep("name", length(settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('lat') %>% unlist() %>% as.numeric())))
  
  #collect time points
  #we need to know which var we want to proceed 
  #cause for every variable we assigned the same time_step object, we only need to grab it from any of what we have here.
  #here we grab the first var to calculate the time step.
  var_first <- Var[1]
  for (i in 1:length(Obs_Prep)) {
    if(!is.character(try(Obs_Prep[[i]]$Var == var_first, silent = T)))Time_Step <- Obs_Prep[[i]]$Time_Step
  }
  #time operations
  if(Time_Step$unit == "year"){
    years <- seq(0, (lubridate::year(Obs_Prep$End_Date) - lubridate::year(Obs_Prep$Start_Date)), as.numeric(Time_Step$num))#how many years between start and end date
    time_points <- as.Date(Obs_Prep$Start_Date) %m+% lubridate::years(years)
  }else if(Time_Step$unit == "day"){
    days <- seq(0, (lubridate::yday(Obs_Prep$End_Date) - lubridate::yday(Obs_Prep$Start_Date)), as.numeric(Time_Step$num))#how many days between start and end date
    time_points <- as.Date(Obs_Prep$Start_Date) %m+% lubridate::days(days)
  }
  
  #We need to keep the order from Var to the actual obs.mean and obs.cov
  OBS <- list()
  new_var <- c()
  #test for loop
  for (i in 1:length(Var)) {
    #grab function based on the variable name
    var <- Var[i]
    obs_prep_fun <- getExportedValue("PEcAn.data.remote", paste0(var, "_prep"))
    
    #grab function argument names
    fun_args <- methods::formalArgs(obs_prep_fun)
    
    #create args list given function argument names
    args <- list()
    #fill in args with what we have so far.
    Ind_list_match <- c()
    for (j in 1:length(fun_args)) {
      if(!is.character(try(variable <- get(fun_args[j]), silent = T))){
        if(typeof(variable)!="closure"){
          args[[fun_args[j]]] <- variable
          Ind_list_match <- c(Ind_list_match, j)
        }
      }
    }
    
    #fill in more args in the Obs_Prep object.
    Temp_unlist <- unlist(Obs_Prep)
    for (j in 1:length(fun_args)) {
      if(j %in% Ind_list_match) next #if we already assigned then jump to the next.
      Ind_single_match <- grep(stringr::str_replace_all(fun_args[j], "[^[:alnum:]]", ""), 
                               stringr::str_replace_all(names(Temp_unlist), "[^[:alnum:]]", ""),
                                  ignore.case = T)
      if(length(Ind_single_match)>1){
        Ind_single_match <- grep(stringr::str_replace_all(paste0(var, fun_args[j]), "[^[:alnum:]]", ""), 
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
  for (i in 1:length(time_points)) {
    t <- time_points[i]
    dat_all_var <- sd_all_var <- matrix(NA, length(Site_Info$site_id), length(new_var)) %>% `colnames<-`(new_var)
    #over variable
    for (j in 1:length(OBS)) {
      if(paste0(t, "_", Var[j]) %in% colnames(OBS[[j]])){
        dat_all_var[,j] <- OBS[[j]][,paste0(t, "_", Var[j])]
        sd_all_var[,j] <- OBS[[j]][,paste0(t, "_SD")]^2 #convert from SD to Var
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
    obs.mean[[i]] <- site_dat_var %>% purrr::set_names(Site_Info$site_id)
    obs.cov[[i]] <- site_sd_var %>% purrr::set_names(Site_Info$site_id)
  }
  names(obs.mean) <- gsub("-", "/", time_points)#not sure if I have to do this.
  names(obs.cov) <- gsub("-", "/", time_points)
  
  #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  for (i in 1:length(obs.mean)) {
    for (j in 1:length(obs.mean[[i]])) {
      if(sum(is.na(obs.mean[[i]][[j]]))){
        obs.mean[[i]][[j]] <- obs.mean[[i]][[j]][-which(is.na(obs.mean[[i]][[j]]))]
        obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-which(is.na(rowSums(obs.cov[[i]][[j]]))), -which(is.na(colSums(obs.cov[[i]][[j]])))]
      }
    }
  }
  
  #rename variables to the var/names in settings.
  for (i in 1:length(time_points)) {
    for (j in 1:length(obs.mean[[i]])) {
      if("AGB" %in% names(obs.mean[[i]][[j]])){
        names(obs.mean[[i]][[j]])[which(names(obs.mean[[i]][[j]])=="AGB")] <- "AbvGrndWood"
      }
      if("SMP" %in% names(obs.mean[[i]][[j]])){
        names(obs.mean[[i]][[j]])[which(names(obs.mean[[i]][[j]])=="SMP")] <- "SoilMoistFrac"
      }
      if("SoilC" %in% names(obs.mean[[i]][[j]])){
        names(obs.mean[[i]][[j]])[which(names(obs.mean[[i]][[j]])=="SoilC")] <- "TotSoilCarb"
      }
    }
  }
  
  save(obs.mean, file = file.path(OutDir, "obs.mean.Rdata"))
  save(obs.cov, file = file.path(OutDir, "obs.cov.Rdata"))
  list(obs.mean = obs.mean, obs.cov = obs.cov)
}