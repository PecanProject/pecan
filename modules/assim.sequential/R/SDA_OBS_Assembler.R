#' Assembler for preparing obs.mean and obs.cov for the SDA workflow
#'
#' @param settings the settings object followed by PEcAn.settings format.
#'
#' @return list of obs.mean and obs.cov
#' @export
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
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
  
  #check if we want to proceed the free run without any observations.
  if (as.logical(settings$state.data.assimilation$free.run)) {
    PEcAn.logger::logger.info("Create obs for free run!")
    #calculate time points.
    time_points <- obs_timestep2timepoint(Obs_Prep$start.date, Obs_Prep$end.date, Obs_Prep$timestep)
    
    #generate obs.mean and obs.cov with NULL filled.
    obs.mean = vector("list", length(time_points)) %>% `names<-`(time_points)
    obs.cov = vector("list", length(time_points)) %>% `names<-`(time_points)
    
    #save files.
    save(obs.mean, file = file.path(Obs_Prep$outdir, "Rdata", "obs.mean.Rdata"))
    save(obs.cov, file = file.path(Obs_Prep$outdir, "Rdata", "obs.cov.Rdata"))
    return(list(obs.mean = obs.mean, obs.cov = obs.cov))
  }
  
  #prepare site_info offline, because we need to submit this to server remotely, which might not support the Bety connection.
  site_info <- settings$run %>% 
    purrr::map('site')%>% 
    purrr::map(function(site.list){
      #conversion from string to number
      site.list$lat <- as.numeric(site.list$lat)
      site.list$lon <- as.numeric(site.list$lon)
      list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
    })%>% 
    dplyr::bind_rows() %>% 
    as.list()
  
  #convert from timestep to time points
  if (length(Obs_Prep$timestep)>0){
    diff_dates <- FALSE
  }else{
    diff_dates <- TRUE
  }
  
  #The order of obs.mean and obs.cov objects are relying on the order of how you organize the Obs_Prep section.
  OBS <- timestep <- time_points_all <- list()
  var_ind <- var <- c()
  #test for loop
  for (i in seq_along(Obs_Prep)) {
    #detect if current section is for different variable preparation function or not.
    if (names(Obs_Prep)[i] %in% c("timestep", "start.date", "end.date", "outdir")){
      next
    }else{
      PEcAn.logger::logger.info(paste("Entering", names(Obs_Prep)[i]))
      fun_name <- names(Obs_Prep)[i]
      var_ind <- c(var_ind, i)
    }
    
    #if we are dealing with different timestep for different variables.
    if (diff_dates){
      timestep[[i]] <- Obs_Prep[[i]]$timestep
      if (!exists("timestep")){
        PEcAn.logger::logger.error(paste0("Please provide timestep under each variable if you didn't provide timestep under Obs_Prep section!"))
        return(0)
      }
      time_points <- obs_timestep2timepoint(Obs_Prep[[i]]$start.date, Obs_Prep[[i]]$end.date, timestep[[i]])
    }else{
      timestep[[i]] <- Obs_Prep$timestep
      time_points <- obs_timestep2timepoint(Obs_Prep$start.date, Obs_Prep$end.date, timestep[[i]])
    }
    #Search function inside the data.remote package
    if(is.character(try(obs_prep_fun <- getExportedValue("PEcAn.data.remote", paste0(fun_name, "_prep")), silent = T))){
      #Search function inside the data.land package, this is explicit for Soilgrids prep function.
      if(is.character(try(obs_prep_fun <- getExportedValue("PEcAn.data.land", paste0(fun_name, "_prep")), silent = T))){
        PEcAn.logger::logger.info("Couldn't find the function: ", paste0(fun_name, "_prep"), ". Please Check it!")
        return(0)
      }
    }
    
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
  time_points <- sort(unique(do.call("c", time_points_all)))
  
  #Create obs.mean and obs.cov
  obs.mean <- obs.cov <- list()
  new_diag <- function(vec){
    if (length(vec) == 1){
      return(vec)
    }else{
      return(diag(vec))
    }
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
  names(obs.mean) <- names(obs.cov) <- time_points
  #remove NA data as this will crash the SDA.
  #for soilgrids specifically, calculate the cov multiplier by the sqrt of length of total time steps.
  if("TotSoilCarb" %in% var){
    Soilgrids_multiplier <- length(time_points_all[[which(var == "TotSoilCarb")]])
  }
  for (i in seq_along(obs.mean)) {
    for (j in seq_along(obs.mean[[i]])) {
      if (sum(is.na(obs.mean[[i]][[j]]))){
        na_ind <- which(is.na(obs.mean[[i]][[j]]))
        obs.mean[[i]][[j]] <- obs.mean[[i]][[j]][-na_ind]
        if(length(new_diag(obs.cov[[i]][[j]])) == 1){
          obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-na_ind]
        }else{
          obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-na_ind, -na_ind]
        }
      }
      SoilC_ind <- which(names(obs.mean[[i]][[j]]) == "TotSoilCarb")
      if (length(SoilC_ind) > 0){
        if(length(obs.mean[[i]][[j]]) > 1){
          diag(obs.cov[[i]][[j]])[SoilC_ind] <- diag(obs.cov[[i]][[j]])[SoilC_ind] * Soilgrids_multiplier
        }else{
          obs.cov[[i]][[j]][SoilC_ind] <- obs.cov[[i]][[j]][SoilC_ind] * Soilgrids_multiplier
        }
      }
    }
  }
  
  #fill in empty element within obs.mean and obs.cov lists.
  #if time steps for all obs are the same
  if(length(unique(unlist(timestep))) == 2){
    if(diff_dates){
      timepoints_fill <-
        purrr::pmap(list(timestep, 
                         Obs_Prep[var_ind] %>% purrr::map(~.x$start.date), 
                         Obs_Prep[var_ind] %>% purrr::map(~.x$end.date)),
                    function(var_timestep, var_start_date, var_end_date){
                      obs_timestep2timepoint(var_start_date, var_end_date, var_timestep)
                    }) %>% 
        purrr::map(function(all_timepoints){
          all_timepoints[which(!all_timepoints %in% time_points)]
        }) %>% 
        do.call(what = "c") %>% 
        unique()
    }else{
      timepoints_fill <- timestep %>% 
        purrr::map(function(var_timestep){
          obs_timestep2timepoint(Obs_Prep$start.date, Obs_Prep$end.date, var_timestep)
        }) %>% 
        purrr::map(function(all_timepoints){
          all_timepoints[which(!all_timepoints %in% time_points)]
        }) %>% 
        do.call(what = "c") %>% 
        unique()
    }
    
    if(length(timepoints_fill)>0){
      obs_mean_fill <- obs_cov_fill <- list()
      time_points_start_end <- sort(c(timepoints_fill, time_points))
      for (i in seq_along(time_points_start_end)) {
        if(time_points_start_end[i] %in% timepoints_fill){
          obs_mean_fill[[as.character(time_points_start_end[i])]] <- list(NULL)
          obs_cov_fill[[as.character(time_points_start_end[i])]] <- list(NULL)
        }else{
          obs_mean_fill[[as.character(time_points_start_end[i])]] <- obs.mean[[as.character(time_points_start_end[i])]]
          obs_cov_fill[[as.character(time_points_start_end[i])]] <- obs.cov[[as.character(time_points_start_end[i])]]
        }
      }
      obs.mean <- obs_mean_fill
      obs.cov <- obs_cov_fill
    }
  }
  
  #create folder in case it doesn't exist.
  if(!file.exists(file.path(Obs_Prep$outdir, "Rdata"))){
    dir.create(file.path(Obs_Prep$outdir, "Rdata"))
  }
  save(obs.mean, file = file.path(Obs_Prep$outdir, "Rdata", "obs.mean.Rdata"))
  save(obs.cov, file = file.path(Obs_Prep$outdir, "Rdata", "obs.cov.Rdata"))
  list(obs.mean = obs.mean, obs.cov = obs.cov)
}