#' build_X
#' 
#' @name build_X
#' @author Alexis Helgeson
#' 
#' @description builds X matrix for SDA
#'
#' @param new.params object created from sda_matchparam, passed from sda.enkf_MultiSite
#' @param nens number of ensemble members i.e. runs
#' @param read_restart_times passed from sda.enkf_MultiSite
#' @param settings settings object, passed from sda.enkf_MultiSite
#' @param outdir location of previous run output folder containing .nc files
#' @param out.configs object created for build_X passed from sda.enkf_MultiSite
#' @param t Default t=1, for function to work within time loop
#' @param var.names list of state variables taken from settings object
#' @param my.read_restart object that points to the model restart function i.e. read_restart.SIPNET
#' @param restart_flag flag if it's a restart stage. Default is FALSE.
#'
#' @return X ready to be passed to SDA Analysis code
build_X <- function(out.configs, settings, new.params, nens, read_restart_times, outdir, t = 1, var.names, my.read_restart, restart_flag = FALSE){
  if(t == 1 & restart_flag){
    reads <-
      furrr::future_pmap(list(out.configs %>% `class<-`(c("list")), settings, new.params),function(configs,my_settings,siteparams) {
        # Loading the model package - this is required bc of the furrr
        #library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
        #source("~/pecan/models/sipnet/R/read_restart.SIPNET.R")
        
        X_tmp <- vector("list", 2)
        
        for (i in seq_len(nens)) {
          X_tmp[[i]] <- do.call( my.read_restart,
                                 args = list(
                                   outdir = outdir,
                                   runid = my_settings$run$id[i] %>% as.character(),
                                   stop.time = read_restart_times[t+1],
                                   settings = my_settings,
                                   var.names = var.names,
                                   params = siteparams[[i]]
                                 )
          )
          
        }
        return(X_tmp)
      })
    
  }else{
    reads <-
      furrr::future_pmap(list(out.configs %>% `class<-`(c("list")), settings, new.params),function(configs,my_settings,siteparams) {
        
        X_tmp <- vector("list", 2)
        
        for (i in seq_len(nens)) {
          X_tmp[[i]] <- do.call( my.read_restart,
                                 args = list(
                                   outdir = outdir,
                                   runid = configs$runs$id[i] %>% as.character(),
                                   stop.time = read_restart_times[t+1],
                                   var.names = var.names,
                                   params = siteparams[[i]]
                                 )
          )
          
        }
        return(X_tmp)
      })
  }
  return(reads)
}