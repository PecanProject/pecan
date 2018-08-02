##' 
##' @param settings PEcAn settings list
##' @param nens number of ensemble members to be sampled
##' 
##' @export
sample_met <- function(settings, nens=1){

  # path where ensemble met folders are
  if(length(settings$run$inputs$met[["path"]]) == 1){
    path <- settings$run$inputs$met[["path"]]
  }else if(!is.null(settings$run$inputs$met[["path"]])){ # this function will be deprecated soon anyway
    path <- settings$run$inputs$met[["path"]][[1]]
  }else{
    PEcAn.logger::logger.error("Met path not found in settings.")
  }
  

  if(settings$host$name == "localhost"){
    ens_members <- list.files(path, recursive = TRUE)
  }else{
    # remote
    ens_members <- PEcAn.remote::remote.execute.cmd(host, paste0('ls -d -1 ', path, "/*.*"))
  }
  

  start_date <- as.POSIXlt(strptime(settings$run$site$met.start, "%Y/%m/%d"))
  end_date   <- as.POSIXlt(strptime(settings$run$site$met.end, "%Y/%m/%d"))
  start_date$zone <- end_date$zone <- NULL
  
  # only the original (not-splitted) file has start and end date only
  tmp_members <- gsub(paste0(".", start_date), "", ens_members) 
  tmp_members <- gsub(paste0(".", end_date),   "", tmp_members) 
  member_names <- unique(dirname(ens_members))
  
  # this will change from model to model, generalize later
  # This function is temporary but if we will continue to use this approach for met ensembles (instead of met process workflow)
  # it might not be a bad idea to have sample_met.model
  if(settings$model$type == "ED2"){
    # TODO : it doesn't have to be called ED_MET_DRIVER_HEADER
    ens_members <- file.path(basename(ens_members), "ED_MET_DRIVER_HEADER") 
    ens_ind     <- seq_along(ens_members)
  }else if(settings$model$type == "SIPNET"){
    ens_ind <- unlist(sapply(paste0(member_names, ".clim"), grep, tmp_members))
  }
  

  # ens_members[ens_ind]
  ens_input <- list()
  for(i in seq_len(nens)){
    ens_input[[i]] <- list(met=NULL)
    ens_input[[i]]$met$path <- file.path(path, ens_members[sample(ens_ind, 1)])
  }
  names(ens_input) <- rep("met",length=nens)
  return(ens_input)
}




