
sample_met <- function(settings,nens=1){
  
  path <- settings$run$inputs$met[["path"]]

  ens_members <- list.files(path, recursive = TRUE)
  
  
  start_date <- as.POSIXlt(strptime(settings$run$site$met.start, "%Y/%m/%d"))
  end_date   <- as.POSIXlt(strptime(settings$run$site$met.end, "%Y/%m/%d"))
  start_date$zone <- end_date$zone <- NULL
  
  # only the original (not-splitted) file has start and end date only
  tmp_members <- gsub(paste0(".", start_date), "", ens_members) 
  tmp_members <- gsub(paste0(".", end_date),   "", tmp_members) 
  member_names <- unique(dirname(ens_members))
  
  #this will change from model to model, generalize later
  ens_ind <- unlist(sapply(paste0(member_names, ".clim"), grep, tmp_members))
  # ens_members[ens_ind]
  ens_input <- list()
  for(i in seq_len(nens)){
    ens_input[[i]] <- list(met=NULL)
    ens_input[[i]]$met$path <- file.path(path, ens_members[sample(ens_ind, 1)])
  }
  names(ens_input) <- rep("met",length=nens)
  return(ens_input)
}




