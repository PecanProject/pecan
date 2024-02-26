#' @title get_ensemble_weights
#' @name  get_ensemble_weights
#' @author Ann Raiho \email{ann.raiho@gmail.com}
#' 
#' @param settings  PEcAn settings object
#' @param time_do   Give user specific time so you don't have to have it be annual
#'
#' @description Creates file of ensemble weights in format needed for SDA
#' 
#' @return NONE
#' 
#' @import lubridate
#' @export
#' 

get_ensemble_weights <- function(settings, time_do){

  
  nens <- as.numeric(settings$ensemble$size)
  
  if(!is.null(try(settings$run$inputs$ensembleweights$path))){
    
    ###-------------------------------------------------------------------###
    ### Loading Weights                                                   ###
    ###-------------------------------------------------------------------###
    weight_file <- utils::read.csv(settings$run$inputs$ensembleweights$path)
    start_date <- settings$run$inputs$ensembleweights$start.date
    end_date <- settings$run$inputs$ensembleweights$end.date
    years_get <- lubridate::year(start_date):lubridate::year(end_date) #assuming year time step... would need to change for other analyses possibly going down the load.data path?
    
    weight_file[weight_file==0] <- .00001 #hack not sure how to deal with zero weights
    
    weight_list <- list()
    
    ###-------------------------------------------------------------------###
    ### Assigning Weights                                                 ###
    ###-------------------------------------------------------------------###
    #TO DO: Right now just takes snapshot weights. Consider averaging over time period.
    for(tt in seq_along(time_do)){
      
      which_ens <- settings$run$inputs$met$path
      
      climate_names <- unlist(lapply(which_ens,FUN=function(x) strsplit(x,'/')[[1]][6]))
      
      #TO DO: make more general for subannual weights
      weight_list[[tt]] <-
        (weight_file[weight_file$year == time_do[tt] &
                       weight_file$climate_model %in% climate_names, 'weights'] / sum(weight_file[weight_file$year ==
                                                                                                    time_do[tt] &
                                                                                                    weight_file$climate_model %in% climate_names, 'weights'])) * nens
      
      if(sum(weight_list[[tt]]) != nens) PEcAn.logger::logger.warn(paste('Time',tt,'does not equal the number of ensemble members',nens))
      
      #TO DO: will need to have some way of dealing with sampling too if there are more ensemble members than weights or vice versa
      
      if(sum(weight_list[[tt]])==0){
        weight_list[[tt]] <- rep(1,nens) #no weights
      }
      
      names(weight_list[[tt]]) <- 1:nens #giving number names because the met names change between files with the '.' and the '-' seps
      
    }
    
  }else{
    weight_list <- list()
    for(tt in 1:length(years_get)){
      weight_list[[tt]] <- rep(1,nens) #no weights
      names(weight_list[[tt]]) <- 1:nens #giving number names because the met names change between files with the '.' and the '-' seps
    }
  }
  
  names(weight_list) <- time_do
  
  save(weight_list, file = file.path(settings$outdir, "ensemble_weights.Rdata"))
  
}


#Example script to rename if climate files get messed up seps
if(FALSE){
  
  files_get <- list.dirs('~/TENSION_MET/')
  
  for(ii in 2:length(files_get)){
    file.rename(from = files_get[ii],
                to = str_replace_all(
                  string = files_get[ii],
                  pattern = '-',
                  replacement = '.'
                ))
  }
  
  
  for(ii in 2:length(files_get)){
    load(paste0(files_get[ii],'/climate.Rdata'))
    rownames(temp.mat) <- rownames(precip.mat) <- 850:2010
    save(temp.mat, precip.mat,file=paste0(files_get[ii],'/climate.Rdata'))
  }
  
}