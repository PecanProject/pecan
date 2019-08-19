#' @title get_ensemble_weights
#' @name  get_ensemble_weights
#' @author Ann Raiho \email{ann.raiho@gmail.com}
#' 
#' @param settings  PEcAn settings object
#'
#' @description Creates file of ensemble weights in format needed for SDA
#' 
#' @return NONE
#' 
#' @import lubridate
#' @export
#' 

get_ensemble_weights <- function(settings){

  
  nens <- as.numeric(settings$ensemble$size)
  
  if(!is.null(try(settings$run$inputs$ensembleweights$path))){
    
    ###-------------------------------------------------------------------###
    ### Loading Weights                                                   ###
    ###-------------------------------------------------------------------###
    weight_file <- read.csv(settings$run$inputs$ensembleweights$path)
    start_date <- settings$run$inputs$ensembleweights$start.date
    end_date <- settings$run$inputs$ensembleweights$end.date
    years_get <- lubridate::year(start_date):lubridate::year(end_date) #assuming year time step... would need to change for other analyses possibly going down the load.data path?
    
    weight_file[weight_file==0] <- .00001 #hack not sure how to deal with zero weights
    
    weight_list <- list()
    
    ###-------------------------------------------------------------------###
    ### Assigning Weights                                                 ###
    ###-------------------------------------------------------------------###
    for(tt in 1:length(years_get)){
      weight_list[[tt]] <- weight_file[weight_file$year==years_get[tt],'weights'] * nens
      
      #assuming weights are in the same order at the met in the settings file
      #will need to have some way of dealing with sampling too if there are more ensemble members than weights or vice versa
      
      
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
  
  names(weight_list) <- years_get
  
  save(weight_list, file = file.path(settings$outdir, "ensemble_weights.Rdata"))
  
}