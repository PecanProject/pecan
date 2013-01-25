#--------------------------------------------------------------------------------------------------#
##' Reads output from model ensemble
##'
##' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year} and \code{end.year}
##' @title Read ensemble output
##' @return a list of ensemble model output 
##' @param ensemble.size the number of ensemble members run
##' @param outdir directory with model output to use in ensemble analysis
##' @param start.year first year to include in ensemble analysis
##' @param end.year last year to include in ensemble analysis
##' @param variables targe variables for ensemble analysis
##' @param model ecosystem model run
##' @export
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, outdir, 
                                 start.year, end.year,variables, model){
  
  ensemble.output <- list()
  for(ensemble.id in 1:ensemble.size) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5))#log10(ensemble.size)+1))
    print(run.id)
    ensemble.output[[ensemble.id]] <- sapply(read.output(run.id, outdir, start.year, end.year,variables,model),mean,na.rm=TRUE)
  }
  return(ensemble.output)
}
#==================================================================================================#