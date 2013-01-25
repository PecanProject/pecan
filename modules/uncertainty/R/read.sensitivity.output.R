
#--------------------------------------------------------------------------------------------------#
##' Reads output of sensitivity analysis runs
##'
##' 
##' @title Read Sensitivity Analysis output 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @param traits model parameters included in the sensitivity analysis
##' @param quantiles quantiles selected for sensitivity analysis
##' @param outdir directory with model output to use in sensitivity analysis
##' @param pft.name name of PFT used in sensitivity analysis (Optional)
##' @param start.year first year to include in sensitivity analysis 
##' @param end.year last year to include in sensitivity analysis
##' @param read.output model specific read.output function
##' @export
#--------------------------------------------------------------------------------------------------#
read.sa.output <- function(traits, quantiles, outdir, pft.name='', 
                           start.year, end.year, variables, model){
  
  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))
  for(trait in traits){
    for(quantile in quantiles){
      if(!quantile == "50"){
        run.id <- get.run.id('SA', round(as.numeric(quantile)/100, 3),
                             trait = trait, pft.name = pft.name)
        print(run.id)
        sa.output[quantile, trait] <-
          sapply(read.output(run.id, outdir,
                             start.year, end.year,
                             variables, model),
                 mean, na.rm=TRUE)
      } else if (quantile == "50") {
        sa.output[quantile, trait] <- sapply(read.output(get.run.id('SA', 'median'),
                                                         outdir,
                                                         start.year, end.year,
                                                         variables, model),
                                             mean,na.rm=TRUE)
      } ## end loop over quantiles
    }
  } ## end loop over traits
  sa.output <- as.data.frame(sa.output)
  return(sa.output)
}
#==================================================================================================#