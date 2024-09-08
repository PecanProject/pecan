##' Equivalent to derive.trait(), but operates over a series of trait datasets,
##' as opposed to individual trait rows. See \code{\link{derive.trait}}; for more information.
##'
##' @name derive.traits
##' @title Performs an arithmetic function, FUN, over a series of traits and returns the result as a derived trait.
##' @export
##' @param FUN arithmetic function
##' @param ... trait datasets that will be supplied to FUN as input
##' @param input list of trait inputs. See examples in \code{\link{derive.trait}}
##' @param var.name name to use in output
##' @param sample.size where traits are normally distributed with a given
##' @param match.columns in the event more than one trait dataset is supplied,
##'        this specifies the columns that identify a unique data point
##' @return a copy of the first input trait with modified mean, stat, and n
derive.traits <- function(FUN, ..., input = list(...),
                          match.columns = c('citation_id', 'site_id', 'specie_id'),
                          var.name = NA, sample.size = 10^6){
  if(length(input) == 1){
    input <- input[[1]]
    #KLUDGE: modified to handle empty datasets
    for(i in (0:nrow(input))[-1]){
      input[i,] <- derive.trait(FUN, input[i,], sample.size=sample.size)
    }
    return(input)
  }
  else if(length(match.columns) > 0){
    #function works recursively to reduce the number of match columns
    match.column <- match.columns[[1]]
    #find unique values within the column that intersect among all input datasets
    columns <- lapply(input, function(data){data[[match.column]]})
    intersection <- Reduce(intersect, columns)
    
    #run derive.traits() on subsets of input that contain those unique values
    derived.traits<-lapply(intersection,
                           function(id){
                             filtered.input <- lapply(input,
                                                      function(data){data[data[[match.column]] == id,]})
                             derive.traits(FUN, input=filtered.input,
                                           match.columns=match.columns[-1],
                                           var.name=var.name,
                                           sample.size=sample.size)
                           })
    derived.traits <- derived.traits[!is.null(derived.traits)]
    derived.traits <- do.call(rbind, derived.traits)
    return(derived.traits)
  } else {
    return(derive.trait(FUN, input = input, var.name = var.name, sample.size = sample.size))
  }
}
