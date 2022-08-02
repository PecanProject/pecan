##' Check whether a unit string is parseable
##' 
##' Function will replace the now-unmaintained `udunits2::ud.is.parseable`
##' @author Tanishq Jain
##' 
##' @param u1 A character string representing a type of units
##' 
##' @return TRUE if the units is parseable, FALSE otherwise.
##' @export
unit_is_parseable <- function(u1){
  tryCatch({
    if(units::as_units(u1))
      return(TRUE)
  },
  error = function(e) FALSE
  )
} # unit_is_parseable