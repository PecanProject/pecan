#' Check whether a string can be interpreted as a unit
#'
#' Function will replace the now-unmaintained `udunits2::ud.is.parseable`
#' @author Tanishq Jain
#'
#' @param unit A character string representing a type of units
#'
#' @return TRUE if the units is parseable, FALSE otherwise.
#'
#' @examples
#'   unit_is_parseable("g/sec^2")
#'   unit_is_parseable("kiglometters")
#'
#' @export
unit_is_parseable <- function(unit){
  tryCatch({
    if(units::as_units(unit))
      return(TRUE)
  },
  error = function(e) FALSE
  )
} # unit_is_parseable
