##' Convert units
##'
##' Unit conversion to replace the now-unmaintained `udunits2::ud.convert`
##' @author Chris Black
##' 
##' @param x numeric vector
##' @param u1 string parseable as the units in which `x` is provided
##' @param u2 string parseable as the units to convert to
##' 
##' @return numeric vector with values converted to units in `u2`
##' @export
ud_convert <- function(x, u1, u2) {
  stopifnot(units::ud_are_convertible(u1, u2))
  x1 <- units::set_units(x, value = u1, mode = "standard")
  x2 <- units::set_units(x1, value = u2, mode = "standard")
  
  units::drop_units(x2)
  } # ud_convert
