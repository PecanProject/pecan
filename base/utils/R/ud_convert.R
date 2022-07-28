##' Convert units
##'
##' Unit conversion to replace the now-unmaintained `udunits2::ud.convert`
##' @author Chris Black
##' 
##' @param x numeric vector
##' @param u1 string parseable as the units in which `x` is provided
##' @param u2 string parseable as the units to convert to
##' 
##' @return numeric vector stripped of units
##' @export
ud_convert <- function(x, u1, u2) {
  stopifnot(units::ud_are_convertible(u1, u2))
  a <- units::set_units(x, value = u1, mode = "standard")
  units::drop_units(units::set_units(a, value = u2, mode = "standard"))
} # ud_convert