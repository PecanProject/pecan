##' Convert units
##'
##' Unit conversion to replace the now-unmaintained `udunits2::ud.convert`
##' @author Chris Black
##' 
##' @param x numeric vector with units `u1` to be converted to units `u2`
##' @param u1 string which is parseable into a `units` compatible unit.
##' @param u2 string which is parseable into a `units` compatible unit,
##'   and for which there exists a defined transformation from the units represented by `u1`.
##' 
##' @return numeric vector stripped of units
##' @export
ud_convert <- function(x, u1, u2) {
  stopifnot(units::ud_are_convertible(u1, u2))
  a <- units::set_units(x, value = u1, mode = "standard")
  units::drop_units(units::set_units(a, value = u2, mode = "standard"))
} # ud_convert