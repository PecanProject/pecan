#' Convert units
#'
#' Unit conversion to replace the now-unmaintained `udunits2::ud.convert`
#' @author Chris Black
#'
#' @param x vector of class "numeric" or "difftime"
#' @param u1 string parseable as the units in which `x` is provided.  If `x` is
#'   class "difftime", then `u1` is not actually used.  However, it still needs
#'   to be supplied and needs to be convertible to `u2` for consistency.
#' @param u2 string parseable as the units to convert to
#'
#' @return numeric vector with values converted to units in `u2`
#' @export
ud_convert <- function(x, u1, u2) {
  stopifnot(units::ud_are_convertible(u1, u2))
  if(inherits(x, "difftime")) {
    x1 <- units::as_units(x)
    if(units(x1) != units(units::as_units(u1))) {
      warning("Units of `x` don't match `u1`, using '", units::deparse_unit(x1), "' instead")
    }
  } else {
    x1 <- units::set_units(x, value = u1, mode = "standard")
  }
  x2 <- units::set_units(x1, value = u2, mode = "standard")
  
  units::drop_units(x2)
  } # ud_convert
