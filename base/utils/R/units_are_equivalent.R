#' Check if two unit strings are equivalent
#'
#' This is to allow multiple forms of the same unit to work, such as
#' `m/s` vs. `m s-1` or `K` and `Kelvin`.
#' @param x A unit string, as character
#' @param y Another unit string for comparison, as character
#' @return `TRUE` if equivalent, `FALSE` otherwise
#' @author Alexey Shiklomanov
#' @export
units_are_equivalent <- function(x, y) {
  x2y <- udunits2::ud.convert(1, x, y)
  1 == x2y
}
