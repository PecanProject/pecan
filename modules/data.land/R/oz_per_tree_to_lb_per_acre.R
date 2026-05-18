#' Convert N application rate from oz N per tree to lb N per acre
#'
#' California extension guidelines for young orchards (CDFA FREP, UC ANR,
#' Almond Board) publish N rates per tree. This helper multiplies by
#' orchard density to give lb N per acre for pipelines that need per acre
#' inputs.
#'
#' @param oz_per_tree numeric vector. N rate in ounces N per tree.
#' @param tpa numeric scalar or vector. Orchard density in trees per acre.
#'   Recycled to length of oz_per_tree if scalar.
#'
#' @return numeric vector. N rate in lb N per acre. NA inputs propagate.
#'
#' @examples
#' oz_per_tree_to_lb_per_acre(c(1, 3), tpa = 145)
#'
#' @export
oz_per_tree_to_lb_per_acre <- function(oz_per_tree, tpa) {
  oz_per_tree * tpa / 16
}
