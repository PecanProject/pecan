#' Generalized plate model
#'
#' This is the fundamental physical model underlying the PROSPECT family of leaf RTMs.
#' @param k Specific absorption coefficient (400 - 2500nm)
#' @param refractive Refractive index (400 - 2500nm)
#' @param N Effective number of mesophyll layers (see [prospect()])
#' @export
gpm <- function(k, refractive, N) {
  stopifnot(
    length(k) == 2101,
    length(refractive) == 2101,
    length(N) == 1
  )
  RT <- matrix(0, 2101, 2)
  outlist <- .Fortran("gpm", k, refractive, N, RT)
  outlist[[4]]
}
