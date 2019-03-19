#' Locations
#'
#' @export
#' @param x (character) a location code name
#' @template curl
#' @return a tibble (data.frame)
#' @examples \dontrun{
#' ## list products
#' nneo_locations()
#'
#' ## get a product
#' res <- nneo_location("HARV")
#' res
#' res$locationProperties
#' res$locationChildren
#' }
nneo_locations <- function(...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "locations", "sites"),
      ...
    )
  )
  tibble::as_data_frame(res$data)
}

#' @export
#' @rdname nneo_locations
nneo_location <- function(x, ...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "locations", x),
      ...
    )
  )
  res$data
}
