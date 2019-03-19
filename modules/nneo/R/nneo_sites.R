#' Sites
#'
#' @export
#' @param x (character) a site code
#' @template curl
#' @return a tibble (data.frame)
#' @examples \dontrun{
#' ## list products
#' nneo_sites()
#'
#' ## get a product
#' res <- nneo_site("JORN")
#' res$dataProducts
#' res$siteDescription
#' }
nneo_sites <- function(...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "sites"),
      ...
    )
  )
  tibble::as_data_frame(res$data)
}

#' @export
#' @rdname nneo_sites
nneo_site <- function(x, ...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "sites", x),
      ...
    )
  )
  res$data
}
