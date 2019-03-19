#' Products
#'
#' @export
#' @param x (character) a product code
#' @template curl
#' @return `nneo_products` returns a tibble (data.frame), and
#' `nneo_product` returns a list
#' @examples \dontrun{
#' ## list products
#' nneo_products()
#'
#' ## get a product
#' res <- nneo_product("DP3.30018.001")
#' res$productDescription
#'
#' ### get many products
#' ids <- c("DP3.30018.001", "DP4.00001.001", "DP3.30025.001")
#' lapply(ids, nneo_product)
#'
#' ## curl options
#' nneo_product("DP3.30018.001", verbose = TRUE)
#' nneo_product("DP3.30018.001", verbose = TRUE, timeout_ms = 1000)
#' nneo_product("DP3.30018.001", verbose = TRUE, useragent = "hello world")
#' }
nneo_products <- function(...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "products"),
      ...
    )
  )
  tibble::as_data_frame(res$data)
}

#' @export
#' @rdname nneo_products
nneo_product <- function(x, ...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "products", x),
      ...
    )
  )
  res$data
}
