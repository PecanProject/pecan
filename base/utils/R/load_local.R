#' Load an `RData` file into a list
#'
#' Instead of polluting the current environment, this allows you to
#' read an `RData` file into a list object of whatever name you choose.
#'
#' @inheritParams base::load
#' @return List, with names corresponding to object names in `file`
#' @author Alexey Shiklomanov
#' @export
#' @examples
#' x <- 1:10
#' y <- 11:15
#' tmp <- tempfile()
#' save(x, y, file = tmp)
#' my_list <- load_local(tmp)
#' rm(tmp)
load_local <- function(file) {
  my_env <- new.env()
  load(file, envir = my_env)
  as.list(my_env)
}
