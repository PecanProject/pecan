#' Defunct functions in PEcAn.utils
#'
#' The functions listed below are defunct and have been removed from the package.
#'  Calling them will produce a message indicating what function, if any, has replaced it.
#'
#' @name PEcAn.utils-defunct
#' @keywords internal
NULL


#' @rdname PEcAn.utils-defunct
#' @section `convert.input`: Use `PEcAn.DB::convert_input` instead.
#' @usage NULL
#' @aliases convert.input convert.input-defunct
#' @export
convert.input <- function(...){
  .Defunct("PEcAn.DB::convert_input", package = NULL)
} # convert.input()
