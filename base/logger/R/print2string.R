#' Convert the printed output of an object to a string. This is
#' particularly useful for passing complex objects (e.g. matrices,
#' data.frames) to simple printing functions like
#' [PEcAn.logger::logger.info()] or [base::message()].
#'
#' Note that for this to work properly in the `PEcAn.logger`
#' functions, you should always add the `wrap = FALSE` argument, and
#' probably add a newline (`"\n"`) before the output of this function.
#'
#' @param x Object to print
#' @param ... Additional arguments to [base::print()].
#' @return Output of `print(x, ...)`, captured as string
#' @author Alexey Shiklomanov
#' @examples
#' logger.info("First few rows of Iris:\n", print2string(iris[1:10, -5]), wrap = FALSE)
#' df <- data.frame(test = c("download", "process", "plot"), status = c(TRUE, TRUE, FALSE))
#' logger.debug("Current status:\n", print2string(df, row.names = FALSE), wrap = FALSE)
#' @export
print2string <- function(x, ...) {
  cout <- capture.output(print(x, ...))
  paste(cout, collapse = "\n")
}
