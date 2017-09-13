#' Remotely execute R code
#'
#' Runs an unevaluated R expression remotely. Wrap R code in `quote({...})` to generate an unevaluated expression.
#' Wrapped code should contain a `dput()` statement, or a warning will be thrown.
#' `dput()` is used to return outputs, so if it is absent, the output will be `
#'
#' @author Alexey Shiklomanov
#' @param code Unevaluated R expression containing code to be run on remote. To generate, use the `quote()` function.
#' @inheritParams remote.execute.cmd
#' @param ... Additional arguments passed to [remote.execute.cmd()].
#'
#' @return Exactly the output of `code`, or `NULL` if no `dput()` statement can be found in the wrapped code.
#' @export
#'
#' @examples
#' host <- list(name = "localhost")
#' code <- quote({
#'   x <- 5
#'   y <- 10
#'   out <- list(xx = seq_len(x), yy = seq_len(y) * 2)
#'   dput(out)
#' })
#' result <- remote.execute.R2(code = code, host = host)
remote.execute.R <- function(code, host, stderr = TRUE, ...) {
  if (!typeof(code) == "language") {
    stop("Code must be an R expression, for instance a block of code wrappen in the `quote()` function.")
  }
  code_string <- deparse(code)
  has_dput <- any(grepl("dput", code_string))
  if (!has_dput) {
    PEcAn.logger::logger.error("No dput statement found in code string.",
                               "This means no values will be returned.")
  }
  code_string_c <- paste(code_string, collapse = ';')
  cmd <- "Rscript"
  args <- c("-e", shQuote(code_string_c))
  result <- remote.execute.cmd(host = host, cmd = cmd, args = args, stderr = stderr, ...)
  if (!has_dput) {
    PEcAn.logger::logger.debug("Command ran successfuly, but no values returned because `dput` was not found.",
                               "Returning NULL.")
    return(NULL)
  } else {
    parsed <- parse(text = result)
    evalled <- eval(parsed)
    return(evalled)
  }
} # remote.execute.R