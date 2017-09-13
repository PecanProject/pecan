#' Remotely execute R code
#'
#' Wrapped code should contain a `dput()` statement, or a warning will be thrown.
#' `dput()` is used to return outputs, so if it is absent, the output will be `NULL`.
#'
#' @author Alexey Shiklomanov
#' @param code R code to be run on remote, either as an unevaluated expression,
#'   a single character string, or a vector of strings to be run in sequence.
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
#' result <- remote.execute.R(code = code, host = host)
#'
#' code2 <- c("x <- 10", "y <- 7", "out <- list(x = seq(x), y = seq(y))", "dput(out)")
#' result <- remote.execute.R(code = code2, host = host)
#'
#' code3 <- "
#'   n <- 10
#'   x <- rnorm(n)
#'   y <- runif(n)
#'   df <- data.frame(norm = x, unif = y)
#'   dput(df)
#' "
#' result <- remote.execute.R(code = code3, host = host)
remote.execute.R <- function(code, host, stderr = TRUE, ...) {
  if (is.character(code)) {
    code_string <- code
  } else if (typeof(code) == "language") {
    code_string <- deparse(code)
  } else {
    PEcAn.logger::logger.severe("Code must be an R quoted expression or a character string.")
  }
  has_dput <- any(grepl("dput", code_string))
  if (!has_dput) {
    PEcAn.logger::logger.error("No dput statement found in code string.",
                               "This means no values will be returned.")
  }
  code_string_c <- gsub("\n", "; ", paste(code_string, collapse = '; '))
  code_string_c <- gsub("^; *", "", code_string_c)
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