#' Expectation: Does PEcAn logger produce warning/debug/error?
#'
#' Tests whether PEcAn.logger produced the expected output.
#' Modeled after testthat::expect_message, but looks for output on stderr
#' (where PEcAn.logger writes it) rather than a message() object
#'
#' @param object object to test, probably a PEcAn function call
#' @param regexp pattern expected in the output
#' @param ... other arguments passed on to \code{\link[testthat]{expect_match}}
#' @examples
#' expect_log(PEcAn.logger::logger.debug("test"), "DEBUG.*test")
#' expect_log(cat("Hello", file = stderr()), "Hello")
#' # Only messages on stderr are recognized
#' expect_failure(expect_log("Hello", "Hello"))
#'
expect_log <- function(object, regexp, ...){
	qobj <- rlang::enquo(object)
	msg <- capture.output(
		{val <- rlang::eval_tidy(qobj)},
		type = "message")
	label = rlang::expr_label(rlang::get_expr(qobj))

	expect(
		length(msg) > 0,
		sprintf("%s did not produce any log messages", label))
	msg = paste(msg, collapse = "\n")
	expect(
		grepl(regexp, msg,  ...),
		sprintf(
			"%s does not match %s.\nActual value: \"%s\"",
			label,
			encodeString(regexp, quote = "\""),
			encodeString(msg)))

	invisible(val)
}


#' Expectation: Does this directory contain all listed files?
#' @param object directory to look in
#' @param files character vector of filenames expected
#' @others_ok logical: allow files not listed in `paths`?
#' @param ... passed on to list.files
expect_files <- function(object, files, others_ok = TRUE, ...) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  files_present <- list.files(path = act$val, ...)
  files_found <- files %in% files_present
  others_found <- !(files_present %in% files)


  if (all(files_found) && (others_ok || !any(others_found))) {
    succeed()
    return(invisible(act$val))
  }

  msg <- ""
  if (!all(files_found)) {
    msg <- sprintf(
      "%s does not contain files(s) %s.",
      act$lab,
      paste(files[!files_found], collapse = ", ")
    )
  }
  if (!others_ok && any(others_found)) {
    msg <- sprintf(
      "%s %s contains unexpected files(s) %s.",
      msg,
      act$lab,
      paste(files_present[others_found], collapse = ", ")
    )
  }
  fail(msg)

  invisible(act$val)
}
