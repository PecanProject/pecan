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
#' expect_log(PEcAn.utils::get.model.output(), "update your workflow")
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
