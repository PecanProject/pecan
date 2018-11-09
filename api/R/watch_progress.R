#' Retrieve the current output of a workflow given its ID
#'
#' @param workflow_id Workflow ID (character or numeric)
#' @param ... Additional arguments to [output_url]
#' @return Workflow output, as character vector with one item per line
#'   in `workflow.Rout` (as returned by `readLines`).
#' @author Alexey Shiklomanov
#' @export
workflow_output <- function(workflow_id, ...) {
  readLines(output_url(workflow_id, "workflow.Rout", ...))
}

#' Continuously monitor the progress of a workflow until it completes.
#' To exit early, send an interrupt signal (Control-C).
#'
#' @inheritParams workflow_output
#' @param nlines Print the last N lines (numeric, default = 10)
#' @param sleep Number of seconds to sleep between status updates
#'   (numeric, default = 3)
#' @return If successful
#' @author Alexey Shiklomanov
#' @export
watch_workflow <- function(workflow_id, nlines = 10, sleep = 3, ...) {
  repeat {
    output <- tryCatch(
      workflow_output(workflow_id, ...),
      error = function(e) "Unable to access workflow output"
    )
    out_sub <- tail(output, nlines)
    message(paste(out_sub, collapse = "\n"), "\n----------------\n")
    if (any(grepl("PEcAn Workflow Complete", output))) {
      return(invisible(NULL))
    }
    Sys.sleep(sleep)
  }
}
