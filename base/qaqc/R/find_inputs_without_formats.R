##' Find inputs in bety with no format records
##' @author Tempest McCabe
##'
##' @param user_id  Optional parameter to search by user_id
##' @param created_before,created_after Optional parameter to search by creation date. Date must be in form 'YYYY-MM-DD'
##' @param updated_before,updated_after Optional parameter to search all entried updated after a certain date. Date must be in form 'YYYY-MM-DD'
##' @param con connection the the bety database
##'
##'
##' @description This is a function that returns a dataframe with all of the input entries that have no associated format records.
##' This is very rare in the database.
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##' @importFrom rlang .data
##' @export
find_inputs_without_formats <- function(con, user_id = NULL, created_after = NULL, updated_after = NULL, created_before = NULL, updated_before = NULL) {
  input_command <- dplyr::tbl(con, "inputs")

  format_command <- dplyr::tbl(con, "formats")

  if (!is.null(user_id)) {
    input_command <- dplyr::filter(input_command, .data$user_id == !!user_id)
  }
  if (!is.null(created_before)) {
    input_command <- dplyr::filter(input_command, .data$created_at < !!created_before)
  }
  if (!is.null(created_after)) {
    input_command <- dplyr::filter(input_command, .data$created_at > !!created_after)
  }
  if (!is.null(updated_before)) {
    input_command <- dplyr::filter(input_command, .data$updated_at < !!updated_before)
  }
  if (!is.null(updated_after)) {
    input_command <- dplyr::filter(input_command, .data$updated_at > !!updated_after)
  }

  format_command <- as.data.frame(format_command)
  input_command <- as.data.frame(input_command)

  colnames(format_command)[1] <- "format_id"
  inputs_without_formats <- dplyr::anti_join(input_command, format_command, by = "format_id")
  colnames(inputs_without_formats)[1] <- "id"

  inputs_without_formats$table_name <- rep("inputs", length.out = length(inputs_without_formats[, 1]))

  return(inputs_without_formats)
}
