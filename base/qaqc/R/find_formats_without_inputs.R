##' Find formats in bety that have no input record in bety
##'
##' @author Tempest McCabe
##'
##' @param con database connection object
##' @param user_id_code  Optional parameter to search by user_id
##' @param created_after Optional parameter to search by creation date. Date must be in form 'YYYY-MM-DD'.
##' @param created_before Optional parameter to search by creation date. Can be used in conjunction with created_after to specify a specific window. Date must be in form 'YYYY-MM-DD'.
##' @param updated_after Optional parameter to search all entries updated after a certain date. Date must be in form 'YYYY-MM-DD'.
##' @param updated_before Optional parameter to search all entries updated before a certain date. Date must be in form 'YYYY-MM-DD'.
##' @param con connection the the bety database
##'
##'
##' @description This is a function that returns a dataframe with all of the format entries that have no associated input records.
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##' @importFrom rlang .data
##' @export
find_formats_without_inputs <- function(con, user_id_code = NULL, created_after = NULL, updated_after = NULL, created_before = NULL, updated_before = NULL) {
  input_command <- dplyr::tbl(con, "inputs")

  format_command <- dplyr::tbl(con, "formats")

  if (!is.null(user_id_code)) {
    format_command <- dplyr::filter(format_command, .data$user_id == !!user_id_code)
  }
  if (!is.null(created_before)) {
    format_command <- dplyr::filter(format_command, .data$created_at < !!created_before)
  }
  if (!is.null(created_after)) {
    format_command <- dplyr::filter(format_command, .data$created_at > !!created_after)
  }
  if (!is.null(updated_before)) {
    format_command <- dplyr::filter(format_command, .data$updated_at < !!updated_before)
  }
  if (!is.null(updated_after)) {
    format_command <- dplyr::filter(format_command, .data$updated_at > !!updated_after)
  }

  format_command <- as.data.frame(format_command)
  input_command <- as.data.frame(input_command)

  colnames(format_command)[1] <- "format_id"
  formats_without_inputs <- dplyr::anti_join(format_command, input_command, by = "format_id")
  colnames(formats_without_inputs)[1] <- "id"

  formats_without_inputs$table_name <- rep("formats", length.out = length(formats_without_inputs[, 1]))

  return(formats_without_inputs)
}
