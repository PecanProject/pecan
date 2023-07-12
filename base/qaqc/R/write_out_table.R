##' write_out_table
##' @author Tempest McCabe
##'
##' @param table a table that is output from one of the find_* functions
##' @param table_name name of table
##' @param outdir path to folder into which the editable table will be written
##' @param relevant_table_columns a list of all columns to keep. ID and table name will be automatically included.
##'
##'
##' @description This is a function that returns a dataframe with all of the format entries that have no associated input records.
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##' @export
write_out_table <- function(table, table_name, outdir, relevant_table_columns) {
  if (!"id" %in% relevant_table_columns) {
    relevant_table_columns <- c(relevant_table_columns, "id")
  }
  if (!"table_name" %in% relevant_table_columns) {
    relevant_table_columns <- c(relevant_table_columns, "table_name")
  }
  if (!any(c("id", "table_name") %in% names(table))) {
    PEcAn.logger::logger.severe("table provided doesn't have a table_name or id column or both. ")
  }


  table <- table[, (relevant_table_columns)]
  utils::write.table(table, file = paste(outdir, "/query_of_", table_name, sep = ""), row.names = FALSE, sep = "|")
}
