##' get_table_column_names
##' @author Tempest McCabe
##'
##' @param table a table that is output from one of the find_* functions,
##' or a data.frame containing the output from multiple find_* functions. Could also be a vector of table names.
##' @param con a connection to the bety database.
##'
##'
##' @description This function will return a vector of the column names for a given table(s) in the bety database.
##' Useful for choosing which columns to include in the written-out table.
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##' @export
get_table_column_names <- function(table, con) {
  if (is.data.frame(table)) {
    if ("table_name" %in% names(table)) {
      table_factor <- as.factor(table$table_name)
      table_name <- levels(table_factor)
    } else {
      PEcAn.logger::logger.severe("Table needs either a 'table_names' column or be a character vector of table names")
    }
  } else if (is.vector(table)) {
    table_name <- table
  } else {
    PEcAn.logger::logger.severe("table must either be a dataframe or a vector")
  }
  column_names <- list()
  for (i in seq_along(table_name)) {
    query <- PEcAn.DB::db.query(paste("SELECT * from", table_name, "LIMIT 1"), con = con)
    column_names[[i]] <- colnames(query)
    names(column_names) <- table_name
  }
  return(column_names)
}
