#' Insert R data frame into SQL database
#'
#' First, subset to matching columns. Then, build an SQL string for the insert 
#' statement. Finally, insert into the database.
#'
#' @param values `data.frame` of values to write to SQL database
#' @param table Name of target SQL table, as character
#' @inheritParams db.query
#' @inherit db.query return
#' @export
#' @examples
#' library(dplyr)
#' irisfile <- tempfile(fileext = ".sqlite")
#' irisdb <- src_sqlite(irisfile, create = TRUE)
#' copy_to(irisdb, iris[1,], name = "iris", overwrite = TRUE)
#' insert_table(iris[-1,], "iris", irisdb)
#' tbl(irisdb, "iris")
insert_table <- function(values, table, con) {
  use_cols <- match_dbcols(values, table, con)
  if (length(use_cols) < 1) {
    PEcAn.logger::logger.severe(
      "No columns match between input and target table."
    )
  }
  PEcAn.logger::logger.debug(
    "Matched the following cols: ",
    paste(use_cols, collapse = ", ")
  )
  values_sub <- values[, use_cols]
  insert_query <- build_insert_query(values, table, con = con)
  db.query(con, insert_query)
}

#' Match names of local data frame to SQL table
#'
#' @inheritParams insert_table
match_dbcols <- function(values, table, con) {
  tbl_db <- dplyr::tbl(con, table)
  table_cols <- dplyr::tbl_vars(tbl_db)
  values_cols <- colnames(values)
  intersect(values_cols, table_cols)
}

#' Build query to insert R data frame into SQL table
#'
#' @inheritParams insert_table
#' @param ... Additional arguments to [dbplyr::build_sql]
build_insert_query <- function(values, table, ...) {
  value_list <- purrr::map(seq_len(nrow(values)), ~as.list(values[.x, ]))

  insert_list <- value_list %>%
    purrr::map(unname) %>%
    purrr::map(dbplyr::escape) %>%
    purrr::map(dbplyr::sql_vector)

  dbplyr::build_sql(
    dbplyr::sql("INSERT INTO"),
    dbplyr::sql(" "),
    dbplyr::ident(table),
    dbplyr::sql(" "),
    dbplyr::sql_vector(dbplyr::escape(colnames(values))),
    dbplyr::sql(" "),
    dbplyr::sql("VALUES"),
    dbplyr::sql(" "),
    dbplyr::sql_vector(insert_list, parens = FALSE, collapse = ", "), ...
  )
}
