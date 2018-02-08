#' Merge local data frame into SQL table
#'
#' @inheritParams insert_table
#' @inheritDotParams insert_table
#' @param by Character vector of columns by which to perform merge. Defaults to all columns in `values`
#' @return Data frame: Inner join of SQL table and input data frame (as unevaluated "lazy query" table)
#' @export
#' @examples
#' irisdb <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' dplyr::copy_to(irisdb, iris[1:10,], name = "iris", overwrite = TRUE)
#' db_merge_into(iris[1:12,], "iris", irisdb)
#' dplyr::tbl(irisdb, "iris") %>% dplyr::count()
db_merge_into <- function(values, table, con, by = NULL, drop = FALSE, ...) {
  values_fixed <- match_dbcols(values, table, con, drop = FALSE)
  if (is.null(by)) {
    by <- match_colnames(values, table, con)
  }
  sql_tbl <- dplyr::tbl(con, table)
  values_merge <- dplyr::anti_join(values_fixed, sql_tbl, by = by, copy = TRUE)
  if (nrow(values_merge) < 1 || ncol(values_merge) < 1) {
    PEcAn.logger::logger.warn(
      "Input table for merge is empty."
    )
  } else {
    insert <- insert_table(values_merge, table, con, ...)
  }
  dplyr::tbl(con, table) %>%
    dplyr::inner_join(values_fixed, copy = TRUE)
}
