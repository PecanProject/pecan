#' Merge local data frame into SQL table
#'
#' @inheritParams insert_table
#' @param by Character vector of columns by which to perform merge. Defaults to all columns in `values`
#' @return Data frame: Inner join of SQL table and input data frame (as unevaluated "lazy query" table)
#' @export
#' @examples
#' library(dplyr)
#' library(RSQLite)
#' irisfile <- tempfile(fileext = ".sqlite")
#' irisdb <- dbConnect(SQLite(), irisfile)
#' copy_to(irisdb, iris[1:10,], name = "iris", overwrite = TRUE)
#' db_merge_into(iris[1:12,], "iris", irisdb)
#' tbl(irisdb, "iris") %>% count()
db_merge_into <- function(values, table, con, by = NULL) {
  if (is.null(by)) {
    by <- match_dbcols(values, table, con)
  }
  sql_tbl <- dplyr::tbl(con, table)
  values_merge <- dplyr::anti_join(values, sql_tbl, by = by, copy = TRUE)
  if (nrow(values_merge) < 1 || ncol(values_merge) < 1) {
    PEcAn.logger::logger.warn(
      "Input table for merge is empty. Skipping operation and returning NULL."
    )
    return(NULL)
  }
  insert <- insert_table(values_merge, table, con)
  dplyr::tbl(con, table) %>%
    dplyr::inner_join(values, copy = TRUE)
}
