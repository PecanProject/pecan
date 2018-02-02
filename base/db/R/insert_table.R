#' Insert R data frame into SQL database
#'
#' First, subset to matching columns. Then, make sure the local and SQL column 
#' classes match, coercing local to SQL as necessary (or throwing an error). 
#' Then, build an SQL string for the insert statement. Finally, insert into the 
#' database.
#'
#' @param values `data.frame` of values to write to SQL database
#' @param table Name of target SQL table, as character
#' @param coerce_col_class logical, whether or not to coerce local data columns 
#' to SQL classes. Default = `TRUE.`
#' @inheritParams db.query
#' @inherit db.query return
#' @export
#' @examples
#' library(dplyr)
#' irisfile <- tempfile(fileext = ".sqlite")
#' irisdb <- src_sqlite(irisfile, create = TRUE)
#' copy_to(irisdb, iris[1,], name = "iris", overwrite = TRUE)
#' insert_table(iris[-1,], "iris", irisdb$con)
#' tbl(irisdb, "iris")
insert_table <- function(values, table, con, coerce_col_class = TRUE) {
  values_fixed <- match_dbcols(values, table, con, coerce_col_class)
  insert_query <- build_insert_query(values_fixed, table, con = con)
  print(insert_query)
  db.query(insert_query, con)
}

#' Match column names and classes between local and SQL table
#'
#' @inheritParams insert_table
#' @return `values` `data.frame` with column names and classes matched to SQL
#' @export
match_dbcols <- function(values, table, con, coerce_col_class = TRUE) {
  use_cols <- match_colnames(values, table, con)
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
  # Load one row to get column types
  sql_row <- dplyr::tbl(con, table) %>% head(1) %>% collect()
  sql_types <- purrr::map(sql_row, class) %>%
    purrr::map_chr(1) %>%
    .[use_cols]
  values_types <- purrr::map(values_sub, class) %>% purrr::map_chr(1)
  type_mismatch <- sql_types != values_types
  if (sum(type_mismatch) > 0) {
    mismatch_string <- sprintf(
      "%s: local is %s, SQL is %s",
      names(values_types),
      values_types,
      sql_types
    )[type_mismatch]
    PEcAn.logger::logger.info(
      "Found type mismatches in the following columns: ",
      paste0(mismatch_string, collapse = "; ")
    )
    if (!coerce_col_class) {
      PEcAn.logger::logger.severe(
        "Type mismatch detected, and `coerce_col_class` is `FALSE`. ",
        "Fix column class mismatches manually."
      )
    } else {
      PEcAn.logger::logger.info(
        "Coercing local column types to match SQL."
      )
      # Coerce values data frame to these types
      values_fixed <- purrr::map2_dfc(values_sub, sql_types, as)
    }
  } else {
    values_fixed <- values_sub
  }
  values_fixed
}

#' Match names of local data frame to SQL table
#'
#' @inheritParams insert_table
match_colnames <- function(values, table, con) {
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
    dbplyr::sql_vector(ident(colnames(values)), collapse = ", "),
    dbplyr::sql(" "),
    dbplyr::sql("VALUES"),
    dbplyr::sql(" "),
    dbplyr::sql_vector(insert_list, parens = FALSE, collapse = ", "), ...
  )
}
