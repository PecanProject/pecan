#' Run prepared query
#'
#' @param con Database connection, as created by [RPostgres::dbConnect]
#' @param query Query template (character, length 1)
#' @param params Query parameters (unnamed list)
#' @return Query result, as `data.frame`
#' @author Alexey Shiklomanov
#' @export
param_query <- function(con, query, params) {
  stopifnot(
    class(con) == "PqConnection",
    is.character(query),
    length(query) == 1,
    is.list(params)
  )
  qry <- DBI::dbSendQuery(con, query)
  res <- DBI::dbBind(qry, params)
  on.exit(DBI::dbClearResult(res))
  DBI::dbFetch(res)
}

#' Run prepared statement (e.g. `insert`, `delete`, `update`)
#'
#' @inheritParams param_query
#' @return Summary of operation (output of [DBI::dbBind])
#' @author Alexey Shiklomanov
#' @export
param_statement <- function(con, query, params) {
  stopifnot(
    class(con) == "PqConnection",
    is.character(query),
    length(query) == 1,
    is.list(params)
  )
  qry <- DBI::dbSendStatement(con, query)
  res <- DBI::dbBind(qry, params)
  return(res)
  on.exit(DBI::dbClearResult(res))
}
