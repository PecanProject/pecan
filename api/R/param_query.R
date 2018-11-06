#' Run prepared query
#'
#' @param con Database connection, as created by [RPostgres::dbConnect]
#' @param query Query template (character, length 1)
#' @param params Query parameters (unnamed list)
#' @return Query result, as `data.frame`
#' @author Alexey Shiklomanov
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
