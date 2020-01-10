#' Execute a PostgreSQL prepared query or statement
#'
#' This provides a safe and efficient way of executing a query or
#' statement with a list of parameters to be substituted.
#'
#' A prepared statement consists of a template query (`query`), which
#' is compiled prior to execution, and a series of parameters
#' (`params`) that are passed into the relevant spots in the template
#' query. In R's `DBI` database interface, this uses three statements:
#' [DBI::dbSendQuery] to create the template query, [DBI::dbBind] to
#' bind parameters to that query, and [DBI::dbFetch] to retrieve the
#' results. Statements ([DBI::dbSendStatement]) work the same way,
#' except there are no results to fetch with [DBI::dbFetch].
#'
#' Prepared statements have several important advantages. First of
#' all, they are automatically and efficiently vectorized, meaning
#' that it is possible to build a single query and run it against a
#' vector of parameters. Second, they automatically enforce strict
#' type checking and quoting of inputs, meaning that they are secure
#' against SQL injection attacks and input mistakes (e.g. giving a
#' character when the table expects a number).
#'
#' @param con Database connection, as created by [RPostgres::dbConnect]
#' @param query Query template (character, length 1)
#' @param params Query parameters (unnamed list)
#' @return For `prepared_query`, the query result as a `data.frame`.
#'   `prepared_statement` exits silently on success.
#' @author Alexey Shiklomanov
#' @examples
#' \dontrun{
#' prepared_query(con, paste(
#'   "SELECT id, folder FROM workflows",
#'   "WHERE user_id = $1"
#' ), list(my_user_id))
#'
#' prepared_statement(con, paste(
#'   "INSERT INTO workflows (id, site_id, model_id, folder)",
#'   "VALUES ($1, $2, $3, $4)"
#' ), list(workflow_id, my_site_id, my_model_id, my_folder))
#'
#' # Note that queries and statements are automatically vectorized
#' # The below query will execute two searches, and return the results
#' # of both in one data.frame
#' prepared_query(con, paste(
#'   "SELECT * FROM dbfiles",
#'   "WHERE file_name ILIKE $1",
#' ), list(c("%cruncep%", "%gfdl%")))
#'
#' # Similarly, this will create two workflows, all with the same
#'   model_id (1) but different site_ids (33, 67)
#' prepared_statement(con, paste(
#'   "INSERT INTO workflows (site_id, model_id)",
#'   "VALUES ($1, $2)"
#' ), list(c(33, 67), 1))
#' 
#'}
#' @export
prepared_query <- function(con, query, params) {
  stopifnot(
    inherits(con, "PqConnection"),
    is.character(query),
    length(query) == 1,
    is.list(params)
  )
  qry <- DBI::dbSendQuery(con, query)
  res <- DBI::dbBind(qry, params)
  on.exit(DBI::dbClearResult(res), add = TRUE)
  DBI::dbFetch(res)
}

#' @rdname prepared_query
#' @export
prepared_statement <- function(con, query, params) {
  stopifnot(
    inherits(con, "PqConnection"),
    is.character(query),
    length(query) == 1,
    is.list(params)
  )
  qry <- DBI::dbSendStatement(con, query)
  res <- DBI::dbBind(qry, params)
  on.exit(DBI::dbClearResult(res), add = TRUE)
}
