PEcAn.logger::logger.setQuitOnSevere(FALSE)

test_that("`query.format.vars` gives an informative error for a format id that does not exist", {
  queries <- character(0)
  fake_query <- function(query, con = NULL, ...) {
    queries <<- c(queries, query)
    if (grepl("from formats where id", query)) {
      return(data.frame())
    }
    stop("unexpected query: ", query)
  }
  mockery::stub(query.format.vars, "PEcAn.DB::db.query", fake_query)

  expect_error(
    query.format.vars(bety = 1, format.id = 0),
    "No format found with id 0"
  )
  # must stop before sending a malformed query for the (missing) mimetype
  expect_false(any(grepl("mimetypes", queries)))
})

test_that("`query.format.vars` gives an informative error for an input id with no format", {
  queries <- character(0)
  fake_query <- function(query, con = NULL, ...) {
    queries <<- c(queries, query)
    if (grepl("from formats as f join inputs", query) ||
        grepl("site_id from inputs", query)) {
      return(data.frame())
    }
    stop("unexpected query: ", query)
  }
  mockery::stub(query.format.vars, "PEcAn.DB::db.query", fake_query)

  expect_error(
    query.format.vars(bety = 1, input.id = 0),
    "No format found for input id 0"
  )
  expect_false(any(grepl("mimetypes", queries)))
})

test_that("`query.format.vars` still looks up the mimetype when the format exists", {
  fake_query <- function(query, con = NULL, ...) {
    if (grepl("from formats where id", query)) {
      return(data.frame(id = 1L, mimetype_id = 2L, name = "fmt"))
    }
    if (grepl("mimetypes where id", query)) {
      stop("reached mimetype query")
    }
    stop("unexpected query: ", query)
  }
  mockery::stub(query.format.vars, "PEcAn.DB::db.query", fake_query)

  expect_error(
    query.format.vars(bety = 1, format.id = 1),
    "reached mimetype query"
  )
})
