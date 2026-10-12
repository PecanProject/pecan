test_that("`db.exists` returns FALSE without attempting a write test if the primary key lookup fails", {
  queries <- character(0)
  fake_query <- function(query, con = NULL, ...) {
    queries <<- c(queries, query)
    if (grepl("role_table_grants", query)) {
      return(data.frame(privilege_type = c("SELECT", "INSERT", "UPDATE")))
    }
    if (grepl("LIMIT 1", query)) {
      return(data.frame(id = 1L, name = "a"))
    }
    if (grepl("pg_attribute", query)) {
      stop("could not look up primary key")
    }
    stop("unexpected query: ", query)
  }

  mockery::stub(db.exists, "db.open", "fake-connection")
  mockery::stub(db.exists, "db.close", NULL)
  mockery::stub(db.exists, "db.query", fake_query)
  mockery::stub(db.exists, "PEcAn.logger::logger.error", NULL)

  params <- list(user = "someuser", dbname = "somedb")
  expect_false(db.exists(params, write = TRUE, table = "sometable"))
  expect_false(any(grepl("^\\s*UPDATE", queries)))
})

test_that("`db.exists` returns TRUE for a table with read access when write is not requested", {
  fake_query <- function(query, con = NULL, ...) {
    if (grepl("role_table_grants", query)) {
      return(data.frame(privilege_type = "SELECT"))
    }
    if (grepl("LIMIT 1", query)) {
      return(data.frame(id = 1L))
    }
    if (grepl("pg_attribute", query)) {
      return(data.frame(attname = "id", format_type = "bigint"))
    }
    stop("unexpected query: ", query)
  }

  mockery::stub(db.exists, "db.open", "fake-connection")
  mockery::stub(db.exists, "db.close", NULL)
  mockery::stub(db.exists, "db.query", fake_query)

  params <- list(user = "someuser", dbname = "somedb")
  expect_true(db.exists(params, write = FALSE, table = "sometable"))
})
