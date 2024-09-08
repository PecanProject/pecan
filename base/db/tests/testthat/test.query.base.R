context("test db.query")

test_that("db.query can execute a trivial SQL statement and return results",{  
    con <- check_db_test()
    ans <- db.query("select count(*) from traits;", con = con)
    expect_is(ans, "data.frame")
    expect_true(is.numeric(ans[,1]))
    expect_true(length(ans) == 1)
    try(db.close(con))
})
