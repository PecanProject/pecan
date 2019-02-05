context("query.traits")

con <- tryCatch(db.open(params = list(
  user = "bety",
  password = "bety",
  host = "localhost"
)), error = function(e) {
  skip("Skipping tests because unable to establish database connection.")
})

teardown({
  db.close(con)
})

round3 <- function(x){ round(stats::median(x, na.rm = TRUE), digits = 3) }


test_that("prints medians and returns a list", {
  msgs <- capture.output(
    {res <- query.traits(
      ids=938, # Switchgrass
      priors=c("SLA", "Vcmax", "not_a_trait"),
      con=con)},
   type = "message")
  expect_length(res, 2)
  expect_s3_class(res$SLA, "data.frame")
  expect_s3_class(res$Vcmax, "data.frame")

  cv_msgs <- capture.output(
    {cv_res <- query.traits(
      ids=10, # Switchgrass cultivar 'Cave-In-Rock'
      priors=c("SLA", "LAI", "not_a_trait"),
      con=con,
      ids_are_cultivars = TRUE)},
    type = "message")
  expect_length(cv_res, 2)
  expect_s3_class(cv_res$SLA, "data.frame")
  expect_s3_class(cv_res$LAI, "data.frame")

  # These test query.trait.data more than query.traits, but it's easy to do here
  expect_match(
    msgs,
    paste("Median SLA :", round3(res$SLA$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    msgs,
    paste("Median Vcmax :", round3(res$Vcmax$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    cv_msgs,
    paste("Median SLA :", round3(cv_res$SLA$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    cv_msgs,
    paste("Median LAI :", round3(cv_res$LAI$mean)),
    fixed = TRUE,
    all = FALSE)
})


test_that("returns empty list if no trait data found", {
  expect_equal(query.traits(ids=1, priors="not_a_trait", con=con), list())
})

