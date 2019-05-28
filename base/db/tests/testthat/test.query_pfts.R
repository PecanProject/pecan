context("Query PFTs")

dbcon <- check_db_test()

test_that("Querying PFT IDs works", {
  pfts <- c("temperate.Early_Hardwood", "temperate.Late_Hardwood")
  p1 <- query_pfts(dbcon, pfts, strict = TRUE)
  expect_s3_class(p1, "data.frame")
  expect_setequal(names(p1), c("id", "pft_type", "name"))
  expect_equal(nrow(p1), length(pfts))
  expect_equal(p1[["name"]], pfts)
})

test_that("Querying PFTs throws correct errors", {
  pfts <- c("temperate.Early_Hardwood", "not_a_real_pft")
  p2_msg <- capture.output(
    p2 <- query_pfts(dbcon, pfts, strict = FALSE),
    type = "message"
  )
  expect_equal(nrow(p2), 1)
  expect_equal(p2[["name"]], pfts[[1]])
  expect_match(p2_msg, "PFTs were not found")
  expect_match(p2_msg, pfts[[2]])

  expect_error(capture.output(
    p3 <- query_pfts(dbcon, pfts, strict = TRUE),
    type = "message"
  ), paste0("PFTs were not found", ".*", pfts[[2]]))
})
