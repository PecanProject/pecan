context("query.traits")

con <- check_db_test()

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

test_that("connection is required", {
  expect_error(
    query.traits(ids = 938, priors = "SLA"),
    '"con" is missing')
})

# Test `query_traits` function, which has a slightly different API
test_that("query_traits works as expected", {
  # NOTE: Capture output used here to avoid polluting the testthat
  # output. The error messages ARE checked with `expect_error`.
  test_that("query_traits throws errors with invalid inputs when `strict` is TRUE.", {
    expect_error(
      capture.output(query_priors(
        c("temperate.Early_Hardwood", "not_a_real_PFT"),
        con = con, strict = TRUE
      ), type = "m"),
      regexp = "PFT: 'not_a_real_PFT'"
    )
    expect_error(
      capture.output(query_priors(
        "temperate.Early_Hardwood", c("sla", "not_a_real_trait"),
        con = con, strict = TRUE
      ), type = "m"),
      regexp = "Trait: 'not_a_real_trait'"
    )
  })

  test_that("query_traits expand argument works as expected", {
    pft <- c("Optics.Temperate_Early_Hardwood",
             "Optics.Temperate_Mid_Hardwood",
             "Optics.Temperate_Late_Hardwood")
    trait <- c("leaf_reflect_vis", "leaf_reflect_nir")
    pdat2 <- query_priors(pft, trait, con = con)
    expect_equal(nrow(pdat2), length(pft) * length(trait))
    expect_true(setequal(pdat2[["pft_name"]], pft))
    expect_true(setequal(pdat2[["name"]], trait))

    expect_error(
      capture.output(query_priors(pft, trait, con = con, expand = FALSE), type = "m"),
      regexp = "Unclear how to recycle"
    )
    pft_sub <- pft[1:2]
    pdat3 <- query_priors(pft_sub, trait, con = con, expand = FALSE)
    expect_equal(nrow(pdat3), length(pft_sub))
    expect_equal(pdat3[["pft_name"]], pft_sub)
    expect_equal(pdat3[["name"]], trait)
  })
})
