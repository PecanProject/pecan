# Tests for sa_run_samples
# The design says what each run is; this turns that plus the quantile tables
# into the parameter values each run uses.

sa_samples_fixture <- function() {
  list(
    pft1 = data.frame(
      SLA   = c(10, 20, 30),
      Vcmax = c(40, 50, 60),
      row.names = c("15.9", "50", "84.1")
    )
  )
}

sa_design_fixture <- function() {
  data.frame(
    param       = 1:5,
    sa_pft      = c(NA, "pft1", "pft1", "pft1", "pft1"),
    sa_trait    = c(NA, "SLA", "SLA", "Vcmax", "Vcmax"),
    sa_quantile = c("50", "15.9", "84.1", "15.9", "84.1"),
    stringsAsFactors = FALSE
  )
}


test_that("the median run holds every trait at its median", {
  per_run <- sa_run_samples(sa_samples_fixture(), sa_design_fixture())

  expect_equal(per_run$pft1$SLA[1], 20)
  expect_equal(per_run$pft1$Vcmax[1], 50)
})


test_that("a run moves only its own trait, leaving the rest at median", {
  per_run <- sa_run_samples(sa_samples_fixture(), sa_design_fixture())

  # row 2 moves SLA to its 15.9 quantile
  expect_equal(per_run$pft1$SLA[2], 10)
  expect_equal(per_run$pft1$Vcmax[2], 50)

  # row 5 moves Vcmax to its 84.1 quantile
  expect_equal(per_run$pft1$Vcmax[5], 60)
  expect_equal(per_run$pft1$SLA[5], 20)
})


test_that("the result has one row per design row and the shape the writers take", {
  per_run <- sa_run_samples(sa_samples_fixture(), sa_design_fixture())

  expect_named(per_run, "pft1")
  expect_true(is.data.frame(per_run$pft1))
  expect_equal(nrow(per_run$pft1), 5)
  expect_named(per_run$pft1, c("SLA", "Vcmax"))
})


test_that("a PFT the design never moves stays at its medians throughout", {
  sa_samples <- sa_samples_fixture()
  sa_samples$pft2 <- data.frame(
    Amax = c(1, 2, 3),
    row.names = c("15.9", "50", "84.1")
  )

  per_run <- sa_run_samples(sa_samples, sa_design_fixture())

  # only pft1 traits move in this design, so pft2 sits at its median every run
  expect_true(all(per_run$pft2$Amax == 2))
  expect_equal(nrow(per_run$pft2), 5)
})


test_that("non-PFT entries such as env are carried at their medians", {
  sa_samples <- sa_samples_fixture()
  sa_samples$env <- data.frame(
    temp = c(5, 15, 25),
    row.names = c("15.9", "50", "84.1")
  )

  per_run <- sa_run_samples(sa_samples, sa_design_fixture())

  # write.sa.configs passes env's medians alongside the PFTs, so this does too
  expect_true("env" %in% names(per_run))
  expect_true(all(per_run$env$temp == 15))
})


test_that("a design without the SA labels is rejected", {
  bare <- data.frame(param = 1:5)

  expect_error(sa_run_samples(sa_samples_fixture(), bare), "labels")
})