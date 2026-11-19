test_that("Yang routes each flux and member independently", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("xgboost", "1.7.8.1")
  skip_if(utils::packageVersion("xgboost") != "1.7.8.1")
  # The production R backend is a standalone process, with data.table attached.
  yang <- new.env(parent = globalenv())
  sys.source(system.file("python", "pecan_downscaling", "targets", "residuals.R",
                         package = "PEcAnAssimSequential", mustWork = TRUE), envir = yang)
  panel <- data.table::data.table(index = 1:5, utc = as.POSIXct("2012-07-16", tz = "UTC"),
                                  Site_ID = c("A", "B", NA, NA, NA), LC = c(1, 1, 1, 9, 9),
                                  nee = c(NA, 10, 10, 10, NA), le = c(20, NA, 20, NA, 20))
  for (field in yang$features) panel[[field]] <- rep(1, 5)
  panel$KGC <- rep("Dfb", 5)
  levels <- list(KGC = c("Dfb", "Dfc"), is_day = c("0", "1"), LC = c("1", "2"))
  model <- function(ec, constant) {
    x <- yang$yang_matrix(panel[1:2, , drop = FALSE], levels, ec)
    fit <- xgboost::xgb.train(list(base_score = constant, nthread = 1L),
                              xgboost::xgb.DMatrix(x, label = c(constant, constant)), nrounds = 1)
    list(model = fit, levels = levels, features = colnames(x))
  }
  bundles <- list(EC = model(TRUE, 2), "1" = model(FALSE, 3))
  models <- rep(list(bundles), 25)
  models[[2]][["1"]] <- NULL
  tower <- panel[1:2, c("Site_ID", "utc")]
  for (member in yang$members) tower[[member]] <- c(30, NA_real_)
  tower$ens02 <- c(NA, 40)
  nee <- yang$member_values(panel, tower, models, "NEE")
  le <- yang$member_values(panel, NULL, models, "LE")
  expect_equal(nee$ens01, c(30, 8, 7, 10, NA))
  expect_equal(nee$route01, c(1, 2, 2, 3, 0))
  expect_equal(nee$ens02, c(NA, 40, 10, 10, NA))
  expect_equal(nee$route02, c(0, 1, 3, 3, 0))
  expect_equal(le$ens01, c(18, NA, 17, NA, 20))
  expect_equal(le$route01, c(2, 0, 2, 0, 3))
})
