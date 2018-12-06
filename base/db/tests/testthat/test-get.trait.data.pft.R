
context("get.trait.data.pft")

con <- db.open(
  params = list(user = "bety", password = "bety", host = "localhost"))

dbdir <- file.path(tempdir(), "dbfiles")
outdir <- file.path(tempdir(), "outfiles")
loglevel <- PEcAn.logger::logger.getLevel()
PEcAn.logger::logger.setLevel("OFF")

teardown({
  db.close(con)
  unlink(c(dbdir, outdir), recursive=TRUE)
  PEcAn.logger::logger.setLevel(loglevel)
})

get_pft <- function(pftname) {
  get.trait.data.pft(
      pft = list(name = pftname, outdir=outdir),
      trait.names = "SLA",
      dbfiles = dbdir,
      modeltype = NULL,
      dbcon = con)
}


test_that("reference species and cultivar PFTs write traits properly",{
  skip("Disabled until Travis bety contains Pavi_alamo and Pavi_all (#1958)")
  pavi_sp <- get_pft("pavi")
  expect_equal(pavi_sp$name, "pavi")
  sp_csv = file.path(dbdir, "posterior", pavi_sp$posteriorid, "species.csv")
  sp_trt = file.path(dbdir, "posterior", pavi_sp$posteriorid, "trait.data.csv")
  expect_true(file.exists(sp_csv))
  expect_true(file.exists(sp_trt))
  expect_gt(file.info(sp_csv)$size, 40) # i.e. longer than the 40-char header
  expect_gt(file.info(sp_trt)$size, 215) # ditto 215-char header

  pavi_cv <- get_pft("Pavi_alamo")
  expect_equal(pavi_cv$name, "Pavi_alamo")
  cv_csv = file.path(dbdir, "posterior", pavi_cv$posteriorid, "cultivars.csv")
  cv_trt = file.path(dbdir, "posterior", pavi_cv$posteriorid, "trait.data.csv")
  expect_true(file.exists(cv_csv))
  expect_true(file.exists(cv_trt))
  expect_gt(file.info(cv_csv)$size, 63) # cultivar.csv headers are longer
  expect_gt(file.info(cv_trt)$size, 215)

  pavi_allcv <- get_pft("Pavi_all")
  expect_equal(pavi_allcv$name, "Pavi_all")
  allcv_csv = file.path(dbdir, "posterior", pavi_allcv$posteriorid, "cultivars.csv")
  allcv_trt = file.path(dbdir, "posterior", pavi_allcv$posteriorid, "trait.data.csv")
  expect_true(file.exists(allcv_csv))
  expect_true(file.exists(allcv_trt))
  expect_gt(file.info(allcv_csv)$size, 63)
  expect_gt(file.info(allcv_trt)$size, 215)


  expect_gt(file.info(allcv_csv)$size, file.info(cv_csv)$size)
  expect_gt(file.info(allcv_trt)$size, file.info(cv_trt)$size)
})

test_that("error cases complain",{
  expect_error(get_pft("NOTAPFT"), "Could not find pft")
  expect_error(get_pft("soil"), "Multiple PFTs named soil")
})
