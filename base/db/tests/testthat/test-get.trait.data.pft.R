
context("get.trait.data.pft")


con <- db.open(
  params = list(user = "bety", password = "bety", host = "localhost"))
teardown(
  db.close(con)
)

test_that("error cases complain",{
  # PFT does not exist
  expect_error(
    get.trait.data.pft(
      pft = list(name = "NOTAPFT", outdir="/dev/null"),
      dbfiles="/dev/null",
      modeltype=NULL,
      dbcon = con),
    "Could not find pft")

  # PFT exists for multiple models
  expect_error(
    get.trait.data.pft(
      pft = list(name = "soil", outdir = "/dev/null"),
      modeltype = NULL,
      dbfiles="/dev/null",
      dbcon = con),
    "Multiple PFTs named soil")
})
