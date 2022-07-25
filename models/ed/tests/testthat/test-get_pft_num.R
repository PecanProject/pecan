test_that("pft parsing util works", {
  x <- list(
    name = "ebifarm.c3grass",
    ed2_pft_number = "5",
    outdir = "myoutdir/pft/ebifarm.c3grass"
  )
  expect_equal(get_pft_num(x), 5)
  y <- list(
    name = "ebifarm.c3grass"
  )
  expect_type(get_pft_num(y), "integer")
})
