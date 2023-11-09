test_that("`check.lists` returns false for appropriate cases", {
  x <- data.frame(id = c(1, 2, 3))
  y <- data.frame(id = c(1, 2, 3, 4))

  # for unequal number of rows
  expect_false(check.lists(x, y))

  # for wrong filename passed 
  expect_false(check.lists(x, y, filename = "wrong.csv"))

  # if x and y are actually unequal
  y <- data.frame(id = c(1, 2, 4))
  expect_false(check.lists(x, y, filename = "species.csv"))
})

test_that("`check.lists` able to correctly work for matching data frames to lists read from csv files", {
  withr::with_tempfile("tf", fileext = ".csv",{
    x <- data.frame(id = c(1, 2, 3))
    y <- data.frame(id = c(1, 2, 3)) 
    write.csv(y, file = tf)
    expect_true(check.lists(x, read.csv(tf), filename = "species.csv"))
  })
})