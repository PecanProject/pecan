test_that("`drop.columns` able to drop specified columns from data", {
  data <- data.frame(
    id = c(1, 2, 3),
    name = c("a", "b", "c"),
    value = c(1.2, 4.5, 6.7)
  )

  updated_data <- drop.columns(data, c("name", "not_a_column"))
  expect_equal(colnames(updated_data), c("id", "value"))
})
