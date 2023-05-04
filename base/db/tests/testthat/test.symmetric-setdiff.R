context("Symmetric setdiff")

test_that("Symmetric setdiff works", {
  con <- check_db_test()
  x <- dplyr::tbl(con, "traits") %>%
    dplyr::filter(!is.na(time)) %>%
    dplyr::arrange(dplyr::desc(created_at)) %>%
    dplyr::select(-mean, -stat) %>%
    dplyr::collect()
  y <- x %>%
    dplyr::mutate_if(~inherits(., "difftime"), as.character)
  msg <- paste(capture.output(xydiff <- symmetric_setdiff(x, y), type = "message"),
               collapse = "\n")
  expect_match(msg, "Detected at least one `integer64` column")
  expect_equal(nrow(xydiff), 0)
})

test_that("Unequal dfs compare unequal", {
  expect_error(
    symmetric_setdiff(data.frame(a = 1L), data.frame(b = 1L)),
    "Cols in `?x`? but not `?y")
  d <- symmetric_setdiff(data.frame(a = 1:3L), data.frame(a = 1:4L))
  expect_length(d$a, 1L)
  expect_equal(d$a, 4L)
  expect_equal(d$source, "y")

})

test_that("symmetric inputs give same output", {
  x <- data.frame(a=1:3L, b=LETTERS[1:3L])
  y <- data.frame(a=2:5L, b=LETTERS[2:5L])
  xy <- symmetric_setdiff(x, y)
  yx <- symmetric_setdiff(y, x)
  purrr::walk2(xy, yx, expect_setequal)
  expect_equal(
    # left input aways labeled x -> xy$source is inverse of yx$source
    dplyr::select(xy, -source) %>% dplyr::arrange(a),
    dplyr::select(yx, -source) %>% dplyr::arrange(a))
})
