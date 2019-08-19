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
