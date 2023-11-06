test_that("`add_icon()` able to create the correct output file", {
  withr::with_dir(tempdir(), {
    add_icon(1, 2, 3)
    # check if file exists
    expect_true(file.exists("Rplots.pdf"))
  })
})