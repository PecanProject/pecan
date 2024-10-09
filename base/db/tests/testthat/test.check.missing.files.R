test_that("`check_missing_files()` able to return correct missing files", {
    # Mock `purrr::map_dfr`
    mocked_res <- mockery::mock(data.frame(file = c("A", "B"), file_size = c(100, 200), missing = c(FALSE, FALSE), empty = c(FALSE, FALSE)))
    mockery::stub(check_missing_files, "purrr::map_dfr", mocked_res)

    res <- check_missing_files(
        result = list(data.frame(file = c("A", "B"))),
        existing.input = data.frame(),
        existing.dbfile = data.frame()
    )

    # Print the structure of `res` for debugging
    str(res)

    expect_equal(length(res), 2)
    expect_true(is.list(res[[1]]))
    expect_true(is.list(res[[2]]))
})
