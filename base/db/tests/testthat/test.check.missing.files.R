test_that("`check_missing_files()` able to return correct missing files", {
    # Mock `purrr::map_dfr`
    mocked_size <- mockery::mock(100,200)
    mockery::stub(check_missing_files, "file.size", mocked_res)

    res <- check_missing_files(
        result = list(data.frame(file = c("A", "B"))),
        existing.input = data.frame(),
        existing.dbfile = data.frame()
    )


    expect_equal(length(res), 2)
    expect_true(is.list(res[[1]]))
    expect_true(is.list(res[[2]]))
})
