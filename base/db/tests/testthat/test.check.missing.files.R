test_that("`check_missing_files()` able to return correct missing files", {
    # Mock `file.size`
    mocked_size <- mockery::mock(100, 200)
    mockery::stub(check_missing_files, "file.size", mocked_size)

    res <- check_missing_files(
        result = list(data.frame(file = c("A", "B"))),
        existing.input = data.frame(),
        existing.dbfile = data.frame()
    )

    # Check that result has expected structure
    expect_equal(length(res), 2)
    expect_true(is.list(res[[1]]))
    expect_true(is.list(res[[2]]))

    # Verify mock was called correctly
    mockery::expect_called(mocked_size, 2)
    expect_equal(mockery::mock_args(mocked_size)[[1]], list("A"))
    expect_equal(mockery::mock_args(mocked_size)[[2]], list("B"))
})
