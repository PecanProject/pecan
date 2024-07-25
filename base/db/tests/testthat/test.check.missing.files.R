test_that("`check_missing_files()` able to return correct missing files", {
    mocked_res <- mockery::mock(list(c("A", "B")))
    mockery::stub(check_missing_files, "purrr::map_dfr", data.frame(missing = c(FALSE), empty = c(FALSE)))
    res <- check_missing_files(
        result = list(data.frame(file = c("A", "B"))),
        outname = "test",
        existing.input = data.frame(),
        existing.dbfile = data.frame()
    )
    
    # Print the structure of `res` for debugging
    str(res)
    
    # This function returns a list as follows: return(list(result_sizes, outlist, existing.input, existing.dbfile))
    # Perform checks to compare results from stubbed functions to actual results
    expect_equal(nrow(res[[1]]), 1)
    expect_equal(res[[1]]$missing, FALSE)
    expect_equal(res[[1]]$empty, FALSE)
    expect_equal(res[[2]], "test")
    expect_equal(nrow(res[[3]][[1]]), 0)
    expect_equal(ncol(res[[3]][[1]]), 0)
    expect_equal(nrow(res[[4]][[1]]), 0)
    expect_equal(ncol(res[[4]][[1]]), 0)
})
