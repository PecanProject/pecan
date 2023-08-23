test_that("`download.url()` able to create the target dir for file download and passes the correct args to curl_download", {
  withr::with_dir(tempdir(), {
    mocked_res <- mockery::mock(TRUE)
    mockery::stub(download.url, 'url_found', TRUE)
    mockery::stub(download.url, 'curl::curl_download', mocked_res)
    res <- download.url('http://localhost/', 'test/index.html')    
    expect_true(file.exists('test'))
    args <- mockery::mock_args(mocked_res)
    expect_equal(args[[1]]$url, 'http://localhost/')
    expect_equal(args[[1]]$destfile, 'test/index.html')
  })
})