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

test_that("download availability checks receive the requested curl options", {
  for (retry in c(TRUE, FALSE)) {
    withr::with_tempfile("tf", {
      options <- list(userpwd = "test:password", httpauth = 1L)
      probe <- mockery::mock(TRUE)
      download <- mockery::mock(tf)
      mockery::stub(download.url, "url_found", probe)
      mockery::stub(download.url, "curl::curl_download", download)
      mockery::stub(download.url, "dir.create", TRUE)

      expect_equal(download.url("http://localhost/private", tf,
                                .opts = options, retry = retry), tf)
      expect_equal(mockery::mock_args(probe)[[1]]$.opts, options)
    })
  }
})

test_that("url_found applies curl options while making a header-only request", {
  handle <- curl::new_handle()
  make_handle <- mockery::mock(handle)
  fetch <- mockery::mock(list(status_code = 200L))
  mockery::stub(url_found, "curl::new_handle", make_handle)
  mockery::stub(url_found, "curl::curl_fetch_memory", fetch)
  options <- list(userpwd = "test:password", httpauth = 1L)

  expect_true(url_found("http://localhost/private", .opts = options))
  expect_equal(mockery::mock_args(make_handle)[[1]]$.list, options)
  expect_equal(mockery::mock_args(make_handle)[[1]]$nobody, 1L)
  expect_identical(mockery::mock_args(fetch)[[1]]$handle, handle)
})
