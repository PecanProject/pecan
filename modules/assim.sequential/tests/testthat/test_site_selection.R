context("Site selection and smooth division")

test_that("smooth.div correctly distributes total and preserves length", {
  res1 <- smooth.div(100, c(25, 25, 25, 25))
  expect_equal(length(res1), 4)
  expect_equal(sum(res1), 100)

  res2 <- smooth.div(53, c(10.2, 15.7, 27.1))
  expect_equal(length(res2), 3)
  expect_equal(sum(res2), 53)
  expect_true(all(res2 == as.integer(res2)))

  # single element vector
  res3 <- smooth.div(42, 42)
  expect_equal(res3, 42)
})

test_that("site_selection validates required columns without length > 1 error", {
  # df with feature columns before lc and weights
  df_valid <- data.frame(
    feat1   = rnorm(20),
    feat2   = rnorm(20),
    lc      = rep(1:2, each = 10),
    weights = rep(1, 20)
  )

  # Missing lc
  df_no_lc <- data.frame(feat1 = 1:10, weights = rep(1, 10))
  expect_equal(site_selection(df_no_lc, tot.sample.size = 5), 0)

  # Missing weights
  df_no_weights <- data.frame(feat1 = 1:10, lc = rep(1, 10))
  expect_equal(site_selection(df_no_weights, tot.sample.size = 5), 0)

  # No feature columns (only lc and weights)
  df_no_features <- data.frame(lc = rep(1:2, each = 5), weights = rep(1, 10))
  expect_equal(site_selection(df_no_features, tot.sample.size = 5), 0)
})

test_that("site_selection handles NA rows and normalizes only feature columns", {
  df <- data.frame(
    feat1   = c(10, 20, 30, NA, 40),
    feat2   = c(100, 200, 300, 400, 500),
    lc      = c(1, 1, 2, 2, 2),
    weights = c(5, 10, 15, 20, 25)
  )

  # Mock faiss_kmeans to test normalization and pipeline without external Python dependency
  mock_kmeans <- function(data_matrix, k, ...) {
    list(
      centroids = matrix(0, nrow = k, ncol = ncol(data_matrix)),
      cluster_ids = rep(seq_len(k), length.out = nrow(data_matrix)),
      distances = rep(0, nrow(data_matrix)),
      total_within_ss = 100 / k
    )
  }

  testthat::with_mocked_bindings(
    faiss_kmeans = mock_kmeans,
    {
      res <- site_selection(
        df = df,
        tot.sample.size = 4,
        min.sample.size = 1,
        normalize = TRUE,
        max.cluster.num = 2,
        elbow.operation = FALSE
      )
      expect_true(is.list(res))
      expect_equal(length(res), 2) # 2 land cover classes
      # Check that original indices correspond to non-NA rows
      sampled_indices <- unlist(res)
      expect_false(4 %in% sampled_indices) # row 4 had NA
    }
  )
})

test_that("site_selection determines optimal cluster number with pathviewr elbow method", {
  df <- data.frame(
    feat1   = 1:20,
    feat2   = 21:40,
    lc      = rep(1:2, each = 10),
    weights = rep(1, 20)
  )

  # Mock total_within_ss with an elbow/bend at k = 2
  elbow_ss <- c(100, 20, 15, 10)
  mock_kmeans <- function(data_matrix, k, ...) {
    list(
      centroids = matrix(0, nrow = k, ncol = ncol(data_matrix)),
      cluster_ids = rep(seq_len(k), length.out = nrow(data_matrix)),
      distances = rep(0, nrow(data_matrix)),
      total_within_ss = elbow_ss[k]
    )
  }

  testthat::with_mocked_bindings(
    faiss_kmeans = mock_kmeans,
    {
      res <- site_selection(
        df = df,
        tot.sample.size = 6,
        min.sample.size = 2,
        normalize = TRUE,
        max.cluster.num = 4,
        elbow.operation = TRUE
      )
      expect_true(is.list(res))
      expect_equal(length(res), 2) # 2 land cover classes
      # Pathviewr identifies elbow at k = 2, giving 2 clusters per land cover class
      expect_equal(length(res[[1]]$cluster.ids), 2)
      expect_equal(length(res[[2]]$cluster.ids), 2)
    }
  )
})
