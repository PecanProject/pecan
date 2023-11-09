library(PEcAn.DB)
library(testthat)
context("SQL insertion helper functions")

test_that(
  "RSQLite-dependent tests work",
  {
    skip_if_not_installed("RSQLite")
    irisdb <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    iris <- tibble::as_tibble(iris) %>%
      dplyr::mutate(
        Species = as.character(Species)
      )
    dplyr::copy_to(irisdb, iris[1,], "iris", overwrite = TRUE)
    # Add extra column to see if it's successfully ignored
    iris2 <- dplyr::mutate(iris, extracol = seq_len(nrow(iris)))
    iris_insert <- iris2[2:10,]
    .insert <- insert_table(iris_insert, "iris", irisdb)
    test_that(
      "Subset of iris was inserted into database",
      {
        iris_insert_test <- dplyr::tbl(irisdb, "iris") %>% dplyr::collect()
        expect_equal(iris[1:10,], iris_insert_test)
      }
    )

    iris_merge <- iris2[5:12,]
    out_merge <- db_merge_into(iris_merge, "iris", irisdb) %>%
      dplyr::collect()
    iris_merge_nrow <- dplyr::tbl(irisdb, "iris") %>%
      dplyr::count() %>%
      dplyr::pull(n)
    test_that(
      "Only subset of iris data were merged",
      {
        expect_equal(out_merge, iris2[5:12,])
        out_merge2 <- db_merge_into(iris_merge, "iris", irisdb) %>%
          dplyr::collect()
        expect_equal(out_merge, out_merge2)
      }
    )

    test_that(
      "Extra column (not in SQL) was retained in `out_merge`",
      {
        expect_true("extracol" %in% colnames(out_merge))
      }
    )
  })

test_that("`match_colnames()` returns intersection of column names of a dataframe to a table", {
  mockery::stub(match_colnames, 'dplyr::tbl', data.frame(id = 1, name = 'test', value = 1))
  expect_equal(
    match_colnames(values = data.frame(id = 1, name = 'test'), table = 'test', con = 1),
    c('id', 'name')
  )
})