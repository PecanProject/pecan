library(PEcAn.DB)
library(DBI)
library(testthat)
library(dplyr)
context("SQL insertion helper functions")

if (requireNamespace("RSQLite")) {
  library(RSQLite)
  iris_file <- tempfile(fileext = ".sqlite")
  irisdb <- dbConnect(SQLite(), iris_file)
  iris <- as_tibble(iris) %>%
    mutate(
      Species = as.character(Species)
    )

  copy_to(irisdb, iris[1,], "iris", overwrite = TRUE)
  # Add extra column to see if it's successfully ignored
  iris2 <- mutate(iris, extracol = row_number())
  iris_insert <- iris2[2:10,]
  .insert <- insert_table(iris_insert, "iris", irisdb)
  iris_insert_test <- tbl(irisdb, "iris") %>% collect()
  test_that(
    "Subset of iris was inserted into database",
    {
      expect_equal(iris[1:10,], iris_insert_test)
    }
  )

  iris_merge <- iris2[5:12,]
  out_merge <- db_merge_into(iris_merge, "iris", irisdb)
  out_merge2 <- db_merge_into(iris_merge, "iris", irisdb)
  iris_merge_nrow <- tbl(irisdb, "iris") %>%
    count() %>%
    pull(n)
  test_that(
    "Only subset of iris data were merged",
    {
      expect_equal(collect(out_merge), iris[5:12,])
      expect_equal(collect(out_merge), collect(out_merge2))
    }
  )

  test_that(
    "Extra column (not in SQL) was retained in `out_merge`",
    {
      expect_true("extracol" %in% colnames(out_merge))
    }
  )

} else {
  message("Skipping insert tests because `RSQLite` not installed.")
}

