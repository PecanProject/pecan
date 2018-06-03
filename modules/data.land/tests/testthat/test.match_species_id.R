
context("Species matching")

test_that("Species matching works", {
  test_merge <- function(input_codes, format_name, bety, ...) {
      dat_merge <- match_species_id(input_codes = input_codes,
                                    format_name = format_name,
                                    bety = bety,
                                    ...)
      test_that(paste0('Species match works for format: ', format_name),
                {
                    expect_equal(dat_merge[1, 'genus'], 'Acer')
                    expect_equal(dat_merge[1, 'species'], 'rubrum')
                    expect_equal(dat_merge[2, 'genus'], 'Tsuga')
                    expect_equal(dat_merge[2, 'species'], 'canadensis')
                    expect_equal(nrow(dat_merge), length(input_codes))
                    expect_false(any(is.na(dat_merge$bety_species_id)))
                    expect_false(any(duplicated(dat_merge)))
                })
      return(dat_merge)
  }

  bety <- dplyr::src_postgres(dbname = 'bety',
                                user = 'bety',
                                password = 'bety')

  test_merge(c('ACRU', 'TSCA'), 'usda', bety)
  test_merge(c(316, 261), 'fia', bety)
  test_merge(c('Acer rubrum', 'Tsuga canadensis'), 'latin_name', bety)

  test_table <- data.frame(bety_species_id = c(30, 1419),
                           input_code = c('AceRub', 'TsuCan'))

  test_merge(input_codes = test_table$input_code, format_name = 'custom', bety = bety,
             translation_table = test_table)
})
