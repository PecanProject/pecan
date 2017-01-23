library(PEcAn.data.land)
library(testthat)

test_merge <- function(input_codes, format_name, betydb) {
    dat_merge <- match_species_id(input_codes = input_codes, 
                                  format_name = format_name,
                                  betydb = betydb)
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

betydb <- dplyr::src_postgres(dbname = 'bety',
                              user = 'bety',
                              password = 'bety')

test_merge(c('ACRU', 'TSCA'), 'usda', betydb)
test_merge(c(316, 261), 'fia', betydb)
