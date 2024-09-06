
context("align_pft")

con <- PEcAn.DB::db.open(PEcAn.DB::get_postgres_envvars(
    host = "localhost",
    user = "bety",
    password = "bety"))
teardown(PEcAn.DB::db.close(con))

observation_one <- c("AMCA3", "AMCA3", "AMCA3", "AMCA3")
observation_two <- c("a", "b", "a", "a")
format_one <- "species_USDA_symbol"
format_two <- "plant_functional_type"

table <- data.frame(
  plant_functional_type_one = c("AMCA3", "AMCA3", "ARHY", "ARHY"),
  plant_functional_type_two = c("a", "a", "b", "b"), # PFT groupings
  stringsAsFactors = FALSE)


test_that("align_data_to_data_pft", {

  expected <- list(
    bety_species_id = list(
      observation_one = data.frame(
        input_code = observation_one,
        bety_species_id = rep(23463, 4),
        genus = rep("Amaranthus", 4),
        species = rep("", 4),
        stringsAsFactors = FALSE),
      observation_two = NA),
    original = list(
      observation_one = observation_one,
      observation_two = observation_two),
    aligned = list(
      aligned_by_observation_one = c("AMCA3", "ARHY", "AMCA3", "AMCA3")))

  aligned_d2d <- align_data_to_data_pft(
    con = con,
    observation_one = observation_one,
    observation_two = observation_two,
    format_one = format_one,
    format_two = format_two,
    custom_table = table)

  expect_identical(aligned_d2d, expected)

  aligned_generic <- align_pft(
    con = con,
    observation_one = observation_one,
    observation_two = observation_two,
    format_one = format_one,
    format_two = format_two,
    custom_table = table,
    comparison_type = "data_to_data")

  expect_identical(aligned_generic, aligned_d2d)

})
