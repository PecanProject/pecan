# Example of querying available water content from SSURGO

devtools::load_all("~/projects/pecan/cimis-et/modules/data.land")

design_points <- readr::read_csv("~/projects/cimis-to-irrigation/design_points.csv") |>
  head(10)

mukeys_list <- purrr::map2(
  design_points$lon, design_points$lat,
  ~ PEcAn.data.land::ssurgo_mukeys_point(point = c(.x, .y), distance = 20)
)

all_mukeys <- unique(unlist(mukeys_list))

soil_data <- PEcAn.data.land::gSSURGO.Query(
  mukeys = all_mukeys,
  fields = c(
    "chorizon.hzdept_r",
    "chorizon.hzdepb_r",
    "chorizon.awc_r"
  )
)

result <- design_points |>
  dplyr::mutate(mukey = mukeys_list) |>
  tidyr::unnest(mukey) |>
  dplyr::mutate(mukey = as.numeric(mukey)) |>
  dplyr::left_join(
    soil_data |>
      dplyr::select(mukey, awc_r),
    by = "mukey"
  )

readr::write_csv(result, "_test_soil_inputs.csv")
