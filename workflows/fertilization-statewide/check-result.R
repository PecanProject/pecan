#!/usr/bin/env Rscript

config <- config::get(file = "workflows/fertilization-statewide/config.yml",
                      config = Sys.getenv("FERT_PROJECT", "default"))

options(arrow.unsafe_metadata = TRUE)

out_path <- config[["output_dir"]]
if (!dir.exists(out_path)) {
  PEcAn.logger::logger.severe("Output not found: ", out_path)
}

shards <- list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(shards) == 0) {
  PEcAn.logger::logger.severe("No parquet shards under: ", out_path)
}
ds <- arrow::open_dataset(shards)

# schema
print(ds$schema)

PEcAn.logger::logger.info(sprintf(
  "%d shards: %s ... %s",
  length(shards), basename(shards[1]), basename(shards[length(shards)])))

tot <- ds |> dplyr::count() |> dplyr::collect()
PEcAn.logger::logger.info(sprintf("total rows: %d", tot[["n"]]))

# rows per ensemble member
print(
  ds |>
    dplyr::count(.data$ens_id) |>
    dplyr::collect() |>
    as.data.frame()
)

# rows per year
print(
  ds |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::count(.data$year) |>
    dplyr::collect() |>
    dplyr::arrange(.data$year) |>
    as.data.frame()
)

# first 5 rows
print(ds |> dplyr::slice_head(n = 5) |> dplyr::collect() |> as.data.frame())

# nh4 and no3 total range (kg N/m^2)
stats <- ds |>
  dplyr::mutate(total_n = .data$nh4_n_kg_m2 + .data$no3_n_kg_m2) |>
  dplyr::summarize(
    min_total_n  = min(.data$total_n, na.rm = TRUE),
    max_total_n  = max(.data$total_n, na.rm = TRUE),
    mean_total_n = mean(.data$total_n, na.rm = TRUE)
  ) |>
  dplyr::collect()
print(stats |> as.data.frame())
