#!/usr/bin/env Rscript

config <- config::get(file = "workflows/ncc-statewide/config.yml",
                      config = Sys.getenv("NCC_PROJECT", "default"))

options(arrow.unsafe_metadata = TRUE)

out_path <- file.path(config[["output_dir"]], config[["output_subdir"]])
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

# rows per material
print(
  ds |>
    dplyr::count(.data$material) |>
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

# org_c and org_n ranges
stats <- ds |>
  dplyr::summarize(
    min_org_c  = min(.data$org_c_kg_m2, na.rm = TRUE),
    max_org_c  = max(.data$org_c_kg_m2, na.rm = TRUE),
    mean_org_c = mean(.data$org_c_kg_m2, na.rm = TRUE),
    min_org_n  = min(.data$org_n_kg_m2, na.rm = TRUE),
    max_org_n  = max(.data$org_n_kg_m2, na.rm = TRUE),
    mean_org_n = mean(.data$org_n_kg_m2, na.rm = TRUE)
  ) |>
  dplyr::collect()
print(stats |> as.data.frame())
