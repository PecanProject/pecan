# extract the salinas socs ensemble to an EFI long-format CSV, the exact same
# shape 030_extract_sipnet_output.R writes for the statewide run
# (scenario,datetime,site_id,lat,lon,pft,parameter,variable,variable_type,prediction).
# the downscaling 030 is hardwired to the statewide output_<scenario> layout, so
# this lifts its read.output -> monthly roll-up -> long pivot logic and points it
# at the calibration run's itr/out/ENS-<ens>-<site> layout instead.
#
# usage:
#   Rscript extract_salinas_ensemble_output.R           # full: all members, 2005-2011
#   Rscript extract_salinas_ensemble_output.R --dry      # 2 members, year 2008 only

args <- commandArgs(trailingOnly = TRUE)
DRY <- "--dry" %in% args

salinas_dir <- "/projectnb/dietzelab/ccmmf/usr/akash/salinas_socs"
itr <- "itr2"
scenario <- "salinas_socs"                       # single scenario label
variables <- c("TotSoilCarb", "AGB")             # SOC pairs with white_salinas obs; AGB for context
out_csv <- file.path(salinas_dir, "ayushman_handoff",
                     if (DRY) "ensemble_output_DRY.csv" else "ensemble_output.csv")

no_cores <- max(future::availableCores() - 1, 1)
future::plan(future::multisession, workers = no_cores)

PEcAn.logger::logger.info("salinas ensemble extraction, dry =", DRY, ", cores =", no_cores)

# per-site lat/lon + pft
site_info <- readr::read_csv(file.path(salinas_dir, "site_info.csv"), show_col_types = FALSE)
stopifnot(all(c("id", "lat", "lon", "site.pft") %in% names(site_info)))

# same routing-pft map as 030 so the pft column matches the statewide csv
sipnet_to_routing_pft <- function(site_pft) {
  dplyr::case_when(
    site_pft %in% c("annual_crop", "grass") ~ "annual crop",
    site_pft == "temperate.deciduous" ~ "woody perennial crop",
    TRUE ~ NA_character_
  )
}

# pool (state) vs flux (rate) tag, same source table as 030
std_vars <- PEcAn.utils::standard_vars
pool_vars <- std_vars |>
  dplyr::filter(stringr::str_detect(tolower(Category), "pool")) |>
  dplyr::pull(Variable.Name)
flux_vars <- std_vars |>
  dplyr::filter(stringr::str_detect(tolower(Category), "flux")) |>
  dplyr::pull(Variable.Name)

# the run registry: runs_manifest.csv holds run_id (ENS-<ens>-<site_id>) + site_id
manifest <- readr::read_csv(file.path(salinas_dir, "output", itr, "runs_manifest.csv"),
                            show_col_types = FALSE) |>
  dplyr::filter(.data$type == "Ensemble") |>
  dplyr::mutate(
    # ENS-00001-socs_sys1 -> ens = 00001 (site_id has no dash so this is safe)
    ens = sub("^ENS-([0-9]+)-.*$", "\\1", .data$run_id),
    dir = file.path(salinas_dir, "output", itr, "out", .data$run_id)
  )

# run window from the year files actually on disk (robust to settings.xml
# structure; every member shares the same 2005-2011 window)
yr_files <- list.files(manifest$dir[[1]], pattern = "^[0-9]{4}\\.nc$")
years <- as.integer(sub("\\.nc$", "", yr_files))
start_year <- min(years)
end_year <- max(years)

if (DRY) {
  manifest <- manifest |> dplyr::slice_head(n = 2)
  start_year <- end_year <- 2008
}

# every run dir must exist, else fail loud rather than silently drop a member
missing_dirs <- manifest$dir[!dir.exists(manifest$dir)]
if (length(missing_dirs) > 0) {
  PEcAn.logger::logger.severe(length(missing_dirs), " run dirs missing, e.g. ",
                              paste(utils::head(basename(missing_dirs), 5), collapse = ", "))
}

PEcAn.logger::logger.info(nrow(manifest), " runs, years ", start_year, "-", end_year,
                          ", variables ", paste(variables, collapse = ", "))

# mute read.output's per-file logging for the duration of the parallel read
logger_level <- PEcAn.logger::logger.setLevel("OFF")
ens_raw <- furrr::future_pmap_dfr(
  list(dir = manifest$dir, ens = manifest$ens, site_id = manifest$site_id),
  function(dir, ens, site_id) {
    PEcAn.utils::read.output(
      runid = paste(ens, site_id, sep = "-"),
      outdir = dir,
      start.year = start_year,
      end.year = end_year,
      variables = variables,
      dataframe = TRUE,
      verbose = FALSE
    ) |>
      # roll each run to monthly in the worker so memory stays flat (same as 030)
      dplyr::mutate(datetime = lubridate::floor_date(.data$posix, "month")) |>
      dplyr::group_by(.data$datetime) |>
      dplyr::summarise(
        dplyr::across(tidyr::all_of(variables), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(site_id = .env$site_id, parameter = as.numeric(.env$ens))
  },
  .options = furrr::furrr_options(seed = TRUE)
)
PEcAn.logger::logger.setLevel(logger_level)

ens_results <- ens_raw |>
  dplyr::arrange(.data$parameter, .data$site_id, .data$datetime) |>
  tidyr::pivot_longer(tidyr::all_of(variables), names_to = "variable", values_to = "prediction") |>
  dplyr::left_join(
    site_info |> dplyr::transmute(site_id = as.character(id), lat, lon, sipnet_pft = site.pft),
    by = "site_id"
  ) |>
  dplyr::mutate(
    scenario = scenario,
    pft = sipnet_to_routing_pft(.data$sipnet_pft),
    variable_type = dplyr::case_when(
      variable %in% pool_vars ~ "pool",
      variable %in% flux_vars ~ "flux",
      TRUE ~ "unknown"
    )
  ) |>
  dplyr::select(scenario, datetime, site_id, lat, lon, pft,
                parameter, variable, variable_type, prediction)

readr::write_csv(ens_results, out_csv)
PEcAn.logger::logger.info("done: ", nrow(ens_results), " rows, ",
                          dplyr::n_distinct(ens_results$parameter), " members, ",
                          dplyr::n_distinct(ens_results$site_id), " sites -> ", out_csv)
