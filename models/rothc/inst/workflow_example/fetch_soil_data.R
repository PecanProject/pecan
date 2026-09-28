#!/usr/bin/env Rscript

# creates ensembles of soil condition files for each requested location.

## run time option parsing
options <- list(
  optparse::make_option("--site_info_path",
    default = "site_info.csv",
    help = "Path to a csv file with at least columns `id`, `lat`, `lon`",
  ),
  optparse::make_option("--out_dir",
    default = "data/soil/",
    help = paste(
      "Output path: Will contain one subdirectory per site_id,",
      "containing n_ensemble netCDF files each named",
      "gSSURGO_soil_<n>.dat"
    )
  ),
  optparse::make_option("--n_ens",
    default = 20,
    help = "number of files to generate per site"
  ),
  optparse::make_option("--overwrite",
    default = FALSE,
    help = paste(
      "Replace files for existing sites, or skip them?",
      "Note: when FALSE, refuses to overwrite even empty directories"
    )
  ),
  optparse::make_option("--n_cores",
    default = Sys.getenv("NCPUS", 1L),
    help = "number of CPUs to use in parallel",
  ),
  optparse::make_option("--parallel_strategy",
    default = "multisession",
    help = "Strategy for parallel conversion, passed to future::plan()",
  )
) |>
  # Show default values in help message
  purrr::modify(\(x) {
    x@help <- paste(x@help, "[default: %default]")
    x
  })

args <- optparse::OptionParser(option_list = options) |>
  optparse::parse_args()

## end options


future::plan(args$parallel_strategy, workers = as.numeric(args$n_cores))

site_info <-
  read.csv(
    args$site_info_path,
    colClasses = c(id = "character")
  ) |>
  dplyr::mutate(outdir = file.path(args$out_dir, id)) |>
  dplyr::select(lat, lon, outdir)
if (!args$overwrite) {
  site_info <- site_info |>
    dplyr::filter(!dir.exists(outdir))
}
site_info |>
  furrr::future_pwalk(
    .f = PEcAn.data.land::extract_soil_gssurgo,
    size = args$n_ens,
    grid_size = 5,
    .options = furrr::furrr_options(seed = TRUE)
  )
