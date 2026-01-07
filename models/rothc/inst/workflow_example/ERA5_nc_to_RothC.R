#!/usr/bin/env Rscript

# Converts a directory full of gridded ERA5 meteorology data
# from PEcAn's standard netCDF format to RothC weather statistics.

# This is basically a thin wrapper around `met2model.RothC()`,
# and is specific to ERA5 only by its assumptions about the filenames:
# It assumes inputs has a directory per location and ensemble member (1-10)
#   containing one file per year, e.g.
#   path/to/ERA5_<location>_<ens>/ERA5.<ens>.<year>.nc
# and produces output with one directory per location containing one multiyear
#   file per ensemble member, e.g.
#   path/to/ERA5_<location>/ERA5.<ens>.<startdate>.<enddate>.nc

# For a related script that takes a list of locations to be converted rather
# than operate on the entire input directory, see
# https://github.com/ccmmf/workflows/blob/main/2a_grass/01_ERA5_nc_to_clim.R


## --------- runtime values: change for your system and simulation ---------

options <- list(
  optparse::make_option("--site_era5_path",
    default = "data_raw/ERA5_CA_nc",
    help = paste(
      "Path to your existing ERA5 data in PEcAn CF format, organized as",
      "single-site, single-year netcdfs in subdirectories per ensemble member.",
      "Files should be named",
      "'<site_era5_path>/ERA5_<siteid>_<ensid>/ERA5.<ensid>.<year>.nc'"
    )
  ),
  optparse::make_option("--site_rothc_met_path",
    default = "data/ERA5_CA_RothC",
    help = paste(
      "Output path:",
      "single-site, multi-year weather summaries, one per ensemble member.",
      "Files will be named",
      "<site_rothc_met_path>/<siteid>/ERA5.<ensid>.<start>.<end>.dat"
    )
  ),
  optparse::make_option("--start_date",
    default = "2016-01-01",
    help = "Date to begin clim file",
  ),
  optparse::make_option("--end_date",
    default = "2024-12-31",
    help = "Date to end clim file",
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


# ----------- end system-specific ---------------------------------


future::plan(args$parallel_strategy, workers = as.numeric(args$n_cores))


dirs <- list.dirs(args$site_era5_path, recursive = FALSE, full.names = FALSE) |>
  data.frame(indir = _) |>
  dplyr::mutate(
    in_path = file.path(args$site_era5_path, indir),
    location = sub(r"(ERA5_(.*)_(\d+)$)", "\\1", indir),
    ens_num = sub(r"(ERA5_(.*)_(\d+)$)", "\\2", indir),
    in_prefix = paste0("ERA5.", ens_num),
    out_path = file.path(args$site_rothc_met_path, paste0("ERA5_", location))
  )

if (!dir.exists(args$site_rothc_met_path)) {
  dir.create(args$site_rothc_met_path, recursive = TRUE)
}

furrr::future_pwalk(
  dirs,
  function(in_path, in_prefix, out_path, ...) {
    PEcAn.RothC::met2model.RothC(
      in.path = in_path,
      start_date = args$start_date,
      end_date = args$end_date,
      in.prefix = in_prefix,
      outfolder = out_path
    )
  }
)
