#!/usr/bin/env Rscript

library(PEcAn.settings)

# Construct one multisite PEcAn XML file for PEPRMT

## Config section -- edit for your project
options <- list(
  optparse::make_option("--n_ens",
    default = 20,
    help = "number of ensemble simulations per site"
  ),
  optparse::make_option("--n_met",
    default = 10,
    help = "number of met files available (ensemble will sample from all)"
  ),
  optparse::make_option("--start_date",
    default = "2016-01-01",
    help = paste(
      "Date to begin simulations.",
      "Ensure your IC files are valid for this date"
    )
  ),
  optparse::make_option("--end_date",
    default = "2024-12-31",
    help = "Date to end simulations"
  ),
  optparse::make_option("--met_dir",
    default = "data/ERA5_PEPRMT",
    help = paste(
      "Directory containing climate data.",
      "Should contain subdirs named by site id"
    )
  ),
  optparse::make_option("--soil_dir",
    default = "data/soil",
    help = paste(
      "Directory containing netCDFs of soil data.",
      "Should contain subdirs named by site id"
    )
  ),
  optparse::make_option("--site_file",
    default = "data/site_info.csv",
    help = paste(
      "CSV file containing one row for each site to be simulated.",
      "Must contain at least columns `id`, `lat`, `lon`, and `site.pft`"
    )
  ),
  optparse::make_option("--template_file",
    default = "template.xml",
    help = paste(
      "XML file containing whole-run settings,",
      "Will be expanded to contain all sites at requested ensemble size"
    )
  ),
  optparse::make_option("--output_file",
    default = "settings.xml",
    help = "path to write output XML"
  ),
  optparse::make_option("--output_dir_name",
    default = "output",
    help = paste(
      "Path the settings should declare as output directory.",
      "This will be inserted replacing [out] in all of the following places:",
      "`outdir` = [out] ; `modeloutdir` = [out]/out; `rundir` = [out]/run;",
      "`host$outdir`: [out]/out; `host$rundir`: [out]/run."
    )
  )
) |>
  # Show default values in help message
  purrr::modify(\(x) {
    x@help <- paste(x@help, "[default: %default]")
    x
  })

args <- optparse::OptionParser(option_list = options) |>
  optparse::parse_args()


## End config section
## Whew, that was a lot of lines to define a few defaults!



site_info <- read.csv(args$site_file)
stopifnot(
  length(unique(site_info$id)) == nrow(site_info),
  all(site_info$lat > 0), # just to simplify grid naming below
  all(site_info$lon < 0)
)
site_info <- site_info |>
  dplyr::mutate(
    # match locations to half-degree ERA5 grid cell centers
    # CAUTION: Calculation only correct when all lats are N and all lons are W!
    ERA5_grid_cell = paste0(
      ((lat + 0.25) %/% 0.5) * 0.5, "N_",
      ((abs(lon) + 0.25) %/% 0.5) * 0.5, "W"
    ),
    # Hack: prepare.settings wants every site to have a name as well as an ID.
    # It's probably never used downstream, but add here to quiet the check.
    # TODO remove the upstream rule
    name = id
  )

settings <- read.settings(args$template_file) |>
  setDates(args$start_date, args$end_date)
settings$info$notes <- paste("Compiled from", args$template_file,
                             "at", Sys.time())

settings$ensemble$size <- args$n_ens
# settings$run$inputs$poolinitcond$ensemble <- args$n_ens

# Hack: setEnsemblePaths leaves all path components other than siteid
# identical across sites.
# To use site-specific grid id, I'll string-replace each siteid
id2grid <- function(s) {
  # replacing in place to preserve names (easier than thinking)
  for (p in seq_along(s$run$inputs$met$path)) {
    s$run$inputs$met$path[[p]] <- gsub(
      pattern = s$run$site$id,
      replacement = s$run$site$ERA5_grid_cell,
      x = s$run$inputs$met$path[[p]]
    )
  }
  s
}

add_soil_pft <- function(s) {
  s$run$site$site.pft <- list(veg = s$run$site$site.pft, soil = "soil")
  s
}

settings <- settings |>
  createMultiSiteSettings(site_info) |>
  setEnsemblePaths(
    n_reps = args$n_met,
    input_type = "met",
    path = args$met_dir,
    d1 = args$start_date |> as.Date() |> format("%Y-%m"),
    d2 = args$end_date |> as.Date() |> format("%Y-%m"),
    path_template = "{path}/ERA5_{id}/ERA5.{n}.{d1}.{d2}.dat"
  ) |>
  papply(id2grid) |>
  # setEnsemblePaths(
  #   n_reps = args$n_ens,
  #   input_type = "poolinitcond",
  #   path = args$ic_dir,
  #   path_template = "{path}/{id}/IC_site_{id}_{n}.nc"
  # ) |>
  setEnsemblePaths(
    n_reps = args$n_ens,
    input_type = "soil_physics",
    path = args$soil_dir,
    # n+1 bc current implementation of extract_soil_gSSURGO saves a
    # gSSURGO_soil_1.nc containing pseudo-layers from _every_
    # soil type found at site, then at least n more files after that
    # ("at least" bc it also writes at least 1 file per soil type)
    # TODO
    path_template = "{path}/{id}/gSSURGO_soil_{n+1}.nc"
  ) |>
  papply(add_soil_pft)

# Update just the first component of the output directory,
# in all four places it's used.
# Note: It feels a bit odd to directly replace the word "output"
# rather than fill a blank or use a @placeholder@, but since existing template
# already passes @placeholder@'s on to be processed in PEcAn I didn't want
# to introduce confusion by making some be replaced at a different stage.
settings$outdir <- sub("^output", args$output_dir_name,
                       settings$outdir)
settings$modeloutdir <- sub("^output", args$output_dir_name,
                            settings$modeloutdir)
settings$rundir <- sub("^output", args$output_dir_name,
                       settings$rundir)
settings$host$outdir <- sub("^output", args$output_dir_name,
                            settings$host$outdir)
settings$host$rundir <- sub("^output", args$output_dir_name,
                            settings$host$rundir)

write.settings(
  settings,
  outputfile = basename(args$output_file),
  outputdir = dirname(args$output_file)
)
