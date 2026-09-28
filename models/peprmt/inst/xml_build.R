#!/usr/bin/env Rscript

library(PEcAn.settings)

# Construct one multisite PEcAn XML file for PEPRMT

## Config section -- edit for your project
options <- list(
  optparse::make_option("--n_ens",
    default = 3,
    help = "number of ensemble simulations per site"
  ),
  optparse::make_option("--n_met",
    default = 10,
    help = "number of met files available (ensemble will sample from all)"
  ),
  optparse::make_option(
    "--start_date",
    default = "2016-01-01", 
    help = paste(
      "Date to begin simulations.",
      "Ensure your IC files are valid for this date"
    )
  ),
  optparse::make_option(
    "--end_date",
    default = "2024-12-31",
    help = "Date to end simulations"
  ),
  optparse::make_option("--met_dir",
    default = "data/met",
    help = paste(
      "Directory containing climate data.",
      "Should contain subdirs named by site id"
    )
  ),
  optparse::make_option("--peprmt_input_dir",
    default = "data/PEPRMT_specific_inputs",
    help = paste(
      "Directory containing csvs of PEPRMT-specific input data.",
      "Includes salinity, nitrate, water level"
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


# Load site info
site_info <- read.csv(args$site_file)

# Ensure each site has one row
stopifnot(
  length(unique(site_info$id)) == nrow(site_info)
)

# Parse lat lon
site_info <- site_info |>
  dplyr::mutate(
    # match locations to half-degree ERA5 grid cell centers
    ERA5_grid_cell = paste0(
      ((lat + 0.25) %/% 0.5) * 0.5, ifelse(lat > 0, "N_", "S_"),
      ((abs(lon) + 0.25) %/% 0.5) * 0.5, ifelse(lon > 0, "E", "W")
    ),
    # Hack: prepare.settings wants every site to have a name as well as an ID.
    # It's probably never used downstream, but add here to quiet the check.
    # TODO remove the upstream rule
    name = id
  )

# Load settings and set specifications
settings_init <- read.settings(args$template_file) |>
  setDates(args$start_date, args$end_date)

settings_init$info$notes <- paste("Compiled from", args$template_file,
                                  "at", Sys.time())

settings_init$ensemble$size <- args$n_ens

settings <- settings_init |>
  
  # Set where demo outputs go
  # setOutDir("output") |>
  
  # Set ensemble dates
  # Note the run dates differ by site (overridden below);
  # for ensemble we take a range that includes all.
  setDates(min(site_info$met.start), max(site_info$met.end)) |>
  
  # Takes all sites listed in site_info.csv,
  # adds empty path templates to each `run$site.[siteid]`
  createMultiSiteSettings(site_info) |>
  
  # Set run start and end dates to match available met data
  # (as hard-coded in site-info.csv -- consider getting from elsewhere?)
  # If dates were equal for all sites, setDates() above this would handle this
  papply(function(s) {
    s$run$start.date <- s$run$site$met.start
    s$run$end.date <- s$run$site$met.end
    s
  })  |>
  # Declare naming scheme for met files.
  # NB this creates paths in the XML that don't exist on disk yet --
  # we'll create them in a separate operation below
  setEnsemblePaths(
    n_reps = ensemble_size,
    input_type = "met",
    path = file.path("data", "met"),
    d1 = "DATES-HERE",
    path_template = "{path}/ERA5_{id}/ERA5.{n}.{d1}.dat"
  ) |>
  setEnsemblePaths(
    n_reps = 1,
    input_type = "PEPRMT",
    path = file.path("data", "PEPRMT_specific_inputs"),
    path_template = "{path}/{id}_formatted.csv"
  ) |>
  # helper functions from peprmt/R/helpers.R to address having multiple grid 
  # cells and date ranges in the same site_info file
  papply(id2grid) |> 
  papply(dates2grid)

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

# Export
write.settings(
  settings,
  outputfile = basename(args$output_file),
  outputdir = dirname(args$output_file)
)
