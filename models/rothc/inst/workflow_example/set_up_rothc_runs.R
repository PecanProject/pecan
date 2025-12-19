#!/usr/bin/env Rscript

# --------------------------------------------------
# Run-time parameters

options <- list(
  optparse::make_option(c("-s", "--settings"),
    default = "settings.xml",
    help = paste(
      "path to the XML settings file you want to use for this run.",
      "Be aware all paths inside the file are interpreted relative to the",
      "working directory of the process that invokes run_model.R,",
      "not relative to the settings file path"
    )
  )
  # TODO: This would be a natural place to specify output directories,
  # but it was already done in the XML build step.
  # Consider moving that here with appropriate runtime option(s).
  # optparse::make_option(c("-d", "--output_dir"),
  #   default = "output",
  #   # ...
  # ),
  # optparse::make_option(c("-o", "--settings_out"),
  #   default = "output/pecan.CONFIG.xml",
  #   # ...
  # ),
) |>
  # Show default values in help message
  purrr::modify(\(x) {
    x@help <- paste(x@help, "[default: %default]")
    x
  })

args <- optparse::OptionParser(option_list = options) |>
  optparse::parse_args()


# Put global environment into "PEcAn mode"
options(warn = 1)
options(error = quote({
  try(PEcAn.utils::status.end("ERROR"))
  try(PEcAn.remote::kill.tunnel(settings))
  if (!interactive()) {
    q(status = 1)
  }
}))

library("PEcAn.all")
PEcAn.all::pecan_version()


# Open and read in settings file for PEcAn run.
settings <- PEcAn.settings::read.settings(args$settings)

if (dir.exists(settings$outdir)) {
  PEcAn.logger::logger.severe(
    "outdir", sQuote(settings$outdir), "already exists.",
    "If you want to replace it, please delete and rerun."
  )
}
dir.create(settings$outdir, recursive = TRUE)
status_file <- file.path(settings$outdir, "STATUS")

# Write model specific configs
PEcAn.utils::status.start("CONFIG")
settings <- PEcAn.workflow::runModule.run.write.configs(settings)
PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
PEcAn.utils::status.end()
