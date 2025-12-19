#!/usr/bin/env Rscript


options <- list(
  optparse::make_option(c("-s", "--settings"),
    default = "output/pecan.CONFIGS.xml",
    help = paste(
      "path to the XML settings file you want to use for this run.",
      "Be aware all paths inside the file are interpreted relative to the",
      "working directory of the process that invokes run_model.R,",
      "not relative to the settings file path"
    )
  ),
  optparse::make_option(c("-n", "--n_cores"),
    default = Sys.getenv("NCPUS", 1),
    help = "number of CPUs to use in parallel"
  )
) |>
  # Show default values in help message
  purrr::modify(\(x) {
    x@help <- paste(x@help, "[default: %default]")
    x
  })

args <- optparse::OptionParser(option_list = options) |>
  optparse::parse_args()


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

# load settings
settings <- PEcAn.settings::read.settings(args$settings)

# run RothC
# NB assumes write.config step has already happened
# (i.e. settings should look like `output/pecan.CONFIGS.xml`)
PEcAn.utils::status.start("MODEL")
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = FALSE)
PEcAn.utils::status.end()

# extract model results for ensemble analysis
# this function is arguably too chatty, so we'll suppress
# INFO-level log output for this step.
loglevel <- PEcAn.logger::logger.setLevel("WARN")
PEcAn.utils::status.start("OUTPUT")
runModule.get.results(settings)
PEcAn.utils::status.end()
PEcAn.logger::logger.setLevel(loglevel)

# Summarize ensemble timeseries + end of run distributions
PEcAn.utils::status.start("ENSEMBLE")
runModule.run.ensemble.analysis(settings, TRUE)
PEcAn.utils::status.end()

# Done
PEcAn.utils::status.start("FINISHED")
PEcAn.remote::kill.tunnel(settings)
PEcAn.utils::status.end()
