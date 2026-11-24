make_segmented_sobol_case <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  met <- file.path(directory, "met.clim")
  events <- file.path(directory, "events.in")
  crops <- file.path(directory, "crops.csv")
  writeLines("2025 1 irrig 0 1", events)
  writeLines(c("date,crop_code", "2025-01-02,D12"), crops)
  climate <- data.frame(year = 2025, day = rep(1:10, each = 4),
    hour = rep(c(0, 6, 12, 18), 10), step = 0.25)
  climate[paste0("c", 5:12)] <- NA_real_
  write.table(climate, met, quote = FALSE, row.names = FALSE, col.names = FALSE)
  settings <- PEcAn.settings::as.Settings(list(
    outdir = directory, rundir = file.path(directory, "run"),
    modeloutdir = file.path(directory, "out"),
    host = list(name = "localhost", rundir = file.path(directory, "run"),
                outdir = file.path(directory, "out")),
    model = list(type = "SIPNET", binary = "", revision = "2.1.0"),
    pfts = list(list(name = "pft1"), list(name = "soil")),
    ensemble = list(size = 2, samplingspace = list(
      parameters = list(method = "uniform"), met = list(method = "sampling"))),
    run = list(site = list(id = "a", lat = 40, lon = -88,
                          site.pft = list(veg = "pft1", soil = "soil")),
      start.date = "2025-01-01", end.date = "2025-01-10",
      inputs = list(met = list(path = c(met, met)), events = list(path = events),
                    crop_changes = list(path = crops)))))
  samples <- list(
    trait.samples = list(pft1 = list(SLA = 101:104)),
    ensemble.samples = list(pft1 = data.frame(Amax = rep(1, 4), SLA = c(10, 20, 30, 40)),
                            soil = data.frame(Rd = rep(0, 4))),
    sa.samples = list(), runs.samples = list(), env.samples = list())
  design <- PEcAn.uncertainty::generate_joint_ensemble_design(
    settings, ensemble_size = 2, samples = samples, sobol = TRUE)
  design$X$param <- design$design_matrix$param <- c(4L, 2L, 4L, 1L, 3L, 1L, 2L, 3L)
  settings <- PEcAn.workflow::runModule.run.write.configs(
    settings, input_design = design, overwrite = FALSE)
  manifest <- read.csv(file.path(directory, "runs_manifest.csv"))
  run_row <- manifest[1, ]
  run_row$param <- design$design_matrix$param[1]
  run_row$met <- design$design_matrix$met[1]
  list(settings = settings, design = design, run_row = run_row, samples = samples)
}

read_sipnet_test_parameters <- function(path) {
  params <- read.table(path, stringsAsFactors = FALSE)
  stats::setNames(params[[2]], params[[1]])
}
