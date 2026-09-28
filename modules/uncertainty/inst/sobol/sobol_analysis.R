#!/usr/bin/env Rscript

# Sobol global sensitivity analysis through the standard PEcAn workflow.
#
#   Rscript sobol_analysis.R <settings.xml>
#
# settings$ensemble$size is the Sobol base sample N. With k sampled inputs
# (parameters plus each input in <samplingspace>) this writes N * (k + 2) runs
# per site. The design is saved to outdir so indices can be computed from a
# later session once the runs finish:
#   sobol_obj <- readRDS(file.path(settings$outdir, "sobol_design.rds"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  PEcAn.logger::logger.severe("usage: Rscript sobol_analysis.R <settings.xml>")
}

settings <- PEcAn.settings::read.settings(args[[1]])
settings <- PEcAn.settings::prepare.settings(settings, force = FALSE)
dir.create(settings$outdir, recursive = TRUE, showWarnings = FALSE)

# one design shared by every site, so all sites run the same parameter and
# input indices
first <- if (PEcAn.settings::is.MultiSettings(settings)) settings[1] else settings
sobol_obj <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings = first,
  ensemble_size = settings$ensemble$size,
  sobol = TRUE
)
saveRDS(sobol_obj, file.path(settings$outdir, "sobol_design.rds"))

settings <- PEcAn.workflow::runModule.run.write.configs(settings, input_design = sobol_obj)
PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
stop_on_error <- as.logical(settings[[c("run", "stop_on_error")]])
if (length(stop_on_error) == 0) stop_on_error <- FALSE
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
PEcAn.uncertainty::runModule.get.results(settings)

sites <- if (PEcAn.settings::is.MultiSettings(settings)) settings else list(settings)
indices <- list()
for (s in sites) {
  variables <- unlist(s$ensemble[names(s$ensemble) == "variable"])
  for (v in variables) {
    told <- PEcAn.uncertainty::compute_sobol_indices(s, sobol_obj, v)
    indices[[length(indices) + 1]] <- data.frame(
      site_id = s$run$site$id,
      variable = v,
      factor = rownames(told$S),
      first_order = told$S[, "original"],
      total_order = told$T[, "original"]
    )
  }
}
utils::write.csv(do.call(rbind, indices),
                 file.path(settings$outdir, "sobol_indices.csv"),
                 row.names = FALSE)
