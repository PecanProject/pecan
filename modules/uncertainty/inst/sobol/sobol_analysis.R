#!/usr/bin/env Rscript

# Sobol global sensitivity analysis through the standard PEcAn workflow.
#
#   Rscript sobol_analysis.R -s settings.xml
#
# settings$ensemble$size is the Sobol base sample N. The design has k columns,
# one for the whole parameter set plus one per input in <samplingspace>, and
# this writes N * (k + 2) runs per site. The design is saved to outdir so
# indices can be computed from a later session once the runs finish:
#   sobol_design <- readRDS(file.path(settings$outdir, "sobol_design.rds"))

args <- PEcAn.settings::get_args()
settings <- PEcAn.settings::read.settings(args$settings)
settings <- PEcAn.settings::prepare.settings(settings, force = FALSE)

# one design shared by every site, so all sites run the same parameter and
# input indices
first <- if (PEcAn.settings::is.MultiSettings(settings)) settings[[1]] else settings
sobol_design <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings = first,
  ensemble_size = first$ensemble$size,
  sobol = TRUE
)
saveRDS(sobol_design, file.path(settings$outdir, "sobol_design.rds"))

settings <- PEcAn.workflow::runModule.run.write.configs(settings, input_design = sobol_design)
PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
PEcAn.workflow::runModule_start_model_runs(settings)
PEcAn.uncertainty::runModule.get.results(settings)

sites <- if (PEcAn.settings::is.MultiSettings(settings)) settings else list(settings)
indices <- list()
for (s in sites) {
  variables <- unlist(s$ensemble[names(s$ensemble) == "variable"])
  for (v in variables) {
    fname <- PEcAn.uncertainty::ensemble.filename(
      s, "ensemble.output", "Rdata",
      all.var.yr = FALSE,
      variable = PEcAn.utils::convert.expr(v)$variable.drv
    )
    told <- PEcAn.uncertainty::compute_sobol_indices(fname, sobol_design)
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
