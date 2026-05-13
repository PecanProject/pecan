#!/usr/bin/env Rscript
#
# Type-level Sobol GSA. Reads any PEcAn settings XML (single site or
# MultiSettings), runs CONFIG/MODEL/OUTPUT workflow with a
# Saltelli A/B/AB design over the input categories declared in
# <ensemble><samplingspace>, then writes per-site, per-variable first- and
# total-order Sobol indices to one CSV next to the model outputs.
#
# usage:
#   Rscript sobol_analysis.R <settings.xml> [N]
# where N is the Sobol base sample size (default: settings$ensemble$size).

suppressPackageStartupMessages({
  library(PEcAn.all)
  library(PEcAn.logger)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  PEcAn.logger::logger.severe(
    "usage: Rscript sobol_analysis.R <settings.xml> [N]"
  )
}
xml_path <- args[[1]]

settings <- PEcAn.settings::read.settings(xml_path)
is_multi <- PEcAn.settings::is.MultiSettings(settings)

# pull sites and variables straight from the parsed XML, no special casing
sites <- if (is_multi) {
  purrr::map_chr(settings, \(s) s$run$site$id)
} else {
  settings$run$site$id
}
# <ensemble><variable> tags repeat with same name; settings$ensemble$variable
# would only return the first one, so pull every "variable" child instead
ens <- if (is_multi) settings[[1]]$ensemble else settings$ensemble
variables <- unlist(ens[names(ens) == "variable"], use.names = FALSE)

N <- if (length(args) >= 2) {
  as.integer(args[[2]])
} else if (is_multi) {
  as.integer(settings[[1]]$ensemble$size)
} else {
  as.integer(settings$ensemble$size)
}

PEcAn.logger::logger.info(
  "Sobol GSA --", length(sites), "site(s),",
  length(variables), "variable(s), N =", N
)

# build one shared design from site #1 and reuse across all sites
# (one input_design per run.write.configs call by design)
design_src <- if (is_multi) settings[[1]] else settings
sobol_obj <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings      = design_src,
  ensemble_size = N,
  sobol         = TRUE
)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings, input_design = sobol_obj$X
)
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = FALSE)
PEcAn.workflow::runModule.get.results(settings)

# per-site, per-variable indices; ensemble_id picks right file in
# multisite outdir without any staging or symlinks
results <- list()
for (i in seq_along(sites)) {
  per_site <- if (is_multi) settings[[i]] else settings
  eid <- per_site$ensemble$ensemble.id
  for (v in variables) {
    res <- PEcAn.uncertainty::compute_sobol_indices(
      outdir      = settings$outdir,
      sobol_obj   = sobol_obj,
      var         = v,
      ensemble_id = eid,
      boot        = TRUE,
      R           = 500L
    )
    res$site     <- sites[[i]]
    res$variable <- v
    results[[paste(sites[[i]], v, sep = "_")]] <- res
  }
}

final   <- dplyr::bind_rows(results)
out_csv <- file.path(settings$outdir, "sobol_indices.csv")
readr::write_csv(final, out_csv)
PEcAn.logger::logger.info("wrote", nrow(final), "rows to", out_csv)
