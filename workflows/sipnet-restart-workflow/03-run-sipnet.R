#!/usr/bin/env Rscript

if (FALSE) {
  devtools::install("models/sipnet", upgrade = FALSE)
  devtools::install("modules/data.land", upgrade = FALSE)
}

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

settings_raw <- PEcAn.settings::read.settings(file.path(config[["outdir_root"]], "settings.xml"))

sens_design <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings_raw,
  settings_raw$ensemble$size
)
write.csv(sens_design$X, file.path(settings_raw$outdir, "input_design.csv"))

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings_raw,
  input_design = sens_design$X
)

source("workflows/sipnet-restart-workflow/utils.R")
jobfiles <- write_segmented_configs.SIPNET(settings, sens_design$X)
# Note: If running a multi-site workflow, use:
# jobfiles <- papply(settings, \(s) write_segmented_configs.SIPNET(s, sens_design$X))

PEcAn.workflow::runModule_start_model_runs(settings)
