source("tests/setup.R")
ensemble.out <- load_ensemble(workflow_dir = workflow_dir, settings = settings, variable = var_names)
plotEnsemble(ensemble.out, param_names[1], var_names[1])
