source("tests/setup.R")
ensemble.out <- load_ensemble(workflow_dir = workflow_dir, settings = settings, variable = var_names)

# Plot one ensemble member
plotEnsemble(ensemble.out, param_names[1], var_names[1])

# Plot all parameters for a given variable
variable <- var_names[1]
plotAllParams(ensemble.out, variable, param_names)

# Plot all variables for a given parameter
param <- param_names[1]
plotAllVars(ensemble.out, param, var_names)
