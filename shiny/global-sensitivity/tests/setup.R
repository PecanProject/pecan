source("helper.R")
source("plotEnsemble.R")
source("load_ensemble.R")

bety <- betyConnect()
workflow_id <- 1000001488    # Working default
run_ids <- get_run_ids(bety, workflow_id)
var_names <- get_var_names(bety, workflow_id, run_ids[1])
var_names <- var_names[!grepl("pool", var_names, ignore.case = TRUE)]  ## Ignore "poolnames" and "carbon pools" variables
current_workflow <- collect(workflow(bety, workflow_id))

workflow_dir <- current_workflow$folder
load(file.path(workflow_dir, "samples.Rdata"))
pft.names <- names(ensemble.samples) %>% .[. != "env"]
param_names <- paste0(pft.names, ".", colnames(ensemble.samples[[1]]))

workflow <- current_workflow
if(nrow(workflow) > 0) {
    workflow_dir <- workflow$folder
    output_dir <- file.path(workflow_dir, "out")
    settings <- xmlToList(xmlParse(file.path(workflow_dir, "pecan.xml")))
    # Load ensemble samples
    ensemble.out <- load_ensemble(workflow_dir = workflow_dir, settings = settings, variable = var_names)
}

