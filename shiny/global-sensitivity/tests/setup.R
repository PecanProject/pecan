library(PEcAn.visualization)
source("plotEnsemble.R")
source("load_ensemble.R")

bety <- betyConnect("../../web/config.php")
workflow_id <- 1000001488
run_ids <- get_run_ids(bety, workflow_id)
var_names <- get_var_names(bety, workflow_id, run_ids[1])
current_workflow <- collect(workflow(bety, workflow_id))

workflow_dir <- current_workflow$folder
load(file.path(workflow_dir, "samples.Rdata"))
pft.names <- names(ensemble.samples) %>% .[. != "env"]
param_names <- paste0(pft.names, ".", colnames(ensemble.samples[[1]]))

workflow <- current_workflow
workflow_dir <- workflow$folder
output_dir <- file.path(workflow_dir, "out")
settings <- XML::xmlToList(XML::xmlParse(file.path(workflow_dir, "pecan.xml")))
