#!/usr/bin/env Rscript

#* This is the entry point to the PEcAn API. 
#* All API endpoints (& filters) are mounted here
#* @author Tezan Sahu

source("auth.R")
source("general.R")

# Set up the global database pool
.bety_params <- PEcAn.DB::get_postgres_envvars(
  host = "localhost",
  dbname = "bety",
  user = "bety",
  password = "bety",
  driver = "Postgres"
)

.bety_params$driver <- NULL
.bety_params$drv <- RPostgres::Postgres()
global_db_pool <- do.call(pool::dbPool, .bety_params)

root <- plumber::Plumber$new()
root$setSerializer(plumber::serializer_unboxed_json())

# Filter for authenticating users trying to hit the API endpoints
root$filter("require-auth", authenticate_user)

# The /api/ping & /api/status are standalone API endpoints 
# implemented using handle() because of restrictions of plumber
# to mount multiple endpoints on the same path (or subpath)
root$handle("GET", "/api/ping", ping)
root$handle("GET", "/api/status", status)

# The endpoints mounted here are related to details of PEcAn models
models_pr <- plumber::Plumber$new("models.R")
root$mount("/api/models", models_pr)

# The endpoints mounted here are related to details of PEcAn sites
sites_pr <- plumber::Plumber$new("sites.R")
root$mount("/api/sites", sites_pr)

# The endpoints mounted here are related to details of PEcAn pfts
pfts_pr <- plumber::Plumber$new("pfts.R")
root$mount("/api/pfts", pfts_pr)

# The endpoints mounted here are related to details of PEcAn formats
formats_pr <- plumber::Plumber$new("formats.R")
root$mount("/api/formats", formats_pr)

# The endpoints mounted here are related to details of PEcAn inputs
inputs_pr <- plumber::Plumber$new("inputs.R")
root$mount("/api/inputs", inputs_pr)

# The endpoints mounted here are related to details of PEcAn workflows
workflows_pr <- plumber::Plumber$new("workflows.R")
root$mount("/api/workflows", workflows_pr)

# The endpoints mounted here are related to details of PEcAn runs
runs_pr <- plumber::Plumber$new("runs.R")
root$mount("/api/runs", runs_pr)

# Available models
runs_pr <- plumber::Plumber$new("available-models.R")
root$mount("/api/availableModels", runs_pr)

# The API server is bound to 0.0.0.0 on port 8000
# The Swagger UI for the API draws its source from the pecanapi-spec.yml file
root$run(host="0.0.0.0", port=8000, debug=TRUE, swagger = function(pr, spec, ...) {
  spec <- yaml::read_yaml("../pecanapi-spec.yml")
  spec
})
