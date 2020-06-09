source("auth.R")
source("ping.R")

root <- plumber::plumber$new()
root$setSerializer(plumber::serializer_unboxed_json())

root$filter("require-auth", authenticate_user)

root$handle("GET", "/api/ping", ping)

models_pr <- plumber::plumber$new("models.R")
root$mount("/api/models", models_pr)

workflows_pr <- plumber::plumber$new("workflows.R")
root$mount("/api/workflows", workflows_pr)

root$run(host="0.0.0.0", port=8000, debug=TRUE, swagger = function(pr, spec, ...) {
  spec <- yaml::read_yaml("pecanapi-spec.yml")
  spec
})


