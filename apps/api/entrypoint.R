library("plumber")

source("auth.R")
source("ping.R")

root <- plumber$new()
root$setSerializer(serializer_unboxed_json())

root$filter("require-auth", authenticate_user)

root$handle("GET", "/api/ping", ping)

models_pr <- plumber$new("models.R")
root$mount("/api/models", models_pr)

root$run(host="0.0.0.0", port=8000, swagger = function(pr, spec, ...) {
  spec <- yaml::read_yaml("pecanapi-spec.yml")
  spec
})


