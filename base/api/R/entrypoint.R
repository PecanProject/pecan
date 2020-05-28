root <- plumber::plumber$new()
root$handle("GET", "/ping", pecanapi::ping)

root$run(port=8000, swagger = function(pr, spec, ...) {
  spec <- yaml::read_yaml("inst/pecan-api-spec.yml")
  spec
})
