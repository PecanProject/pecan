pkg_path <- "~/Projects/pecan/pecan/modules/rtm"
devtools::document(pkg_path)
devtools::install(pkg_path)
library(PEcAnRTM)

params <- c('N' = 1.4, 
            'Cab' = 40,
            'Car' = 8,
            'Cw' = 0.01,
            'Cm' = 0.01)
sensor <- "identity"
data(sensor.rsr)
generate_obs <- function(i){
  obs.raw <- prospect(params, 5)[,1] + generate.noise()
  obs <- spectral.response(obs.raw, sensor)
  return(obs)
}

n_obs <- 3
obs <- do.call(cbind, lapply(seq_len(n_obs), generate_obs))

invert.options <- default.settings.prospect
invert.options$inits <- invert.options$inits.function()
invert.options$model <- function(params) {
  spectral.response(prospect(params,5)[,1], sensor)
}
invert.options$ngibbs.min <- 5000
invert.options$ngibbs.step <- 2000
invert.options$ngibbs.max <- 100000
invert.options$do.lsq <- TRUE
invert.options$nchains <- 3

options(warn=1)

test.parallel <- invert.auto(obs, invert.options, parallel = TRUE)
print_results_summary(test.parallel)

plot(PEcAn.assim.batch::makeMCMCList(test.parallel$n_eff_list))
plot(PEcAn.assim.batch::makeMCMCList(test.parallel$deviance_list))

#test.serial <- invert.custom(obs, invert.options)

#sp <- summaryRprof()
#head(sp$by.self, 15)
#head(sp$by.total, 20)
