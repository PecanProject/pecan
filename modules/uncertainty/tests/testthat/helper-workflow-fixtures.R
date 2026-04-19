# Mock PFT settings list
# Mirrors the `pft` list element used throughout the pipeline
# (get.trait.data.pft -> run.meta.analysis.pft -> get.parameter.samples).
make_mock_pft <- function(name = "temperate.Hardwood",
                          outdir = tempfile("pft_"),
                          posteriorid = 9999L) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  list(
    name        = name,
    outdir      = outdir,
    posteriorid = posteriorid,
    constants   = list()
  )
}

# Mock trait data (as saved in trait.data.Rdata)
# A named list of data frames, one per trait.
make_mock_trait_data <- function(traits = c("SLA", "Vcmax"),
                                 n_obs = 10,
                                 seed = 42) {
  set.seed(seed)
  result <- list()
  for (trait in traits) {
    result[[trait]] <- data.frame(
      mean         = rnorm(n_obs, mean = ifelse(trait == "SLA", 20, 40), sd = 3),
      stat         = rep(1.5, n_obs),
      n            = rep(5L, n_obs),
      statname     = rep("SD", n_obs),
      site_id      = seq_len(n_obs),
      greenhouse   = rep(FALSE, n_obs),
      name         = rep(trait, n_obs),
      treatment_id = rep(1L, n_obs),
      control      = rep(1L, n_obs),
      specie_id    = rep(1L, n_obs),
      citation_id  = rep(1L, n_obs),
      cultivar_id  = rep(NA_integer_, n_obs),
      date         = rep(NA_character_, n_obs),
      time         = rep(NA_character_, n_obs),
      stringsAsFactors = FALSE
    )
  }
  result
}

# Mock prior distributions (as saved in prior.distns.Rdata)
make_mock_prior_distns <- function(traits = c("SLA", "Vcmax")) {
  data.frame(
    distn  = rep("norm", length(traits)),
    parama = c(20, 40)[seq_along(traits)],
    paramb = c(5, 10)[seq_along(traits)],
    n      = c(50L, 30L)[seq_along(traits)],
    row.names = traits,
    stringsAsFactors = FALSE
  )
}

# Mock MCMC results (as saved in trait.mcmc.Rdata)
# Returns a named list of `coda::mcmc.list` objects (one per trait),
# mimicking output from `pecan.ma()`.
make_mock_trait_mcmc <- function(traits = c("SLA"),
                                 n_samples = 200,
                                 seed = 42) {
  set.seed(seed)
  result <- list()
  for (trait in traits) {
    chain <- matrix(
      rnorm(n_samples, mean = ifelse(trait == "SLA", 20, 40), sd = 2),
      ncol = 1
    )
    colnames(chain) <- "beta.o"
    result[[trait]] <- coda::mcmc.list(coda::mcmc(chain))
  }
  result
}

# Mock posterior distributions (as saved in post.distns.Rdata)
make_mock_post_distns <- function(traits = c("SLA", "Vcmax")) {
  data.frame(
    distn  = rep("norm", length(traits)),
    parama = c(20.5, 39.8)[seq_along(traits)],
    paramb = c(2.1, 4.5)[seq_along(traits)],
    n      = c(50L, 30L)[seq_along(traits)],
    row.names = traits,
    stringsAsFactors = FALSE
  )
}

# Write a full set of trait-pipeline fixtures to disk
# Saves the .Rdata files that run.meta.analysis.pft() would normally
# produce, so downstream steps (get.parameter.samples) can be tested
# without running the actual meta-analysis.
write_trait_pipeline_fixtures <- function(outdir,
                                          traits = c("SLA", "Vcmax"),
                                          n_obs = 10,
                                          seed = 42) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  trait.data <- make_mock_trait_data(traits, n_obs = n_obs, seed = seed)
  save(trait.data, file = file.path(outdir, "trait.data.Rdata"))

  prior.distns <- make_mock_prior_distns(traits)
  save(prior.distns, file = file.path(outdir, "prior.distns.Rdata"))

  trait.mcmc <- make_mock_trait_mcmc(traits, seed = seed)
  save(trait.mcmc, file = file.path(outdir, "trait.mcmc.Rdata"))

  post.distns <- make_mock_post_distns(traits)
  save(post.distns, file = file.path(outdir, "post.distns.Rdata"))
  save(post.distns, file = file.path(outdir, "post.distns.MA.Rdata"))
  file.symlink(
    file.path(outdir, "post.distns.MA.Rdata"),
    file.path(outdir, "post.distns.link.check")
  )

  invisible(outdir)
}
