# helper-test-data.R
# Shared test fixtures for meta-analysis pipeline tests
#
# These helpers create minimal but realistic test data structures
# that mirror what the actual pipeline produces and consumes.

# Create minimal trait data matching the structure expected by
# meta_analysis_standalone() and run.meta.analysis.pft()
#
# The columns here match the @param documentation in run.meta.analysis.pft.R:
#   name, mean, statname, stat, greenhouse, n,
#   site_id, specie_id, citation_id, cultivar_id,
#   date, time, control
create_test_trait_data <- function(n_obs = 10, trait_mean = 20, trait_sd = 2, seed = 42) {
  set.seed(seed)
  test_trait <- data.frame(
    citation_id  = rep(1L, n_obs),
    site_id      = rep(1:2, length.out = n_obs),
    name         = rep(paste0("species_", 1:2), length.out = n_obs),
    trt_id       = rep("control", n_obs),
    control      = rep(1L, n_obs),
    greenhouse   = rep(0L, n_obs),
    date         = rep(1, n_obs),
    time         = rep(NA, n_obs),
    cultivar_id  = rep(1L, n_obs),
    specie_id    = rep(1L, n_obs),
    n            = rep(5L, n_obs),
    mean = rnorm(n_obs, mean = trait_mean, sd = trait_sd),
    stat         = rep(trait_sd / sqrt(5), n_obs),
    statname     = rep("SE", n_obs),
    treatment_id = seq_len(n_obs),
    stringsAsFactors = FALSE
  )
  return(test_trait)
}

# Create a minimal prior distributions data.frame
# matching the structure returned by PEcAn.DB::query.priors()
#
# Uses normal distribution so that check_consistent() and
# p.point.in.prior() work without exotic distribution functions.
create_test_priors <- function(trait_names = "SLA",
                               distn = "norm",
                               parama = 20,
                               paramb = 5) {
  prior.distns <- data.frame(
    distn  = rep(distn, length(trait_names)),
    parama = rep(parama, length(trait_names)),
    paramb = rep(paramb, length(trait_names)),
    n      = rep(NA_real_, length(trait_names)),
    stringsAsFactors = FALSE
  )
  rownames(prior.distns) <- trait_names
  return(prior.distns)
}

# Create a minimal pft list matching what run.meta.analysis.pft() expects
#
# This mirrors the structure from settings$pfts[[i]] after get.trait.data.pft()
# has been run, which adds posteriorid to the pft object.
create_test_pft <- function(outdir = file.path(tempdir(), "test-pft"),
                            pft_name = "temperate.deciduous",
                            posteriorid = 9999L) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  list(
    name        = pft_name,
    outdir      = outdir,
    posteriorid = posteriorid
  )
}

# Write trait.data.Rdata and prior.distns.Rdata files to a directory
# so that run.meta.analysis.pft() can load them
#
# This simulates what get.trait.data.pft() produces as output.
setup_trait_files <- function(outdir,
                              trait_names = "SLA",
                              trait_mean = 20,
                              trait_sd = 2,
                              n_obs = 10,
                              prior_parama = 20,
                              prior_paramb = 5) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  # Build trait.data as a named list of data.frames (one per trait)
  trait.data <- stats::setNames(
    lapply(trait_names, function(tn) {
      create_test_trait_data(
        n_obs = n_obs,
        trait_mean = trait_mean,
        trait_sd = trait_sd
      )
    }),
    trait_names
  )
  save(trait.data, file = file.path(outdir, "trait.data.Rdata"))

  # Build prior.distns data.frame
  prior.distns <- create_test_priors(
    trait_names = trait_names,
    parama = prior_parama,
    paramb = prior_paramb
  )
  save(prior.distns, file = file.path(outdir, "prior.distns.Rdata"))

  invisible(list(trait.data = trait.data, prior.distns = prior.distns))
}
