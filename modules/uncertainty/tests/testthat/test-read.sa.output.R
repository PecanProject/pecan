# Tests for read.sa.output

quantiles <- c("15.9", "50", "84.1")

# one manifest shared by two sites, as a multisite run writes it
write_two_site_manifest <- function(dir) {
  sites <- rep(c(1001, 1002), each = 3)
  manifest <- data.frame(
    run_id   = paste(c("lo", "med", "hi"), sites, sep = "-"),
    site_id  = sites,
    pft_name = "pft1",
    trait    = "SLA",
    quantile = quantiles,
    type     = "Sensitivity"
  )
  utils::write.csv(manifest, file.path(dir, "runs_manifest.csv"), row.names = FALSE)
}

site_run_ids <- function(site) {
  design <- data.frame(
    sa_pft      = c(NA, "pft1", "pft1"),
    sa_trait    = c(NA, "SLA", "SLA"),
    sa_quantile = c("50", "15.9", "84.1")
  )
  sa_run_id_table(design, paste(c("med", "lo", "hi"), site, sep = "-"))
}

read_sla <- function(dir, sa.run.ids) {
  # each run's output is the site in its run id
  mockery::stub(read.sa.output, "PEcAn.utils::read.output",
                function(runid, ...) list(NPP = as.numeric(sub(".*-", "", runid))))
  read.sa.output(
    traits = "SLA", quantiles = quantiles, pecandir = dir, outdir = dir,
    pft.name = "pft1", start.year = 2001, end.year = 2001,
    variable = list(expression = "NPP", variables = "NPP"),
    sa.run.ids = sa.run.ids
  )
}


test_that("each site reads its own runs when sites share a manifest", {
  dir <- withr::local_tempdir()
  write_two_site_manifest(dir)

  expect_equal(read_sla(dir, site_run_ids(1001))$SLA, rep(1001, 3))
  expect_equal(read_sla(dir, site_run_ids(1002))$SLA, rep(1002, 3))
})


test_that("without run ids, runs are looked up in the manifest", {
  dir <- withr::local_tempdir()
  write_two_site_manifest(dir)
  manifest <- utils::read.csv(file.path(dir, "runs_manifest.csv"))
  utils::write.csv(manifest[manifest$site_id == 1001, ],
                   file.path(dir, "runs_manifest.csv"), row.names = FALSE)

  expect_equal(read_sla(dir, NULL)$SLA, rep(1001, 3))
})
