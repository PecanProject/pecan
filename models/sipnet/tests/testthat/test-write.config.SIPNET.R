test_that("write.config.SIPNET", {
  pth <- withr::local_tempdir()

  event_lines <- "2025 1 irrig 0 1"
  event_src_path <- file.path(pth, "events-a.in")
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
  writeLines(event_lines, con = event_src_path)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = ""),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(
          met = list(path = ""),
          events = list(path = event_src_path)
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  res <- write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(Amax = 5, AmaxFrac = 0.99, leafC = 47)),
    settings = s,
    run.id = "run1"
  )

  # events file correctly copied
  expect_match(
    readLines(file.path(pth, "run", "run1", "events.in")),
    event_lines,
    fixed = TRUE,
    all = TRUE
  )

  # (at least some) parameters updated
  param_result <- readLines(file.path(pth, "run", "run1", "sipnet.param"))
  expect_match(
    param_result,
    "aMax 10", # this is Amax * SLA,
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    param_result,
    "aMaxFrac 0.99", # raw template had 0.76
    fixed = TRUE,
    all = FALSE
  )
  # leaf C specific weight is leafC / SLA, with units converted to g C/m2 leaf
  expect_match(
    param_result,
    "leafCSpWt 235 ", # space at end to catch unit errors (eg fail on 23500)
    fixed = TRUE,
    all = FALSE
  )

})


test_that("the config writer accepts NULL defaults and matches named constants", {
  path <- withr::local_tempdir()
  settings <- PEcAn.settings::as.Settings(list(
    outdir = file.path(path, "out"), rundir = file.path(path, "run"),
    modeloutdir = file.path(path, "out"),
    pfts = list(cropB = list()),
    model = list(binary = "", revision = "v2.2.0"),
    run = list(
      site = list(name = "site1", lat = 40, lon = -88),
      inputs = list(met = list(path = "")),
      start.date = "2025-01-01", end.date = "2025-01-02"
    ),
    host = list(name = "localhost", outdir = file.path(path, "out"),
                rundir = file.path(path, "run"))
  ))
  for (id in c("template", "override")) {
    dir.create(file.path(settings$rundir, id), recursive = TRUE)
  }
  traits <- list(cropB = list())
  template <- utils::read.table(system.file("template.param_v2", package = "PEcAn.SIPNET"))
  write.config.SIPNET(NULL, traits, settings, "template")
  result <- utils::read.table(file.path(settings$rundir, "template", "sipnet.param"))
  expect_equal(result$V2[result$V1 == "aMaxFrac"], template$V2[template$V1 == "aMaxFrac"])

  defaults <- list(
    list(name = "cropA", constants = list(AmaxFrac = 0.2)),
    list(name = "cropB", constants = list(AmaxFrac = 0.8))
  )
  write.config.SIPNET(defaults, traits, settings, "override")
  result <- utils::read.table(file.path(settings$rundir, "override", "sipnet.param"))
  expect_equal(result$V2[result$V1 == "aMaxFrac"], 0.8)
})

test_that("PFT constants resolve names without positional cross-crop reuse", {
  defaults <- list(
    list(name = "cropA", constants = list(AmaxFrac = 0.2)),
    list(name = "cropB", constants = list(AmaxFrac = 0.8)),
    list(name = "soil", constants = list(frozenSoilEff = 0.35))
  )
  expect_equal(
    sipnet_pft_constants(defaults, c("cropB", "soil")),
    list(list(AmaxFrac = 0.8), list(frozenSoilEff = 0.35))
  )
  expect_equal(
    sipnet_pft_constants(defaults, c("cropB", "soil", "env")),
    list(list(AmaxFrac = 0.8), list(frozenSoilEff = 0.35), NULL)
  )
  expect_equal(
    sipnet_pft_constants(list(list(constants = list(AmaxFrac = 0.2))), c("cropA", "soil")),
    rep(list(list(AmaxFrac = 0.2)), 2)
  )
  expect_equal(
    sipnet_pft_constants(list(list(constants = list(AmaxFrac = 0.2))), c("cropA", "env")),
    list(list(AmaxFrac = 0.2), NULL)
  )
  expect_error(sipnet_pft_constants(list(list(), list()), "cropA"), "unique names")
  expect_error(sipnet_pft_constants(defaults, "missing"), "unique names")
  defaults[[2]]$name <- "cropA"
  expect_error(sipnet_pft_constants(defaults, "cropA"), "unique names")
})

test_that("generic ensemble and SA writers accept the auxiliary environment group", {
  path <- withr::local_tempdir()
  settings <- PEcAn.settings::as.Settings(list(
    outdir = file.path(path, "out"), rundir = file.path(path, "run"),
    modeloutdir = file.path(path, "out"),
    pfts = list(
      list(name = "cropA", constants = list(AmaxFrac = 0.2)),
      list(name = "cropB", constants = list(AmaxFrac = 0.8)),
      list(name = "soil", constants = list(frozenSoilEff = 0.35))
    ),
    ensemble = list(size = 3, samplingspace = list(parameters = list(method = "uniform"))),
    sensitivity.analysis = list(quantiles = c(0.25, 0.5, 0.75)),
    model = list(binary = "", revision = "v2.2.0"),
    run = list(
      site = list(id = "772", name = "site1", lat = 40, lon = -88),
      inputs = list(met = list(path = "")),
      start.date = "2025-01-01", end.date = "2025-01-02"
    ),
    host = list(name = "localhost", outdir = file.path(path, "out"),
                rundir = file.path(path, "run"))
  ))
  prior <- tibble::tibble(
    trait = "AmaxFrac", distn = "norm", parama = 0.5, paramb = 0.05, n = 10
  ) |> tibble::column_to_rownames("trait")
  soil_prior <- prior
  rownames(soil_prior) <- "frozenSoilEff"
  withr::local_seed(4136)
  samples <- PEcAn.uncertainty::get_parameter_samples(
    pft_names = c("cropB", "soil"), prior_distns_list = list(prior, soil_prior),
    ensemble.size = 3, sa_quantiles = c(0.25, 0.5, 0.75)
  )
  expect_null(settings$run$site$site.pft)
  expect_named(samples$ensemble.samples, c("cropB", "soil", "env"))

  for (analysis in c("ensemble", "sensitivity")) {
    if (analysis == "ensemble") {
      design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings, 3, samples)
      run_samples <- samples$ensemble.samples
      descriptions <- NULL
    } else {
      design <- PEcAn.uncertainty::generate_OAT_SA_design(settings, samples = samples)
      run_samples <- PEcAn.uncertainty::sa_run_samples(samples$sa.samples, design$design_matrix)
      descriptions <- PEcAn.uncertainty::sa_run_descriptions(
        design$design_matrix, "772", c("cropB", "soil")
      )
    }
    # auxiliary samples still reach the model writer without acquiring PFT constants
    run_samples$env$soilWHC <- rep(7, nrow(design$design_matrix))
    result <- PEcAn.uncertainty::write.ensemble.configs(
      input_design = design$design_matrix, ensemble.size = nrow(design$design_matrix),
      defaults = settings$pfts, ensemble.samples = run_samples, settings = settings,
      model = "SIPNET", write.to.db = FALSE, run_descriptions = descriptions
    )
    expect_equal(nrow(result$runs), nrow(design$design_matrix))
    for (id in result$runs$id) {
      param <- utils::read.table(file.path(settings$rundir, id, "sipnet.param"))
      expect_equal(param$V2[param$V1 == "aMaxFrac"], 0.8)
      expect_equal(param$V2[param$V1 == "frozenSoilEff"], 0.35)
      expect_equal(param$V2[param$V1 == "soilWHC"], 7)
    }
  }
})

test_that("update_flag_lines", {
  txt <- c("!comment", "NITROG = 0", "GDD = 1")

  # existing lines updated, new lines added
  expect_equal(
    update_flag_lines(txt, c(GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
  expect_equal(
    update_flag_lines(txt, c(new_flag = 0, NITROG = "1")),
    c("!comment", "NITROG = 1", "GDD = 1", "new_flag = 0")
  )

  # empty flags return input
  expect_equal(txt, update_flag_lines(txt, c()))
  expect_equal(txt, update_flag_lines(txt, NULL))

  # unnamed arguments ignored
  expect_equal(txt, update_flag_lines(txt, c("unnamed")))
  expect_equal(
    update_flag_lines(txt, c(1, GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
})

test_that("leafNResorptionFrac trait reaches the v2 param file", {
  pth <- withr::local_tempdir()
  event_src_path <- file.path(pth, "events-a.in")
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
  writeLines("2025 1 irrig 0 1", con = event_src_path)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = "v2.2.0"),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(
          met = list(path = ""),
          events = list(path = event_src_path)
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(leafNResorptionFrac = 0.6)),
    settings = s,
    run.id = "run1"
  )

  param_result <- readLines(file.path(pth, "run", "run1", "sipnet.param"))
  expect_match(param_result, "leafNResorptionFrac 0.6", fixed = TRUE, all = FALSE)
})

test_that("stem and root respiration are written as per year rates", {
  pth <- withr::local_tempdir()
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
  s <- PEcAn.settings::as.Settings(list(
    outdir = file.path(pth, "out"), rundir = file.path(pth, "run"),
    pfts = list(pft1 = list()),
    model = list(binary = "", revision = "v2.2.0"),
    run = list(site = list(name = "site1", lat = 40, lon = -88),
               inputs = list(met = list(path = "")),
               start.date = "2025-01-01", end.date = "2025-01-02"),
    host = list(name = "", outdir = file.path(pth, "out"), rundir = file.path(pth, "run"))
  ))
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(stem_respiration_rate = 1, root_respiration_rate = 10)),
    settings = s, run.id = "run1"
  )
  p <- utils::read.table(file.path(pth, "run", "run1", "sipnet.param"), stringsAsFactors = FALSE)
  param_value <- function(name) p[p[[1]] == name, 2]

  # 1 umol CO2 kg-1 s-1 is 12.01e-9 g C g-1 s-1; sipnet's year is 365 days.
  # template Q10s (2.0 veg, 2.6 fine root) take the 25 C rates to 0 C
  g_per_year <- 12.01e-9 * 86400 * 365
  expect_equal(param_value("baseVegResp"), 1 * g_per_year * 2.0^(-2.5))
  expect_equal(param_value("baseFineRootResp"), 10 * g_per_year * 2.6^(-2.5))
})

test_that("plantStorageNInit is sized to the carbon-limited leaf flush", {
  # scaffold for a v2 run; the irrig-only event file keeps internal phenology
  make_settings <- function(pth) {
    event_src_path <- file.path(pth, "events-a.in")
    dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
    writeLines("2025 1 irrig 0 1", con = event_src_path)
    PEcAn.settings::as.Settings(
      list(
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run"),
        pfts = list(pft1 = list()),
        model = list(binary = "", revision = "v2.2.0"),
        run = list(
          site = list(name = "site1", lat = 40, lon = -88),
          inputs = list(
            met = list(path = ""),
            events = list(path = event_src_path)
          ),
          start.date = "2025-01-01",
          end.date = "2025-01-02"
        ),
        host = list(
          name = "",
          outdir = file.path(pth, "out"),
          rundir = file.path(pth, "run")
        )
      )
    )
  }
  read_param <- function(pth) {
    utils::read.table(file.path(pth, "run", "run1", "sipnet.param"),
                      stringsAsFactors = FALSE)
  }
  pval <- function(p, name) p[p[[1]] == name, 2]

  # expected value from the same template the config writer reads
  tmpl <- utils::read.table(
    system.file("template.param_v2", package = "PEcAn.SIPNET"),
    stringsAsFactors = FALSE
  )
  tval <- function(name) tmpl[tmpl[[1]] == name, 2]
  expected_n <- min(tval("leafGrowth"),
                    tval("plantWoodInit") * (1 - tval("fineRootFrac")) *
                      tval("leafOnReallocFrac")) *
    max(0, 1 / tval("leafCN") - 1 / tval("woodCN"))

  # computed default from the template wood pool
  pth <- withr::local_tempdir()
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list()),
    settings = make_settings(pth), run.id = "run1"
  )
  expect_equal(pval(read_param(pth), "plantStorageNInit"), expected_n)
  expect_gt(expected_n, 0)

  # an IC supplied value takes precedence over the computed default
  pth <- withr::local_tempdir()
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list()),
    settings = make_settings(pth), run.id = "run1",
    IC = data.frame(plantStorageNInit = 3.3)
  )
  expect_equal(pval(read_param(pth), "plantStorageNInit"), 3.3)

  # zero initial wood means no flush to support: storage N stays 0.
  # the computation must use the IC adjusted wood pool, not the template's.
  pth <- withr::local_tempdir()
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list()),
    settings = make_settings(pth), run.id = "run1",
    IC = data.frame(AbvGrndWood = 0, abvGrndWoodFrac = 0.6,
                    coarseRootFrac = 0.2, fineRootFrac = 0.2)
  )
  p <- read_param(pth)
  expect_equal(pval(p, "plantWoodInit"), 0)
  expect_equal(pval(p, "plantStorageNInit"), 0)
})

test_that("leaf carbon from a poolinitcond file sets the initial leaf pool", {
  pth <- withr::local_tempdir()
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)

  ic_path <- file.path(pth, "ic.nc")
  time_dim <- ncdf4::ncdim_def("time", "days since 2025-01-01", 0)
  leaf_var <- ncdf4::ncvar_def("leaf_carbon_content", "kg C m-2", time_dim)
  nc <- ncdf4::nc_create(ic_path, leaf_var)
  ncdf4::ncvar_put(nc, leaf_var, 0.1)
  ncdf4::nc_close(nc)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = "2.0"),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(met = list(path = ""), poolinitcond = list(path = ic_path)),
        # inside the template leaf on window, so laiInit is not zeroed
        start.date = "2025-07-01",
        end.date = "2025-07-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list(SLA = 20, leafC = 45)),
    settings = s,
    run.id = "run1"
  )

  p <- utils::read.table(file.path(pth, "run", "run1", "sipnet.param"))
  pval <- function(name) p[p[[1]] == name, 2]
  # sipnet initializes leaf C as laiInit * leafCSpWt (g C m-2);
  # tolerance covers the rounding when sipnet.param is written
  expect_equal(pval("laiInit") * pval("leafCSpWt"), 100, tolerance = 1e-6)
})
