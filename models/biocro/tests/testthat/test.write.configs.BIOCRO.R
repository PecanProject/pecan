context("checking write.configs.BIOCRO")

settings.xml <- file.path("data", "pecan.biocro.xml")
settings <- PEcAn.settings::read.settings(settings.xml)
settings$database$bety <- do.call(
  PEcAn.DB::get_postgres_envvars,
  settings$database$bety)

testthat::skip_if_not(PEcAn.DB::db.exists(settings[[c("database", "bety")]]))

settings <- PEcAn.settings::prepare.settings(settings)

samples <- list(biocro.saof = (data.frame(
  Vcmax = c(31.9, 42.4, 57),
  cuticular_cond = c(1800, 4380, 10700),
  leaf_respiration_rate_m2 = c(1, 1.9, 3.6),
  stomatal_slope.BB = c(2.7, 3.3, 3.9),
  row.names = c("15.866", "50", "84.134"))))

test_that("convert.samples.BIOCRO works for BioCro 0.9", {
  biocro.parms <- convert.samples.BIOCRO(samples$biocro.saof, 0.9)
  expect_equal(dim(biocro.parms), dim(samples$biocro.saof))
  expect_null(biocro.parms[["vmax1"]])
  expect_equal(biocro.parms$vmax, samples$biocro.saof$Vcmax)
  expect_equal(biocro.parms$b0, samples$biocro.saof$cuticular_cond/1e+06)
  expect_equal(biocro.parms$SLA, samples$biocro.saof$SLA)
  expect_equal(biocro.parms$Rd, samples$biocro.saof$leaf_respiration_rate_m2)
  expect_equal(biocro.parms$b1, samples$biocro.saof$stomatal_slope.BB)
})

test_that("convert.samples.BIOCRO works for BioCro 1.0", {
  biocro.parms <- convert.samples.BIOCRO(samples$biocro.saof, 1.0)
  expect_equal(dim(biocro.parms), dim(samples$biocro.saof))
  expect_null(biocro.parms[["vmax"]]) # [[ instead of $ to avoid partial matching to "vmax1"
  expect_equal(biocro.parms$vmax1, samples$biocro.saof$Vcmax)
  expect_equal(biocro.parms$b0, samples$biocro.saof$cuticular_cond/1e+06)
  expect_equal(biocro.parms$iSp, samples$biocro.saof$SLA)
  expect_equal(biocro.parms$Rd, samples$biocro.saof$leaf_respiration_rate_m2)
  expect_equal(biocro.parms$b1, samples$biocro.saof$stomatal_slope.BB)
})

test_that("convert.samples.BIOCRO accepts list, matrix, data frame", {
  in_df <- data.frame(Vcmax = 1, b0 = 2, SLA=3)
  in_lst <- list(Vcmax = 1, b0 = 2, SLA = 3)
  in_mtrx <- matrix(1:3, ncol = 3, dimnames = list(NULL, c("Vcmax", "b0", "SLA")))
  
  out <- in_df
  out$SLA <- out$SLA/10 # bety sends kg/m2, biocro takes g/cm2
  colnames(out) <- c("vmax1", "b0", "iSp")
    
  expect_equal(convert.samples.BIOCRO(in_df, 1.0), out)  # Redmine #1491
  expect_equal(convert.samples.BIOCRO(in_lst, 1.0), out)
  expect_equal(convert.samples.BIOCRO(in_mtrx, 1.0), out)
})

test_that("write.config.BIOCRO produces expected output", {
  for (q in rownames(samples$biocro.saof)) {
    outdir <- file.path(settings$modeloutdir, q)
    rundir <- file.path(settings$rundir, q)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    dir.create(rundir, showWarnings = FALSE, recursive = TRUE)
    
    trait.values <- list()
    trait.values[[settings$pfts$pft$name]] <- samples$biocro.saof[q, ]
    
    readme <- file.path("data", "README.txt")
    species <- file.path("data", "species.csv")
    
    expect_true(file.exists(readme))
    expect_true(file.exists(species))
    
    expect_true(file.copy(readme, file.path(rundir, "README.txt"), overwrite = TRUE))
    expect_true(file.copy(species, file.path(settings$pfts$pft$outdir, "species.csv"), 
      overwrite = TRUE))
    
    # mock_version stub always reports BioCro version as 0.95
    mockery::stub(write.config.BIOCRO, "utils::packageVersion", mock_version)
    write.config.BIOCRO(defaults = settings$pfts, trait.values = trait.values, 
      settings = settings, run.id = q)
    
    config <- file.path(rundir, "config.xml")
    expect_true(file.exists(config))
    
    config.xml <- XML::xmlParse(config)
    config.list <- XML::xmlToList(config.xml)
    biocro.trait.values <- convert.samples.BIOCRO(trait.values[[settings$pfts$pft$name]], 0.9)
    expect_equal(biocro.trait.values[["vmax"]], as.numeric(config.list$pft$photoParms[["vmax"]]))
    expect_equal(biocro.trait.values[["b0"]], as.numeric(config.list$pft$photoParms[["b0"]]))
    expect_equal(biocro.trait.values[["b1"]], as.numeric(config.list$pft$photoParms[["b1"]]))
    expect_equal(biocro.trait.values[["Rd"]], as.numeric(config.list$pft$photoParms[["Rd"]]))
  }
  
})


test_that("get_biocro_defaults returns a list and warns if no match", {

  # Mock up the relevant bits of a result from data("packagename"),
  # containing all combinations of genus and param list:
  # "sorghum_initial_state", "sorghum_parameters", ..., "Zea_mays_modules"
  data_result <- list(
    results = matrix(
      data = as.vector(sapply(
        c("sorghum", "Zea_diploperennis", "Zea_mays"),
        paste0,
        c("_initial_state",   "_parameters", "_modules"))),
      ncol = 1,
      dimnames = list(NULL, "Item")))
  mockery::stub(get_biocro_defaults, "utils::data", function(...)data_result)

  # Mock up results from from_bc, which always returns a list but the contents
  # depend which dataset was requested.
  # It's called four times per invocation of get_biocro_results, so we provide
  # four appropriate mock responses and cycle over them
  mock_default_list <- mockery::mock(
    list(canopy_module_name="c4_canopy"), # <genus>_modules
    list(Stem=1, Leaf=0.02), # <genus>_initial_state
    list(Rd=1.1, jmax=180), #<genus>_parameters
    list(canopy_module_name="c4_canopy", # <genus>_modules again
      soil_module_name="one_layer_soil_profile"),
    cycle=TRUE)
  mockery::stub(get_biocro_defaults, "from_bc", mock_default_list)

  sorg <- get_biocro_defaults("Sorghum")
  mockery::expect_called(mock_default_list, 4)
  mockery::expect_args(mock_default_list, 1, "sorghum_modules")
  mockery::expect_args(mock_default_list, 2, "sorghum_initial_state")
  mockery::expect_args(mock_default_list, 3, "sorghum_parameters")
  mockery::expect_args(mock_default_list, 4, "sorghum_modules")
  expect_type(sorg, "list")
  expect_length(sorg, 4)
  expect_equal(sorg$parameters$jmax, 180)
  expect_equal(sorg$type$photosynthesis, "C4")
  expect_equal(sorg$type$genus, "sorghum")

  zea_msg <- capture.output(
    {zea_res <- get_biocro_defaults("Zea")},
    type = "message")
  expect_match(
    zea_msg,
    "Multiple possible default parameter sets for Zea",
    all = FALSE)
  mockery::expect_called(mock_default_list, 8)
  mockery::expect_args(mock_default_list, 7, "Zea_diploperennis_parameters")
  expect_is(zea_res, "list")
  expect_length(zea_res, 4)
  expect_equal(zea_res$initial_values$Stem, 1)
  expect_equal(zea_res$type$genus, "Zea_diploperennis")
    # ^ Correct behavior, but maybe not what our hypothetical user wanted!

  null_msg <- capture.output(
    {null_res <- get_biocro_defaults("Not_a_genus")},
    type = "message")
  expect_match(
    null_msg,
    "No default parameter sets for Not_a_genus found in BioCro",
    all=FALSE)
  expect_null(null_res)
  # expect get_biocro_defaults to exit w/o calling from_bc => # calls unchanged
  mockery::expect_called(mock_default_list, 8)
})
