
settings.xml <- file.path(data, )
settings <- read.settings("data/pecan.biocro.xml")

samples <- list(
  biocro.saof = (data.frame(
    Vcmax = c(31.9, 42.4, 57.0),
    cuticular_cond = c(1800, 4380, 10700),
    leaf_respiration_rate_m2 = c(1.0, 1.9, 3.6),
    stomatal_slope.BB = c(2.7, 3.3, 3.9),
    row.names = c("15.866", "50", "84.134"))))

test_that("convert.samples.BIOCRO works", {
  biocro.parms <- convert.samples.BIOCRO(samples$biocro.saof)
  expect_equal(dim(biocro.parms), dim(samples$biocro.saof))
  expect_equal(biocro.parms$vmax, samples$biocro.saof$Vcmax)
  expect_equal(biocro.parms$b0, samples$biocro.saof$cuticular_cond/1e6)
  expect_equal(biocro.parms$SLA, samples$biocro.saof$SLA)
  expect_equal(biocro.parms$Rd, samples$biocro.saof$leaf_respiration_rate_m2)
  expect_equal(biocro.parms$b1, samples$biocro.saof$stomatal_slope.BB)

  ## re-create bug #1491
  test.list <- list(vmax = 1, b0 = 2)
  convert.samples.BIOCRO(test.list) ## this should work
})

test_that("write.configs.BIOCRO produces expected output",{
  for(q in rownames(samples$biocro.saof)){
    outdir <- file.path(settings$modeloutdir, q)
    rundir <- file.path(settings$rundir, q)
    dir.create(outdir, showWarnings=FALSE, recursive=TRUE)
    dir.create(rundir, showWarnings=FALSE, recursive=TRUE)
    
    trait.values <- list()
    trait.values[[settings$pfts$pft$name]] <- samples$biocro.saof[q, ]
    
    readme <- file.path(extdir, "README.txt")
    species <- file.path(extdir, "species.csv")
    
    expect_true(file.exists(readme))
    expect_true(file.exists(species))
    
    expect_true(file.copy(readme, file.path(rundir, "README.txt"), overwrite=TRUE))
    expect_true(file.copy(species, file.path(settings$pfts$pft$outdir, "species.csv"), overwrite=TRUE))
    
    
    write.config.BIOCRO(defaults = settings$pfts,
                        trait.values = trait.values,
                        settings = settings,
                        run.id = q)
    
    config <- file.path(rundir, "config.xml")
    expect_true(file.exists(config))
    
    config.xml <- xmlParse(config)
    config.list <- xmlToList(config.xml)
    biocro.trait.values <- convert.samples.BIOCRO(trait.values[[settings$pfts$pft$name]])
    expect_equal(biocro.trait.values[["vmax"]], as.numeric(config.list$pft$photoParms[["vmax"]]))  
    expect_equal(biocro.trait.values[["b0"]], as.numeric(config.list$pft$photoParms[["b0"]]))  
    expect_equal(biocro.trait.values[["b1"]], as.numeric(config.list$pft$photoParms[["b1"]]))
    expect_equal(biocro.trait.values[["Rd"]], as.numeric(config.list$pft$photoParms[["Rd"]]))
  }    
  
})
