## #-------------------------------------------------------------------------------
## # Copyright (c) 2012 University of Illinois, NCSA.
## # All rights reserved. This program and the accompanying materials
## # are made available under the terms of the 
## # University of Illinois/NCSA Open Source License
## # which accompanies this distribution, and is available at
## # http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
# dummy trait values
trait.values <-
  list(
    SetariaWT = structure(
      list(
        mort2 = 19.9551305310619,
        growth_resp_factor = 0.271418338660799,
        leaf_turnover_rate = 4.08633744111248,
        leaf_width = 4.16095405673501,
        nonlocal_dispersal = 0.208572992916628,
        fineroot2leaf = 2.25017242716454,
        root_turnover_rate = 0.527523622000746,
        seedling_mortality = 0.949939872416094,
        stomatal_slope = 4.07086860946459,
        quantum_efficiency = 0.0565042189665225,
        Vcmax = 22.3047025944851,
        r_fract = 0.313812660341759,
        cuticular_cond = 12992.9906222683,
        root_respiration_rate = 5.48040042748477,
        Vm_low_temp = 10.0000004842057,
        SLA = 40.1495401375622
      ),
      row.names = "50",
      class = "data.frame"
    ),
    env = structure(
      list(),
      .Names = character(0),
      row.names = "NA",
      class = "data.frame"
    )
  )

defaults <-
  list(
    pft = list(
      name = "SetariaWT",
      ed2_pft_number = "1",
      outdir = "/home/ericrscott/model-vignettes/ED2/testoutput/two_pfts/outdir//pft/SetariaWT",
      posteriorid = 9000001416
    )
  )

test_that("convert.samples.ED works as expected", {
  testthat::local_edition(3)
  expect_equal(convert.samples.ED(c("Vcmax" = 1))[["Vcmax"]],
               0.7052557)
  expect_equal(convert.samples.ED(c("plant_min_temp" = 0))[["plant_min_temp"]], 273.15)
  expect_equal(convert.samples.ED(c("root_respiration_rate" = 1))[["root_respiration_factor"]],
               0.35263,
               tolerance = 1e-5)
})

testdir <- tempfile()
dir.create(testdir)
withr::defer(unlink(testdir, recursive = TRUE))
unzip("data/outdir.zip", exdir = testdir)
# unzip("models/ed/tests/testthat/data/outdir.zip", exdir = testdir)
outdir <- file.path(testdir, "outdir")

test_that("write.config.jobsh.ED2() writes correct model2netcdf.ED2() args", {
  settings <- 
    PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
  settings$outdir <- outdir
  job.sh <- write.config.jobsh.ED2(settings, run.id = "test_run")
  expect <- deparse(dput(extract_pfts(settings$pfts)))
  expect_true(any(stringr::str_detect(job.sh, stringr::fixed(expect))))
})

test_that("write.config.jobsh.ED2() works with long list of PFTs", {
  settings <- 
    PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
  more_pfts <- list(
    pft = list(name = "tempconif", ed2_pft_number = 7),
    pft = list(name = "temperate.Evergreen_Hardwood", ed2_pft_number = 8),
    pft = list(name = "temperate.Early_Hardwood", ed2_pft_number = 9),
    pft = list(name = "temperate.North_Mid_Hardwood", ed2_pft_number = 10),
    pft = list(name = "temperate.Late_Hardwood", ed2_pft_number = 11)
    )
  settings$pfts <- append(settings$pfts, more_pfts)
  job.sh <- write.config.jobsh.ED2(settings, run.id = "test_run")
  expect <- deparse1(dput(extract_pfts(settings$pfts)))
  expect_true(any(stringr::str_detect(job.sh, stringr::fixed(expect))))
})

test_that("New ED2IN tags get added at bottom of file", {
  #1. read in pecan.xml in data/pecan_checked.xml
  settings <- PEcAn.settings::read.settings("data/pecan_checked.xml")
  #for debugging:
  # settings <- PEcAn.settings::read.settings("models/ed/tests/testthat/data/pecan_checked.xml")
  
  #2. Set rundir to tempdir
  rundir <- tempfile()
  dir.create(rundir)
  on.exit(unlink(rundir, recursive = TRUE))
  settings$rundir <- rundir
  run.id <- "ENS-00001-76"
  dir.create(file.path(rundir, run.id))
  #3. add arbitrary ed2in_tag to settings list
  settings$model$ed2in_tags$NEW_TAG <- "0"
  #4. run write.config.ED2 
  trait.values <-
    list(
      SetariaWT = structure(
        list(
          mort2 = 19.9551305310619,
          growth_resp_factor = 0.271418338660799,
          leaf_turnover_rate = 4.08633744111248,
          leaf_width = 4.16095405673501,
          nonlocal_dispersal = 0.208572992916628,
          fineroot2leaf = 2.25017242716454,
          root_turnover_rate = 0.527523622000746,
          seedling_mortality = 0.949939872416094,
          stomatal_slope = 4.07086860946459,
          quantum_efficiency = 0.0565042189665225,
          Vcmax = 22.3047025944851,
          r_fract = 0.313812660341759,
          cuticular_cond = 12992.9906222683,
          root_respiration_rate = 5.48040042748477,
          Vm_low_temp = 10.0000004842057,
          SLA = 40.1495401375622
        ),
        row.names = "50",
        class = "data.frame"
      ),
      env = structure(
        list(),
        .Names = character(0),
        row.names = "NA",
        class = "data.frame"
      )
    )
  
  defaults <-
    list(
      pft = list(
        name = "SetariaWT",
        ed2_pft_number = "1",
        outdir = "/home/ericrscott/model-vignettes/ED2/testoutput/two_pfts/outdir//pft/SetariaWT",
        posteriorid = 9000001416
      )
    )
  old_level <- PEcAn.logger::logger.setLevel("DEBUG")
  x <- capture.output(
    write.config.ED2(
      trait.values = trait.values,
      settings = settings,
      run.id = run.id,
      defaults = defaults,
      check = FALSE
    ),
    type = "message"
  )
  PEcAn.logger::logger.setLevel(old_level)
  
  
  #5. check if new tag exists
  ed2in_out <- read_ed2in(file.path(rundir, run.id, "ED2IN"))
  expect_equal(ed2in_out$NEW_TAG, 0)
  
  #check that info is printed

  expect_true(any(stringr::str_detect(x, "NEW_TAG")))
  
  #check that last non-comment line of ED2IN is "$END"
  #TODO someone better at regex could do this more efficiently
  lines <- trimws(readLines(file.path(rundir, run.id, "ED2IN")))
  not_comments <- lines[stringr::str_detect(lines, "^!", negate = TRUE)]
  not_spaces <- not_comments[stringr::str_detect(not_comments, ".+")]
  expect_equal(not_spaces[length(not_spaces)], "$END")
  
  #6. compare to template
  # ed2in_template <- read_ed2in(system.file(settings$model$edin, package = "PEcAn.ED2"))
  # Not sure what to expect regarding tag names or number of tags relative to template
})


test_that("write.config.xml.ED2() uses correct history file", {
  #1. read in pecan.xml in data/pecan_checked.xml
  settings <- PEcAn.settings::read.settings("data/pecan_checked.xml")
  #for debugging:
  # settings <- PEcAn.settings::read.settings("models/ed/tests/testthat/data/pecan_checked.xml")
  
  #2. Set rundir to tempdir
  rundir <- tempfile()
  dir.create(rundir)
  on.exit(unlink(rundir, recursive = TRUE))
  settings$rundir <- rundir
  run.id <- "ENS-00001-76"
  dir.create(file.path(rundir, run.id))
  #3. set revision to 81
  settings$model$revision <- "81"

  x <- capture.output(
    write.config.xml.ED2(
      settings = settings,
      trait.values = trait.values,
      defaults = defaults
    ),
    type = "message"
  )
  
  expect_true(any(stringr::str_detect(x, "history.r81")))
  
})


## test_that("remove.configs.ED2 works with remote host",{
##   settings <- list(outdir = "/tmp/",
##                    run = list(host = list(name = "ebi-cluster.igb.illinois.edu",
##                                 rundir = "/home/scratch/tmp/",
##                                 outdir = "/home/scratch/tmp/")))
  
##   system("ssh ebi-cluster.igb.illinois.edu 'touch /home/scratch/tmp/c.foo'")
##   expect_output(remove.config.ED2(main.outdir = settings$outdir, settings = settings),
##                 "/home/scratch/tmp/c.foo")
##   system("ssh ebi-cluster.igb.illinois.edu 'touch /home/scratch/tmp/ED2INc.foo'")
##   expect_output(remove.config.ED2(settings$outdir, settings),
##                 "/home/scratch/tmp//ED2INc.foo")
##   file.remove("bug1325.xml")
## })
