#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

test_that("trait dictionary loads and has expected columns",{
  rm(list = ls())
  data(trait.dictionary)
  expect_true(exists("trait.dictionary"))
  expect_true(all(c("id", "figid", "units", "model.id") %in%
                  colnames(ed.trait.dictionary)))
  expect_true(ncol(trait.dictionary) >= 4) # dim = 49 x 4 at time of writing
  expect_true(nrow(trait.dictionary) >=49)
test.stats <- data.frame(Y=rep(1,5),
                           stat=rep(1,5),
                           n=rep(4,5),
                           statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD'))
})

test_that("get.trait.data will return trait data even when there is no meta.analysis tag in the settings file", {
  writeLines(" 
<pecan>
  <pfts>
    <pft>
      <name>ebifarm.pavi</name>
      <outdir>test/</outdir>
    </pft>
  </pfts>
  <outdir>test/</outdir>
  <database>
    <userid>ebi_analys_user</userid>
    <passwd>b742xsAu</passwd>
    <location>localhost</location>
    <name>ebi_analysis</name>
  </database>
</pecan>", con = "test/test.xml")

  settings <- read.settings("test/test.xml")
  get.trait.data()
  
  expect_true(file.exists("test/trait.data.Rdata"))
})
