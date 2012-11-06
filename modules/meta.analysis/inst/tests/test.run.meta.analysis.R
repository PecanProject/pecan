#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
test_that("p.point.in.prior function is functional",{
  prior <- list(distn = "norm", parama = 0, paramb = 1)
  expect_equal(p.point.in.prior(point = 3, prior = prior),
               pnorm(3))
  expect_equal(p.point.in.prior(point = -3, prior = prior),
               pnorm(-3))
})

test_that("meta-analysis is skipped if trait.data is empty",{
    settings.text <- "
<pecan>
  <pfts>
    <pft>
      <name>ebifarm.pavi</name>
      <outdir>/tmp/test/</outdir>
    </pft>
  </pfts>
  <outdir>/tmp/test/</outdir>
  <database>
    <userid>ebi_analys_user</userid>
    <passwd>b742xsAu</passwd>
    <host>ebi-forecast</host>
    <name>ebi_analysis</name>
  </database>
  <meta.analysis>
    <iter>3000</iter>
    <random.effects>FALSE</random.effects>
  </meta.analysis>
</pecan>"
    settings <- read.settings(settings.text)
    trait.data <- list()
    save(trait.data, file = file.path(settings$pfts$pft$outdir, "trait.data.Rdata"))
    run.meta.analysis()
