##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------

read.settings <- PEcAn.common::read.settings
temp.settings <- PEcAn.utils::temp.settings
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
    <location>localhost</location>
    <name>ebi_analysis</name>
  </database>
</pecan>"

test_that("get.trait.data will return trait data even when there is no meta.analysis tag in the settings file", {
  settings <- read.settings(settings.text)
#  get.trait.data()
#  expect_true(file.exists('/tmp/test/trait.data.Rdata'))
})
