## #-------------------------------------------------------------------------------
## # Copyright (c) 2012 University of Illinois, NCSA.
## # All rights reserved. This program and the accompanying materials
## # are made available under the terms of the 
## # University of Illinois/NCSA Open Source License
## # which accompanies this distribution, and is available at
## # http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
## test_that("convert.samples.ED works as expected",{
##   expect_equivalent(convert.samples.ED(c("Vcmax" = 1)),
##                     0.7052557)
##   expect_equivalent(signif(convert.samples.ED(c("root_respiration_rate" = 1)),5),
##                     c("root_respiration_factor" = 0.35263))
## })

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
