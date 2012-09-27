#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
require(XML)

context("tests for read.settings and related functions")
test_that("read settings returns error if no settings file found (issue #1124)",{
  default.settings.files <- c("/etc/pecan.xml", "~/.pecan.xml",
                              "pecan.xml", Sys.getenv("PECAN_SETTINGS"))
  ## need to be revised for use with log.* functions
  if(!any(sapply(default.settings.files, file.exists))){
    print("the following error messages are expected results of log.error")
    expect_message(read.settings(), "Did not find any settings file to load.")
  }
})

context("check that example settings file is valid")

settings.list <- read.settings(inputfile = system.file("tests/test.settings.xml",
                                 package = "PEcAn.all"))

test_that("test.settings.xml has an unique output directory for each PFT",{
  pfts <- unlist(settings.list$pfts)
  i.pfts   <- names(pfts) == "pft.name"
  i.outdir <- names(pfts) == "pft.outdir"
  expect_equal(sum(i.pfts), sum(i.outdir))
  expect_equal(sum(i.pfts), length(unique(pfts[i.pfts])))
  expect_equal(length(unique(pfts[i.pfts])), length(unique(pfts[i.outdir])))
  rm(i.pfts, i.outdir)      
})

test_that("read.settings gives expected warnings",{
  writeLines(con = "warning1144.xml",
             text = "<pecan><pfts><pft>
                      <name>testPFTname</name>
                      <outdir>/tmp/</outdir></pft></pfts></pecan>") 
  writeLines(con = "fixed1144.xml",
             text = "<pecan> 
                      <outdir>/tmp/</outdir>
                      <pfts><pft>
                        <name>testPFTname</name>
                        <outdir>/tmp/</outdir>
                      </pft></pfts></pecan>")
  expect_output(read.settings("warning1144.xml"), "No output folder")
  fixed1144 <- read.settings("fixed1144.xml")
  expect_equal(fixed1144$pfts$pft$name, "testPFTname")
  file.remove("bug1144.xml", "warning1144.xml", "fixed1144.xml")
})


test_that("merge 2 xml files", {
  ## merge the files
  print(xmlMerge(xmlParse("a.xml"), xmlParse("b.xml")))
  settings <- xmlToList(xmlMerge(xmlParse("a.xml"), xmlParse("b.xml")))
  
  ## check results
  ## expect_equal(names(settings), c("a", "b", "c"))
 
  expect_equal(names(settings$a), c("text", ".attrs"))
  expect_equal(settings$a$text, "b")
  expect_equal(names(settings$a$.attrs), c("var1", "var2"))
  expect_equal(settings$a$.attrs[['var1']], "2")
  expect_equal(settings$a$.attrs[['var2']], "2")
  
  ## expect_equal(names(settings$b), c("text", ".attrs"))
  ## expect_equal(settings$b$text, "a")
  ## expect_equal(names(settings$b$.attrs), c("var1"))
  ## expect_equal(settings$b$.attrs[['var1']], "1")
  
  ## expect_equal(names(settings$c), c("d", "e", "f", "g", ".attrs"))
  ## expect_equal(names(settings$c$.attrs), c("var1", "var2"))
  ## expect_equal(settings$b$.attrs[['var1']], "1")
  
  expect_equal(names(settings$c$d), NULL)
  expect_equal(settings$c$d, "b")
 
  expect_equal(names(settings$c$e), c("text", ".attrs"))
  expect_equal(settings$c$e$text, "b")
  ## expect_equal(names(settings$c$e$.attrs), c("var1", "var2"))
  expect_equal(settings$c$e$.attrs[['var1']], "2")      
  ## expect_equal(settings$c$e$.attrs[['var2']], "1")      
  
  expect_equal(names(settings$c$f), NULL)
  ## expect_equal(settings$c$f, "a")
  
  expect_equal(names(settings$c$g), NULL)
  expect_equal(settings$c$g, "b")
})

test_that("logger prints right messages",{
  ## log.debug("debug")
  ## log.enable("DEBUG", FALSE)
  ## log.debug("debug")

  ## log.info("info")
  ## log.enable("INFO", FALSE)
  ## log.info("info")

  ## log.warn("warn")
  ## log.enable("WARN", FALSE)
  ## log.warn("warn")

  ## log.error("error")
  ## log.enable("ERROR", FALSE)
  ## log.error("error")

  ## log.enable("ERROR", TRUE)
  ## log.output(console=FALSE)
  ## log.error("error")
})
## test_that("read.settings can set up directories on a remote server",{
## if(PEcAn.utils::test.remote("ebi-cluster.igb.illinois.edu")){
##   writeLines(con = "bug1322.xml", text = "<pecan><outdir>/tmp/</outdir>
##   <pfts><pft><name>'foo'</name><outdir>/tmp/</outdir></pft></pfts>
##   <run><folder>/tmp/</folder>
##   <host>
##     <name>ebi-cluster.igb.illinois.edu</name>
##     <rundir>/home/scratch/tmp/</rundir>
##     <outdir>/home/scratch/tmp/</outdir>
##   </host></run></pecan>")
##   read.settings("bug1322.xml")
##   file.remove("bug1322.xml")
##   }
## })
