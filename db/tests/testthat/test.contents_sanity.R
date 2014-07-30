#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


test_that("append.covariates appends managements to yields",{
    test.traits <- query.base("select * from traits where id in (select trait_id from covariates) limit 10;", con = con)
    tmpcov <- query.covariates(test.traits$id, con = con)
    covariates <- tmpcov[!duplicated(tmpcov$trait_id),]
    append.test <- append.covariate(data = test.traits, column.name = "level", covariates.data = covariates)
    expect_true(nrow(append.test) >= nrow(covariates))
    
    ##    expect_true(nrow(append.test) >= nrow(test.traits))
})

test_that("query.data works",{
    ##    expect_true(nrow(query.data("SLA", "938")) > 0)
    expect_equal(nrow(query.data("xyz", "-1", con = con)), 0)
    ##    expect_true(nrow(query.data("LMA", spstr = vecpaste(1:20000))) > 1000)
})


test_that("expected tables exist",{
  tables <- db.query("show tables;", con = con)[,1]
  ## make sure that all tables are present:
  expected_tables <- c("citations", "citations_sites", "citations_treatments", 
    "covariates", "cultivars", "dbfiles", "ensembles", "entities",
    "formats", "formats_variables", "inputs", "inputs_runs", "inputs_variables",
    "likelihoods", "location_yields", "machines", "managements", "managements_treatments",
    "methods", "mimetypes", "models", "pfts", "pfts_priors", "pfts_species", 
    "posteriors", "posteriors_runs", "priors", "runs", "schema_migrations", 
    "sessions", "sites", "species", "traits", "treatments", "users", 
    "variables", "workflows", "yields")
  expect_true(all(expected_tables %in% tables))
})

## the following suite could be more comprehensive, and only focus on fields used by PEcAn
test_that("database has a workflows table with appropriate columns",{
  ## regression test for redmine #1128
  workflows <- db.query("describe workflows;", con = con)
  expect_true(all(c("site_id", "model_id", "hostname", "start_date", "end_date", "params", "folder", "started_at", "finished_at", "created_at", "updated_at", "advanced_edit") %in% workflows$Field))
})

test_that("query.covariates returns expected data.frame",{
  ids <- 1:10
  test.query <- query.covariates(ids, con = con)
  expect_equal(colnames(test.query), c("trait_id", "level", "name"))
  expect_true(nrow(test.query) >= length(ids))
})
