#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
con <- db.open(list(driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))

test_that("append.covariates appends managements to yields",{
  test.traits <- db.query("select * from traits where id in (select trait_id from covariates) limit 10;", con = con)
  tmpcov <- query.covariates(test.traits$id, con = con)
  covariates <- tmpcov[!duplicated(tmpcov$trait_id),]
  append.test <- append.covariate(data = test.traits, column.name = "level", covariates.data = covariates)
  expect_true(nrow(append.test) >= nrow(covariates))
  # expect_true(nrow(append.test) >= nrow(test.traits))
})

test_that("query.data works",{
  # expect_true(nrow(query.data("SLA", "938")) > 0)
  expect_equal(nrow(query.data("xyz", "-1", con = con)), 0)
  # expect_true(nrow(query.data("LMA", spstr = vecpaste(1:20000))) > 1000)
})


context("test that expected tables exist") # modeltypes
expected_tables <- c("citations", "citations_sites", "citations_treatments", 
                     "covariates", "cultivars", "dbfiles", "ensembles", "entities",
                     "formats", "formats_variables", "inputs", "inputs_runs", "inputs_variables",
                     "likelihoods", "location_yields", "machines", "managements", "managements_treatments",
                     "methods", "mimetypes", "models", "pfts", "pfts_priors", "pfts_species", 
                     "posteriors", "priors", "runs", "schema_migrations", 
                     "sessions", "sites", "species", "traits", "treatments", "users", 
                     "variables", "workflows", "yields")
for (t in expected_tables){
  test_that(paste(t, "table exists and has >= 1 columns"),{
    tmp <- db.query(paste("select * from", t, "limit 1"), con = con) # will fail if table missing
  })
}  

## the following suite could be more comprehensive, and only focus on fields used by PEcAn
test_that("database has a workflows table with appropriate columns",{
  ## regression test for redmine #1128
  workflows <- db.query("select * from workflows;", con = con)
  if(nrow(workflows) >= 1){
    expect_true(all(c("id", "folder", "started_at", "finished_at", "created_at", 
                      "updated_at", "site_id", "model_id", "hostname", "params", "advanced_edit", 
                      "start_date", "end_date") %in% colnames(workflows)))    
  }

})

## the following suite could be more comprehensive, and only focus on fields used by PEcAn
test_that("sites have id and geometry column",{
  ## regression test for redmine #1128
  sites <- db.query("select * from sites limit 1;", con = con)
  expect_true(all(c("id", "city", "state", "country", "mat", "map", "soil", "som", 
                    "notes", "soilnotes", "created_at", "updated_at", "sitename", 
                    "greenhouse", "user_id", "local_time", "sand_pct", "clay_pct", 
                    "geometry")
                  %in% colnames(sites)))
})

test_that("query.covariates returns expected data.frame",{
  ids <- 1:10
  test.query <- query.covariates(ids, con = con)
  expect_equal(colnames(test.query), c("trait_id", "level", "name"))
  expect_true(nrow(test.query) >= length(ids))
})

db.close(con = con)