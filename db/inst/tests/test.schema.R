##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------

## check database connection as condition of running tests:
test_that("expected tables exist",{
  settings <<- xmlToList(xmlParse("pecan.xml"))
  if(db.exists(settings$database)){
    tables <- db.query("show tables;", params=settings$database)
    ## make sure that all tables are present:
    expect_true(all(sapply(c("citations", "citations_sites", "citations_treatments", 
                             "coppice", "counties", "covariates", "cultivars", "dbfiles", 
                             "ensembles", "entities", "formats", "formats_variables", 
                             "inputs", "inputs_runs", "inputs_variables", "likelihoods", 
                             "location_yields", "machines", "managements", 
                             "managements_treatments", "methods", "mimetypes", "models", 
                             "pfts", "pfts_priors", "pfts_species", "planting", 
                             "posteriors", "posteriors_runs", "priors", "runs", 
                             "schema_migrations", "seeding", "sessions", "sites", "species", 
                             "traits", "treatments", "users", "variables", "workflows", 
                             "yields"), function(x) grepl(x, tables ))))
  }
})
