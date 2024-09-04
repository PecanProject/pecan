context("Basic Sanity tests for PEcAn functions that query BETYdb")
test_that("append.covariates appends managements to yields",{
  con <- check_db_test()
  test.traits <- db.query("select * from traits where id in (select trait_id from covariates) limit 10;", con = con)
  tmpcov <- query.covariates(test.traits$id, con = con)
  covariates <- tmpcov[!duplicated(tmpcov$trait_id),]
  append.test <- append.covariate(data = test.traits, column.name = "level", covariates.data = covariates)
  expect_true(nrow(append.test) >= nrow(covariates))
  # expect_true(nrow(append.test) >= nrow(test.traits))
  try(db.close(con))
})

test_that("query.data works",{
  con <- check_db_test()
  # expect_true(nrow(query.data("SLA", "938")) > 0)
  expect_equal(nrow(query.data("xyz", "-1", con = con)), 0)
  # expect_true(nrow(query.data("LMA", spstr = vecpaste(1:20000))) > 1000)
  try(db.close(con))
})


context("test that expected tables exist") # modeltypes
expected_tables <- c("citations", "citations_sites", "citations_treatments", 
                     "covariates", "cultivars", "dbfiles", "ensembles", "entities",
                     "formats", "formats_variables", "inputs", "inputs_runs",
                     "likelihoods", "machines", "managements", "managements_treatments",
                     "methods", "mimetypes", "models", "pfts", "pfts_priors", "pfts_species", 
                     "posteriors", "priors", "runs", "schema_migrations", 
                     "sessions", "sites", "species", "traits", "treatments", "users", 
                     "variables", "workflows", "yields")
for (t in expected_tables){
  test_that(paste(t, "table exists and has >= 1 columns"),{
    tmp <- NULL
    con <- check_db_test()
    suppressWarnings(tmp <- db.query(paste("select * from", t, "limit 1"), con = con))
      #RyK added suppressWarnings for sites.geometry having unrecognized field type
    expect_false(is.null(tmp))
    try(db.close(con))
  })
}  

## the following suite could be more comprehensive, and only focus on fields used by PEcAn
test_that("database has a workflows table with appropriate columns",{
  con <- check_db_test()
  ## regression test for redmine #1128
  workflows <- db.query("select * from workflows;", con = con)
  if(nrow(workflows) >= 1){
    expect_true(all(c("id", "folder", "started_at", "finished_at", "created_at", 
                      "updated_at", "site_id", "model_id", "hostname", "params", "advanced_edit", 
                      "start_date", "end_date") %in% colnames(workflows)))    
  }
  try(db.close(con))

})

## the following suite could be more comprehensive, and only focus on fields used by PEcAn
test_that("sites have id and geometry column",{
  con <- check_db_test()
  ## regression test for redmine #1128
  sites <- suppressWarnings(db.query("select * from sites limit 1;", con = con)) 
    #RyK added suppressWarnings for geometry having unrecognized field type
  expect_true(all(c("id", "city", "state", "country", "mat", "map", "soil", "som", 
                    "notes", "soilnotes", "created_at", "updated_at", "sitename", 
                    "greenhouse", "user_id", "sand_pct", "clay_pct", 
                    "geometry","time_zone")
                  %in% colnames(sites)))
  try(db.close(con))
})

test_that("query.covariates returns expected data.frame",{
  con <- check_db_test()
  ids <- 1:10
  test.query <- query.covariates(ids, con = con)
  expect_equal(colnames(test.query), c("trait_id", "level", "name"))
  expect_true(nrow(test.query) >= length(ids))
  try(db.close(con))
})
