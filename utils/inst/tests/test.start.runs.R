


### Redmine issue #1565 error trying to keep a database connection open ...
test_that("mysql connection is not closed if a start.runs.test opens/closes a connection",{
  settings <- read.settings(system.file("test.settings.xml",
                                        package = "PEcAn.utils"))
  start.runs.TEST <<- function(runid){
    testcon <- query.base.con(settings)
    query.base("show tables;", testcon)
    query.close(testcon)
  }
  start.model.runs("TEST")  
  
  start.runs.TEST2 <- function(runid){
    query.base("show tables;")
  }
})
