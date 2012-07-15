## This won't work for me remotely.  We need to figure out how to connect remotely for this test.
#test_that("query.base does not keep opening connections without closing them", {
#  newcon <- function() dbConnect("MySQL", group = "ebi_analysis", dbname = "ebi_analysis", password = "b742xsAu", username = "ebi_analys_user", host = 'ebi-forecast.igb.uiuc.edu', port=3306)
#  for (i in 1:17) query.base("select count(*) from priors;", con = newcon())
#})
