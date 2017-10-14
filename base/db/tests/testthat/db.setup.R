# Check if running on continuous integration
# If yes, skip this test
check_db_test <- function() {
    is_ci <- Sys.getenv('CI') != ''
    con <- NULL
    if (!is_ci) {
        try({
            if(PEcAn.remote::fqdn() == "pecan2.bu.edu") {
                con <- db.open(list(host="psql-pecan.bu.edu", driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))
            } else {
                con <- db.open(list(host="localhost", driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))
            }
        }, silent=T)
    }

    if(is.null(con)) {
        testthat::skip("Can't get a valid test connection right now. Skipping test. ")
    } else {
        return(con)
    }
}

