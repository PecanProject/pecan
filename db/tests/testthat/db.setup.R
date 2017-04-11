con <- NULL
try({
    if(fqdn() == "pecan2.bu.edu") {
        con <- db.open(list(host="psql-pecan.bu.edu", driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))
    } else {
        con <- db.open(list(host="localhost", driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety"))
    }
}, silent=T)

if(is.null(con)) {
    skip("Can't get a valid test connection right now. Skipping test. ")
}
