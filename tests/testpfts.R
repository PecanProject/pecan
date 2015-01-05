require(PEcAn.utils)
require(PEcAn.DB)
require(PEcAn.MA)
require(RPostgreSQL)

runmeta <- function(pftid, pftname, model, dbparam) {
  folder <- file.path(dbparam$dbfiles, "pfts", model, pftname)
  unlink(folder, recursive=TRUE, force=TRUE)
  dir.create(folder, recursive=TRUE)
  pft <- list(name=pftname, outdir=folder)
  cat(paste0("TESTING [id=", pftid, " pft='", pftname, "' model='",  model, "'] : get.traits\n"), file=stderr())
  pfts <- get.trait.data(list(pft), model, dbparam$dbfiles, dbparam, TRUE)
  cat(paste0("TESTING [id=", pftid, " pft='", pftname, "' model='",  model, "'] : meta.analysis\n"), file=stderr())
  run.meta.analysis(pfts, 3000, FALSE, dbparam$dbfiles, dbparam)
  cat(paste0("TESTING [id=", pftid, " pft='", pftname, "' model='",  model, "'] : OK\n"), file=stderr())
}

testpft <- function(pftid, pftname, model, dbparam) {
  tryCatch(runmeta(pftid, pftname, model, dbparam),
           error=function(e) {
             cat(paste0("TESTING [id=", pftid, " pft='", pftname, "' model='",  model, "'] : BROKEN - ", e$message, "\n"), file=stderr())
             for(con in dbListConnections(dbDriver("PostgreSQL"))) {
               db.close(con)
             }
           })
}

dbparam <- list(dbname="bety", user="bety", password="bety", dbfiles="testpfts", write=FALSE, driver="PostgreSQL")
pfts <- db.query("SELECT pfts.id AS id, pfts.name AS pft, modeltypes.name AS model FROM pfts, modeltypes WHERE pfts.modeltype_id=modeltypes.id ORDER BY id;", param=dbparam)

options(scipen=12)
apply(pfts, 1, function(x) { testpft(x[[1]], x[[2]], x[[3]], dbparam) })
