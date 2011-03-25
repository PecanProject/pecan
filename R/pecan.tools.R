save.object <- function(object, outdir=settings$outdir){
    save(object, file = paste(outdir, substitute(object), ".Rdata", sep = ""))
}

load.object <- function(objectname, outdir=settings$outdir){
    load(paste(outdir, objectname, ".Rdata", sep = ""))
}
