save.objects <- function(objects, outdir=settings$outdir){
  for (object in objects){
    save(object, file = paste(outdir, substitute(object), ".Rdata", sep = ""))
  }   
}

load.objects <- function(objects, outdir=settings$outdir){
  for (object in objects){
    load(paste(outdir, substitute(object), ".Rdata", sep = ""))
  }
}
