save.object <- function(object, outdir=settings$outdir){
    save(object, file = paste(outdir, substitute(object), ".Rdata", sep = ""))
}

load.object <- function(objectname, outdir=settings$outdir){
    load(paste(outdir, objectname, ".Rdata", sep = ""))
}

settings <- function(){
  library(XML)
  if(interactive()){
    settings.file = '~/pecan/tundra.xml'
  } else {
    settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
  }
  settings.xml <- xmlTreeParse(settings.file)
  settings <- xmlToList(settings.xml)
  return(settings)
}
