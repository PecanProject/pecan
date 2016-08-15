rm(list=ls())
library(devtools)

load_all('/fs/data2/rykelly/pecan/settings')

.get.run.settings <- function(template.settings, site.id) {
  start.date = template.settings$run$start.date
  end.date = template.settings$run$end.date
  inputs = template.settings$run$inputs
  return(list(
    site = list(
      id = site.id,
      met.start = start.date,
      met.end = end.date),
    start.date = start.date,
    end.date = end.date,
    inputs = inputs
  ))
}

.set.out.dir <- function(settings, out.dir) {
  settings$outdir <- out.dir 
  settings$rundir <- NULL
  settings$modeloutdir <- NULL
  settings$host$rundir <- NULL
  settings$host$outdir <- NULL
  settings$host$modeloutdir <- NULL
    
  for(j in 1:length(settings$pfts)) {
    settings$pfts[[j]]$outdir <- NULL
  }
  
  return(settings)
}

# --------------------
template.settings.file = '/fs/data2/rykelly/mandifore/testruns/015_mandSE_pecan.template.01.xml'
site.ids = c(1000003335, 1000003148)
out.dir = '/fs/data2/rykelly/mandifore/testruns/016_mandSE_test.01'


settings <- read.settings(template.settings.file)
  settings <- .set.out.dir(settings, out.dir)


run.settings <- lapply(site.ids, .get.run.settings, template.settings=settings)
settings[["run", global=FALSE]] <- run.settings


dir.create(out.dir, showWarnings=F)
saveXML(listToXml(settings, "pecan.multi"), file=file.path(out.dir, "pecan.xml"))

