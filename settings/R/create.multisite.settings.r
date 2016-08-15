##' @export
createMultiSiteSettings <- function(template.settings, site.ids, out.dir) {
  if(missing(out.dir)) {
    if(is.null(template.settings$outdir)) {
      warning("No out.dir set!")
      out.dir <- NULL
    } else {
      out.dir <- template.settings$outdir
    }
  }
  
  template.settings <- .set.out.dir(template.settings, out.dir)
  
  run.settings <- lapply(site.ids, .get.run.settings, template.settings=template.settings)
  
  template.settings[["run", global=FALSE]] <- run.settings
  return(template.settings)
}

##' @export
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

##' @export
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
