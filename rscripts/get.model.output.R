library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  options(error = browser)
  if(user == 'dlebauer'){
    settings.file = '~/in/ebifarm/prior/ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
  ## settings.file <- commandArgs(trailingOnly=TRUE)
} 

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

host<- settings$run$host
 

if(settings$run$host$name == 'localhost'){
  send.files <- function(x){
    file.copy(from = paste(settings$pecanDir,x, sep = ''),
              to   = settings$outdir,
              overwrite = TRUE)
  }
  lapply(c('R/utils.R', 'R/model.specific.R', 'rscripts/read.output.R', paste(settings$outdir, 'samples.Rdata', sep = '')), move.files)
  setwd(settings$outdir)
  source('read.output.R')
} else {
  move.files <- function(x){
    rsync(from = paste(settings$pecanDir, x, sep = ''),
          to   = paste(settings$run$host$name, ':',settings$run$host$outdir, sep = ''))
  }
  lapply(c('R/utils.R', 'R/model.specific.R', 'rscripts/read.output.R'), move.files)
  rsync(from = paste(settings$outdir, 'samples.Rdata', sep = ''),
        to = paste(settings$run$host$name, ':',settings$run$host$outdir, sep = ''))
  system(paste("ssh -T", settings$run$host$name, "'",
               "cd", settings$run$host$outdir, "; R --vanilla < read.output.R'"))
  
  rsync(from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'output.Rdata', sep=''),
        to = settings$outdir)
}
