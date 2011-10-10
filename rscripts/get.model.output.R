library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  options(error = browser)
  if(user == 'dlebauer'){
    settings.file = '~/out/2011.08.26/settings.ebifarm.pavi.xml'
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

outdir <- settings$outdir
host<- settings$run$host
 

if(host$name == 'localhost'){
  file.copy(from = paste(settings$pecanDir, '/rscripts/read.output.R', sep = ''),
            to   = outdir,
            overwrite = TRUE)
  setwd(outdir)
  source('read.output.R')
} else {
  rsync(from = paste(settings$pecanDir, '/rscripts/read.output.R ', sep = ''),
        to   = paste(host$name, ':',host$outdir, sep = ''))
  system(paste("ssh -T", host$name, "'",
               "cd", host$outdir, "; R --vanilla < read.output.R'"))
  
  rsync(from = paste(host$name, ':', host$outdir, '/output.Rdata', sep=''),
        to = outdir)
}
