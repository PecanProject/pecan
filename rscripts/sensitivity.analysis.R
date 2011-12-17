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

load(paste(settings$outdir, 'output.Rdata', sep=''))
load(paste(settings$outdir, 'samples.Rdata', sep=''))

if('sensitivity.analysis' %in% names(settings)) {
  for(pft in settings$pfts){
    print(pft)

    traits <- names(trait.samples[[pft$name]])
    quantiles.str <- rownames(sa.samples[[pft$name]])
    quantiles.str <- quantiles.str[which(quantiles.str != '50')]
    quantiles <- as.numeric(quantiles.str)/100
    ## ensemble.output <- read.ensemble.output(settings$ensemble$size, settings$outdir, pft.name=pft$name)

    ## only perform sensitivity analysis on traits where no more than 2 results are missing
    good.saruns <- sapply(sensitivity.output[[pft$name]], function(x) sum(is.na(x)) <=2)
    if(!all(good.saruns)) { # if any bad saruns, reduce list of traits and print warning
      bad.saruns <- !good.saruns
      warning(paste('missing >2 runs for', vecpaste(traits[bad.saruns]),
                    '\n sensitivity analysis or variance decomposition will be performed on these trait(s)',
                    '\n it is likely that the runs did not complete, this should be fixed !!!!!!'))
    }
    
    sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[pft$name]][traits],
                                                sa.samples = sa.samples[[pft$name]][ ,traits],
                                                sa.output = sensitivity.output[[pft$name]][ ,traits],
                                                outdir = pft$outdir)
    sensitivity.plots <- plot.sensitivities(sensitivity.results$sensitivity.plot.inputs,
                                   linesize = 1,
                                   dotsize = 3)
    pdf(paste(pft$outdir, 'sensitivityanalysis.pdf', sep = ''), height = 12, width = 9)
    print(sensitivity.plots)
    dev.off()

    vd.plots <- plot.variance.decomposition(sensitivity.results$variance.decomposition.plot.inputs)
    pdf(paste(pft$outdir, 'variancedecomposition.pdf', sep=''), width = 11, height = 8)
    cv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$coef.vars*100,4)
    el.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,4)
    el.xrange <- range(pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,6))
    pv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$partial.variances*100,4)
    do.call(grid.arrange, c(vd.plots, ncol = 4))
    dev.off() 
  }
}  ## end if sensitivity analysis
