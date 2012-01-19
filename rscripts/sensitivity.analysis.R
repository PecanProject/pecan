library(XML)
if(interactive()){
  user <- system('echo $USER', intern = TRUE)
  options(error = browser)
  if(user == 'dlebauer'){
    settings.file = '~/in/ebifarm/post/ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/toolik.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- commandArgs(trailingOnly=TRUE)[[1]]
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

load(paste(settings$outdir, 'output.Rdata', sep=''))
sensitivity.output<-sa.agb
load(paste(settings$outdir, 'samples.Rdata', sep=''))

initial.analysis<-read.csv('initial.analysis.csv')
initial.analysis$partial.variances <- 10^-initial.analysis$partial.variances
initial.analysis$elasticities[initial.analysis$pft!='tundra.grass'] <- 10^-(initial.analysis$elasticities[initial.analysis$pft!='tundra.grass']+1)
initial.analysis$coef.vars<-10^-(initial.analysis$coef.vars)
initial.analysis$coef.vars[initial.analysis$trait=='Vm_low_temp'] <- get.coef.var(rnorm(10000, -3, 2))
initial.analysis$coef.vars[initial.analysis$trait=='quantum_efficiency'] <- get.coef.var(rnorm(10000, 0.0600, 0.0100))

for(pft in settings$pfts){

  
  prior.decomp <- list()
  if('sensitivity.analysis' %in% names(settings)) {

    quantiles.str <- rownames(sa.samples[[pft$name]])
    quantiles.str <- quantiles.str[which(quantiles.str != '50')]
    quantiles <- as.numeric(quantiles.str)/100
    ## ensemble.output <- read.ensemble.output(settings$ensemble$size, settings$outdir, pft.name=pft$name)

    ## only perform sensitivity analysis on traits where no more than 2 results are missing
    traits <- names(trait.samples[[pft$name]])
    good.saruns <- sapply(sensitivity.output[[pft$name]], function(x) sum(is.na(x)) <=2)
    if(!all(good.saruns)) { # if any bad saruns, reduce list of traits and print warning
      bad.saruns <- !good.saruns
      warning(paste('missing >2 runs for', vecpaste(traits[bad.saruns]),
                    '\n sensitivity analysis or variance decomposition will be performed on these trait(s)',
                    '\n it is likely that the runs did not complete, this should be fixed !!!!!!'))
    }
    browser()
    sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[pft$name]][traits],
                                                sa.samples = sa.samples[[pft$name]][ ,traits],
                                                sa.output = sensitivity.output[[pft$name]][ ,traits],
                                                outdir = pft$outdir)
    save(sensitivity.results, file=paste(pft$outdir, 'sensitivity.results.Rdata', sep=''))
    initial.analysis.pft <- initial.analysis[initial.analysis$pft==pft$name,]
    
    initial.partial.variances <- initial.analysis.pft$partial.variances
    names(initial.partial.variances) <- initial.analysis.pft$trait
    
    initial.elasticities <- initial.analysis.pft$elasticities
    names(initial.elasticities) <- initial.analysis.pft$trait
    foo<-intersect(names(initial.elasticities), traits)
    initial.elasticities[foo] <- sensitivity.results$variance.decomposition.plot.inputs$elasticities[foo]
    
    initial.coef.vars <- initial.analysis.pft$coef.vars
    names(initial.coef.vars) <- initial.analysis.pft$trait
    
    
    initial.decomp <- list(coef.vars=initial.coef.vars,
        elasticities = initial.elasticities,
        partial.variances = initial.partial.variances)
"    sensitivity.plots <- plot.sensitivities(sensitivity.results$sensitivity.plot.inputs,
                                   linesize = 1,
                                   dotsize = 3)
    pdf(paste(settings$outdir, 'sensitivityanalysis.pdf', sep = ''), height = 12, width = 9)
    sensitivity.plots
    dev.off()"

    pdf(paste(pft$outdir, 'variancedecomposition.pdf', sep=''), width = 11, height = 8)

    vd.plots <- plot.variance.decomposition(sensitivity.results$variance.decomposition.plot.inputs,
        prior.plot.inputs = initial.decomp)
    cv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$coef.vars*100,4)
    el.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,4)
    el.xrange <- range(pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,6))
    pv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$partial.variances*100,4)
    do.call(grid.arrange, c(vd.plots, ncol = 4))
    
    vd.plots <- plot.variance.decomposition(sensitivity.results$variance.decomposition.plot.inputs)
    cv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$coef.vars*100,4)
    el.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,4)
    el.xrange <- range(pretty(sensitivity.results$variance.decomposition.plot.inputs$elasticities,6))
    pv.xticks <- pretty(sensitivity.results$variance.decomposition.plot.inputs$partial.variances*100,4)
    do.call(grid.arrange, c(vd.plots, ncol = 4))
    
    vd.plots <- plot.variance.decomposition(initial.decomp)
    cv.xticks <- pretty(initial.decomp$coef.vars*100,4)
    el.xticks <- pretty(initial.decomp$elasticities,4)
    el.xrange <- range(pretty(initial.decomp$elasticities,6))
    pv.xticks <- pretty(initial.decomp$partial.variances*100,4)
    do.call(grid.arrange, c(vd.plots, ncol = 4))

    dev.off() 
  }
}

