library(XML)
if(interactive()){
  user <- system('echo $USER', intern = TRUE)
  options(error = browser)
  if(user == 'dlebauer'){
    settings.file = '~/pecan/2011.07.18/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
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

outdir <- settings$outdir
host<- settings$run$host
load(paste(outdir, 'samples.Rdata', sep=''))

ssh(host$name, 'cd ', host$outdir, '/ ; R --vanilla ',
    args=paste('<', settings$pecanDir, '/rscripts/read.output.R',sep=''))
rsync(paste(host$name, ':', host$outdir, '/output.Rdata', sep=''),
      outdir)

load(paste(outdir, 'output.Rdata', sep=''))

for(pft in settings$pfts){
  traits <- names(trait.samples[[pft$name]])
  quantiles.str <- rownames(sa.samples[[pft$name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100
                                        #ensemble.output <- read.ensemble.output(settings$ensemble$size, outdir, pft.name=pft$name)
  
  if('sensitivity.analysis' %in% names(settings)) {
    sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[pft$name]],
                                                sa.samples = sa.samples[[pft$name]],
                                                sa.output = sa.agb[[pft$name]],
                                                outdir = pft$outdir)
    sa.plots <- plot.sensitivities(sensitivity.results$sensitivity.plot.inputs)
    pdf(paste(pft$outdir, 'sensitivityanalysis.pdf', sep = ''), height = 12, width = 9)
    do.call(grid.arrange, c(sa.plots, nrow = 4, ncol = ceiling(length(traits)/4)))
    dev.off()

    vd.plots <- plot.variance.decomposition(sensitivity.results$variance.decomposition.plot.inputs)
    pdf(paste(pft$outdir, 'variancedecomposition.pdf', sep=''), width = 11, height = 8)
    do.call(grid.arrange, c(vd.plots, ncol = 4))
    grid.edit(gPath("axis_v", "axis.ticks"), grep = TRUE, gp = gpar(col = 'white'))
    dev.off()
  
  }
}

