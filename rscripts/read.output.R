source('utils.R')
source('model.specific.R')
load('samples.Rdata')
sensitivity.output <- list()
ensemble.output    <- list()
for(pft.name in names(trait.samples)){
  
  traits <- names(trait.samples[[pft.name]])
  quantiles.str <- rownames(sa.samples[[pft.name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100

  start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)

  if('sensitivity.analysis' %in% names(settings)) {
    sensitivity.output[[pft.name]] <- read.sa.output(traits,
                                                     quantiles,
                                                     outdir = getwd(), 
                                                     pft.name=pft.name,
                                                     start.year,
                                                     end.year)
    save(sensitivity.output, file = 'output.Rdata')

  }

  if('ensemble' %in% names(settings)) {
    ensemble.output[[pft.name]] <- read.ensemble.output(settings$ensemble$size,
                                                        outdir = getwd(), 
                                                        pft.name=pft.name,
                                                        start.year,
                                                        end.year)
    save(ensemble.output, file = 'output.Rdata')
  }
  if(all(c('ensemble', 'sensitivity.analysis') %in% names(settings))) {
    save(ensemble.output, sensitivity.output, file = 'output.Rdata')
  }
}
