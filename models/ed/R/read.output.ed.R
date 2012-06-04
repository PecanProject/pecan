### TODO: Update this code.

#source('utils.R')
#source('model.specific.R')
#load('samples.Rdata')

read.output.ed <- function(){
  
  sensitivity.output <- list()
  ensemble.output    <- list()
  
  start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year),
                       NA, settings$sensitivity.analysis$start.year)
  end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year),
                       NA, settings$sensitivity.analysis$end.year)
  
  if('sensitivity.analysis' %in% names(settings)) {
    
    for(pft.name in names(trait.samples)){
      
      traits <- names(trait.samples[[pft.name]])
      quantiles.str <- rownames(sa.samples[[pft.name]])
      quantiles.str <- quantiles.str[which(quantiles.str != '50')]
      quantiles <- as.numeric(quantiles.str)/100
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits,
                                                       quantiles,
                                                       outdir = getwd(), 
                                                       pft.name=pft.name,
                                                       start.year,
                                                       end.year)
      save(sensitivity.output, file = 'output.Rdata')
      
    }
  }
  
  if('ensemble' %in% names(settings)) {
    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            outdir = getwd(), 
                                            start.year,
                                            end.year)
    save(ensemble.output, file = 'output.Rdata')
  }
  
  if(all(c('ensemble', 'sensitivity.analysis') %in% names(settings))) {
    save(ensemble.output, sensitivity.output, file = 'output.Rdata')
  }
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################