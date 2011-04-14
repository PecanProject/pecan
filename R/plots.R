##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x.values trait quantiles used in sensitivity analysis
##' @param fun spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param prior.x.values if given, plots sensitivity for prior run 
##' @param prior.fun if given, plots sensitivity for prior run
##' @return object of class ggplot
plot.sensitivity <- function(x.values, fun, trait, prior.x.values = NA, prior.fun = NA) {
  print(x.values)
  LENGTH_OUT=100
  x <- seq(from = min(x.values), to = max(x.values), length.out = LENGTH_OUT)
  y <- fun(x)
  ylim = ceiling(max(y))
  line.data <- data.frame(x = x, y = y)
  point.data <- data.frame(x = x.values, y = fun(x.values))
  range(x)
  range(x.values)
  print(line.data)
  saplot <- ggplot() +
      geom_line(data = line.data, aes(x, y), size = 2) +
      geom_point(data = point.data, aes(x, y), size = 3) +
      scale_y_continuous(limits = c(0,ylim), breaks = seq(0, ylim, 0.5)) +
      theme_bw() +
      opts(title=trait.dictionary(trait)$figid,
          axis.text.x = theme_text(size=14),
          axis.text.y = theme_text(size=14),
          axis.title.x = theme_blank(), 
          axis.title.y = theme_blank(),
          plot.title = theme_text(size = 20),
          panel.border = theme_blank())
  if(!is.na(prior.x.values) & !is.na(prior.fun)){
    x <- seq(from = min(prior.x.values), to = max(prior.x.values), length.out = LENGTH_OUT)
    saplot <- saplot +
        geom_line(aes(x, fun(x)), size = 2, color = 'grey') +
        geom_point(aes(x.values, fun(x.values)), size = 3, color = 'grey')
  }
  return(saplot)
}

#        scale_x_continuous(limits = ifelse(diff(range(x))/min(x) < 0.25 ,
#                             c(0, max(x) * 1.1),
#                             c(0.9, 1.1) * range(x))) 
#                            
#                               coord_cartesian(x=c(0, max(x)*1.1))


## geom_line(aes(x, y),
##           color = 'grey',
##           size = 2,
##           data = lpr ) +
##             geom_point(aes(x,y),
##                        data = dpr,
##                        color = 'grey',
##                        size = 3) +
##                          geom_line(aes(x, y),                                                       
##                                    color = 'black',
##                                    size = 2,
##                                    data = lpo) +
##                                      geom_point(aes(x,y),
##                                                 data=dpo,
##                                                 color = 'black',
##                                                 size = 3) +
##                                                   geom_point(aes(x,y), data= dpr[4,], color = 'grey', size = 5) +
##                                                     geom_point(aes(x,y), data= dpo[4,], color = 'black', size = 5) + scale_shape(solid=FALSE) +
plot.variance.decomposition <- function(coef.vars, elasticities, explained.variances, outdir){
  traits<-names(explained.variances)
  
  coef.var.plot <- qplot(traits, coef.vars, log='y')
  
  elasticity.plot <- qplot(traits, elasticities, xlab='')
  elasticity.min <-min(elasticities[elasticities>1e-10])
  if(log10(elasticity.min)+1 < log10(max(elasticities))){
    elasticity.plot <- elasticity.plot + scale_y_log10(limits=c(elasticity.min, max(elasticities)))
  }
  
  explained.var.plot <- qplot(traits, explained.variances, xlab='') + scale_y_log10(limits=c(1e-8, 1))
  
  ## stand in to be replaced by plot used in publication
  pdf(paste(outdir, 'variancedecomposition.pdf', sep=''), width = 12, height = 8)
  grid.arrange(coef.var.plot + coord_flip(),
      elasticity.plot + coord_flip(),
      explained.var.plot + coord_flip(),
      ncol = 3)
  dev.off()
}

plot.sensitivities <- function(sa.samples, sa.splinefuns, outdir){
  traits<-names(sa.samples)
  pdf(paste(outdir, 'sensitivity.analysis.pdf', sep=''), height = 12, width = 20)
  for(trait in traits)
    print(plot.sensitivity(sa.samples[[trait]], sa.splinefuns[[trait]], trait))
  dev.off()
}

