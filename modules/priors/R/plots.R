##--------------------------------------------------------------------------------------------------#
##' Plots a prior density from a parameterized probability distribution  
##'
##' @name plot.prior.density
##' @title Add Prior Density
##' @param prior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @param prior.color color of line to be plotted
##' @return plot with prior density added
##' @seealso \code{\link{pr.dens}}
##' @author David LeBauer
##' @export
##' @examples
##' plot.prior.density(pr.dens('norm', 0, 1))
plot.prior.density <- function(prior.density, base.plot = NULL, prior.color = 'black' ) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  new.plot <- base.plot +  geom_line(data = prior.density,
                                     aes(x = x, y = y),
                                     color = prior.color)
  return(new.plot)
}

##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##'  Add posterior density to a plot
##'
##' @name plot.posterior.density
##' @title Add posterior density. 
##' @param posterior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @return plot with posterior density line added
##' @export
##' @author David LeBauer
plot.posterior.density <- function(posterior.density, base.plot = NULL) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  new.plot <- base.plot +  geom_line(data = posterior.density,
                                     aes(x = x, y = y))
  return(new.plot)  
}
##==================================================================================================#

##--------------------------------------------------------------------------------------------------#
##' Plot prior density and data
##'
##' @name priorfig
##' @title Prior Figure 
##' @param priordata observations to be plotted as points
##' @param priordensity density of prior distribution, calculated by \code{\link{prior.density}}
##' @param trait name of trait
##' @param xlim limits for x axis
##' @author David LeBauer
##' @return plot / grob of prior distribution with data used to inform the distribution 
priorfig <- function(priordata = NA, priordensity = NA,
                     trait = '', xlim = 'auto', fontsize = 18){
  if(is.data.frame(priordata)){
    colnames(priordata) <- 'x'
  }
  
  if(isTRUE(xlim == 'auto')) {
    x.breaks <- pretty(c(signif(priordensity$prior.x, 2)), 4)
    xlim <- range(x.breaks)
  } else {
    x.breaks <- pretty(signif(xlim, 2), 4)
    xlim <- range(c(x.breaks, xlim))
  }
  
  priorfigure <- ggplot() + theme_bw() + 
    scale_x_continuous(limits = xlim, breaks = x.breaks, trait.lookup(trait)$units) +
    scale_y_continuous(breaks = NULL)+
    opts(title = trait.lookup(trait)$figid,
         panel.grid.major = theme_blank(),    
         panel.grid.minor = theme_blank(),
         axis.text.y = theme_blank(),
         axis.text.x = theme_text(size= fontsize),
         axis.title.y = theme_blank(), ## hide y axis label
         axis.title.x = theme_text(size = fontsize * 0.9), ## hide y axis label
         plot.title = theme_text(size = fontsize*1.1),
         ## plot.margin = unit(c(0.4, 0.1, 0.1, 0.1), 'lines'), 
         panel.border = theme_border(c('bottom'))
    ) 
  
  
  if(is.data.frame(priordata)){
    priordata <- subset(priordata, subset = !is.na(x))
    dx <- with(priordata,
               min(abs(diff(x)[diff(x)!=0])))
    priordata <- transform(priordata,
                           ## add jitter to separate equal values 
                           x = x + runif(length(x), -dx/2, dx/2))
    rug <- geom_rug(data = priordata, aes(x))
    priorfigure <- priorfigure + rug
  } 
  if(is.data.frame(priordensity[1])){
    dens.line <- geom_line(data=priordensity, aes(x=prior.x, y=dens.x))
    qpts <- get.quantiles.from.density(priordensity)
    dens.ci <- geom_point(data = qpts, aes(x,y))
    priorfigure <- priorfigure + dens.line + dens.ci
  }
  return(priorfigure)
} 
##==================================================================================================#

##--------------------------------------------------------------------------------------------------#
##' Plot trait density and data
##'
##' @name plot.trait
##' @title Plot trait density
##' @param trait character, name of trait to be plotted
##' @param prior named distribution with parameters
##' @param posterior.sample 
##' @param trait.data 
##' @param fontsize 
##' @return plot (grob) object
##' @author David LeBauer
##' @export
##' @examples
##' \dontrun{
##' prior1 <- data.frame(distn = 'norm',
##'                      parama = 20,
##'                      paramb = 5)
##' data1  <- data.frame(Y = c(19, 21), se = c(1,1))
##' plot.trait(trait = 'Vcmax',
##'           prior = prior1,
##'           trait.df = data1)
##' }
plot.trait <- function(trait,
                       prior = NULL,
                       posterior.sample = NULL,
                       trait.df = NULL,
                       fontsize = list(title = 18, axis = 14),
                       x.lim = NULL,
                       y.lim = NULL,
                       logx = FALSE) {
  ## Determine plot components
  plot.posterior <- !is.null(posterior.sample)
  plot.prior     <- !is.null(prior)
  plot.data      <- !is.null(trait.df)
  
  ## If no posterior, set prior density to black
  
  ## get units for plot title
  units <- trait.lookup(trait)$units
  
  prior.density <- posterior.density <- data.frame(x = NA, y = NA)
  if(plot.prior){
    prior.color = 'black'
    prior.density   <- create.density.df(distribution = prior)
    prior.density <- prior.density[prior.density$x > 0, ]
  }
  if(plot.posterior){
    posterior.density <- create.density.df(samps = posterior.sample)
    posterior.density <- posterior.density[posterior.density$x > 0, ]
    prior.color <-  'grey'
  }
  
  if(is.null(x.lim)){
    if(!is.null(trait.df)){
      data.range <- max(c(trait.df$Y, trait.df$Y + trait.df$se), na.rm = TRUE)
    } else {
      data.range <- NULL
    }
    x.lim <- range(c(posterior.density$x, prior.density$x, data.range), na.rm = TRUE)
  }
  if(is.null(y.lim)){
    y.lim <- range(posterior.density$y, prior.density$y, na.rm = TRUE)
  }
  print(trait)
  print(x.lim)
  print(y.lim)
  ticks <<- lapply(data.frame(x = x.lim, y = y.lim), pretty, 4)
  
  print(ticks)
  x.lim <<- range(ticks$x)
  y.lim <<- range(ticks$y) 
  
  base.plot <- create.base.plot() + theme_bw()
  prior.density   <- create.density.df(distribution = prior) 
  prior.ymax      <- ifelse(plot.prior,
                            max(prior.density$y),
                            NA)
  posterior.ymax  <- ifelse(plot.posterior,
                            max(posterior.density$y),
                            NA)
  ymax           <- max(prior.ymax, posterior.ymax, na.rm = TRUE) 
  
  prior.xmax     <- ifelse(plot.prior,
                           quantile(prior.density$x, 0.999),
                           NA)
  posterior.xmax <- ifelse(plot.posterior,
                           quantile(posterior.density$x, 0.999),
                           NA)
  xmax           <- max(prior.xmax, posterior.xmax, na.rm = TRUE) 
  x.ticks        <- pretty(c(0,xmax))
  y.ticks        <- pretty(c(0,ymax))
  x.ticklength   <- max(y.ticks)/50
  
  browser()
  if(plot.prior){
    keep <- (prior.density$x > x.lim[1] &
               prior.density$x < x.lim[2] &
               prior.density$y > y.lim[1] &
               prior.density$y < y.lim[2])
    
    prior.density <- prior.density[keep,]
    base.plot <- plot.prior.density(prior.density,
                                    base.plot = base.plot,
                                    prior.color = prior.color)
  }
  if(plot.posterior){
    keep <- (posterior.density$x > x.lim[1] &
               posterior.density$x < x.lim[2] &
               posterior.density$y > y.lim[1] &
               posterior.density$y < y.lim[2])
    posterior.density <- posterior.density[keep, ]
    base.plot <- plot.posterior.density(posterior.density,
                                        base.plot = base.plot)
  }
  if(plot.data){
    base.plot <- plot.data(trait.df,
                           base.plot = base.plot,
                           ymax = y.lim[2])
  }
  
  trait.plot <- base.plot +
    opts(title= trait.lookup(trait)$figid, 
         axis.text.x = theme_text(size = fontsize$axis),
         axis.text.y = theme_blank(),
         axis.title.x = theme_text(size = fontsize$axis),
         axis.title.y = theme_blank(),
         axis.ticks = theme_blank(),
         axis.line = theme_blank(),
         legend.position = "none",
         plot.title = theme_text(size = fontsize$title),
         panel.grid.major = theme_blank(),
         panel.grid.minor = theme_blank(),
         panel.border = theme_blank()) +
    geom_segment(aes(x = 0, y = 0, yend = 0, xend = x.lim[2])) +
    geom_segment(aes(x = pretty(x.lim, 4),
                     y = 0,
                     xend = pretty(x.lim, 4),
                     yend = -y.lim[2]/50)) 
  return(trait.plot)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Plot probability density and data
##'
##' @name plot.densities
##' @title Plot Trait Probability Densities
##' @param sensitivity.results list containing sa.samples and sa.splines 
##' @param outdir
##' @author David LeBauer
##' @return outputs plots in outdir/sensitivity.analysis.pdf file 
plot.densities <- function(density.plot.inputs, outdir, ...){
  trait.samples          <- density.plot.inputs$trait.samples
  trait.df               <- density.plot.inputs$trait.df
  prior.trait.samples    <- density.plot.inputs$trait.df
  
  traits <- names(trait.samples)
  pdf(paste(outdir, 'trait.densities.pdf', sep=''), height = 12, width = 20)
  
  for(trait in traits) {
    density.plot <- plot.density(trait.sample       =  trait.samples[,trait],
                                 trait.df         = trait.df[[trait]],
                                 ...)
    print(sensitivity.plot)
  }
  dev.off()
}
##==================================================================================================#



##--------------------------------------------------------------------------------------------------#
##' Find quantiles on a density data frame
##'
##' @name get.quantiles.from.density
##' @title Get the quantiles from prior density
##' @param priordensity density dataframe generated by \code{\link{create.density.df}}
##' @param quantiles default is the median and 95% CI;
##' @author David LeBauer
##' @export
##' @examples
##' prior.df <- create.density.df(distribution = list('norm',0,1))
##' get.quantiles.from.density(prior.df)
##' samp.df <- create.density.df(samps = rnorm(100))
##' get.quantiles.from.density(samp.df)
get.quantiles.from.density <- function(density.df, quantiles = c(0.025, 0.5, 0.975)){
  colnames(density.df) <- c('prior.x', 'dens.x')
  density.df$prob.x <- density.df$dens.x/sum(density.df$dens.x)
  qi <- sapply(quantiles, function(x) which.min(abs(cumsum(density.df$prob.x)- x)))
  qs <- density.df[qi,c('prior.x', 'dens.x')]
  colnames(qs) <- c('x', 'y')
  return(qs)
}
##==================================================================================================#
