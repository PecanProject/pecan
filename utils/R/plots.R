#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##' Variable-width (dagonally cut) histogram
##'
##' 
##' When constructing a histogram, it is common to make all bars the same width.
##' One could also choose to make them all have the same area.
##' These two options have complementary strengths and weaknesses; the equal-width histogram oversmooths in regions of high density, and is poor at identifying sharp peaks; the equal-area histogram oversmooths in regions of low density, and so does not identify outliers.
##' We describe a compromise approach which avoids both of these defects. We regard the histogram as an exploratory device, rather than as an estimate of a density. 
##' @name dhist
##' @title Diagonally Cut Histogram 
##' @param x is a numeric vector (the data)
##' @param a is the scaling factor, default is 5 * IQR
##' @param nbins is the number of bins, default is assigned by the Stuges method
##' @param rx  is the range used for the left of the left-most bin to the right of the right-most bin  
##' @param eps used to set artificial bound on min width / max height of bins as described in Denby and Mallows (2009) on page 24.
##' @param xlab is label for the x axis 
##' @param plot = TRUE produces the plot, FALSE returns the heights, breaks and counts
##' @param lab.spikes = TRUE labels the \% of data in the spikes
##' @return list with two elements, heights of length n and breaks of length n+1 indicating the heights and break points of the histogram bars. 
##' @author Lorraine Denby, Colin Mallows
##' @references Lorraine Denby, Colin Mallows. Journal of Computational and Graphical Statistics. March 1, 2009, 18(1): 21-31. doi:10.1198/jcgs.2009.0002.
dhist <- function(x, a=5*iqr(x),
                nbins=nclass.Sturges(x), rx = range(x,na.rm = TRUE),
                eps=.15, xlab = "x", plot = TRUE,lab.spikes = TRUE)
{

  if(is.character(nbins))
    nbins <- switch(casefold(nbins),
                    sturges = nclass.Sturges(x),
                    fd = nclass.FD(x),
                    scott = nclass.scott(x),
                    stop("Nclass method not recognized"))
  else if(is.function(nbins))
    nbins <- nbins(x)

  x <- sort(x[!is.na(x)])
  if(a == 0)
    a <- diff(range(x))/100000000
  if(a != 0 & a != Inf) {
    n <- length(x)
    h <- (rx[2] + a - rx[1])/nbins
    ybr <- rx[1] + h * (0:nbins)
    yupper <- x + (a * (1:n))/n
                                        # upper and lower corners in the ecdf
    ylower <- yupper - a/n
                                        #
    cmtx <- cbind(cut(yupper, breaks = ybr), cut(yupper, breaks = 
                                ybr, left.include = TRUE), cut(ylower, breaks = ybr),
                  cut(ylower, breaks = ybr, left.include = TRUE))
    cmtx[1, 3] <- cmtx[1, 4] <- 1
                                        # to replace NAs when default r is used
    cmtx[n, 1] <- cmtx[n, 2] <- nbins
                                        #
                                        #checksum <- apply(cmtx, 1, sum) %% 4
    checksum <- (cmtx[, 1] + cmtx[, 2] + cmtx[, 3] + cmtx[, 4]) %%
    4
                                        # will be 2 for obs. that straddle two bins
    straddlers <- (1:n)[checksum == 2]
                                        # to allow for zero counts
    if(length(straddlers) > 0) {
      counts <- table(c(1:nbins, cmtx[ - straddlers, 1]))
    } else {
      counts <- table(c(1:nbins, cmtx[, 1]))
    }
    counts <- counts - 1
                                        #
    if(length(straddlers) > 0) {
      for(i in straddlers) {
        binno <- cmtx[i, 1]
        theta <- ((yupper[i] - ybr[binno]) * n)/a
        counts[binno - 1] <- counts[binno - 1] + (
                                                  1 - theta)
        counts[binno] <- counts[binno] + theta
      }
    }
    xbr <- ybr
    xbr[-1] <- ybr[-1] - (a * cumsum(counts))/n
    spike<-eps*diff(rx)/nbins
    flag.vec<-c(diff(xbr)<spike,F)
    if ( sum(abs(diff(xbr))<=spike) >1) {
      xbr.new<-xbr
      counts.new<-counts
      diff.xbr<-abs(diff(xbr))
      amt.spike<-diff.xbr[length(diff.xbr)]
      for (i in rev(2:length(diff.xbr))) {
        if (diff.xbr[i-1] <= spike&diff.xbr[i] <= spike &
            !is.na(diff.xbr[i])) {
          amt.spike <- amt.spike+diff.xbr[i-1]
          counts.new[i-1] <- counts.new[i-1]+counts.new[i]
          xbr.new[i] <- NA
          counts.new[i] <- NA
          flag.vec[i-1] <- T
        }
        else amt.spike<-diff.xbr[i-1]
      }
      flag.vec<-flag.vec[!is.na(xbr.new)]
      flag.vec<-flag.vec[-length(flag.vec)]
      counts<-counts.new[!is.na(counts.new)]
      xbr<-xbr.new[!is.na(xbr.new)]

    }
    else flag.vec<-flag.vec[-length(flag.vec)]
    widths <- abs(diff(xbr))
    ## N.B. argument "widths" in barplot must be xbr
    heights <- counts/widths
  }
  bin.size <- length(x)/nbins
  cut.pt <- unique(c(min(x) - abs(min(x))/1000,
                     approx(seq(length(x)), x, (1:(nbins - 1)) * bin.size, rule = 2)$y, max(x)))
  aa <- hist(x, breaks = cut.pt, plot = FALSE, probability = TRUE)
  if(a == Inf) {
    heights <- aa$counts
    xbr <- aa$breaks
  }
  amt.height<-3
  q75<-quantile(heights,.75)
  if (sum(flag.vec)!=0) {
    amt<-max(heights[!flag.vec])
    ylim.height<-amt*amt.height
    ind.h<-flag.vec&heights> ylim.height
    flag.vec[heights<ylim.height*(amt.height-1)/amt.height]<-F
    heights[ind.h] <- ylim.height
  }
  amt.txt<-0
  end.y<-(-10000)
  if(plot) {
    barplot(heights, abs(diff(xbr)), space = 0, density = -1, xlab = 
            xlab, plot = TRUE, xaxt = "n",yaxt='n')
    at <- pretty(xbr)
    axis(1, at = at - xbr[1], labels = as.character(at))
    if (lab.spikes) {
      if (sum(flag.vec)>=1) {
        usr<-par('usr')
        for ( i in seq(length(xbr)-1)) {
          if (!flag.vec[i]) {
            amt.txt<-0
            if (xbr[i]-xbr[1]<end.y) amt.txt<-1
          }
          else {
            amt.txt<-amt.txt+1
            end.y<-xbr[i]-xbr[1]+3*par('cxy')[1]
          }
          if (flag.vec[i]) {
            txt<-paste(' ',format(round(counts[i]/
                                        sum(counts)*100)),'%',sep='')
            par(xpd = TRUE)
            text(xbr[i+1]-xbr[1],ylim.height-par('cxy')[2]*(amt.txt-1),txt, adj=0)
          }}
      }
      else print('no spikes or more than one spike')
    }
    invisible(list(heights = heights, xbr = xbr))
  }
  else {
    return(list(heights = heights, xbr = xbr,counts=counts))
  }
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculate interquartile range
##'
##' Calculates the 25th and 75th quantiles given a vector x; used in function \link{dhist}.
##' @name iqr
##' @title Interquartile range
##' @param x vector
##' @return numeric vector of length 2, with the 25th and 75th quantiles of input vector x. 
iqr <- function(x){
  return(diff(quantile(x, c(0.25, 0.75), na.rm = TRUE)))
}
##==================================================================================================#

##--------------------------------------------------------------------------------------------------#
##' Creates empty ggplot object
##'
##' An empty base plot to which layers created by other functions
##' (\code{\link{add.data}}, \code{\link{add.prior.density}},
##' \code{\link{add.posterior.density}}) can be added.
##' @name create.base.plot
##' @title Create Base Plot
##' @return empty ggplot object
##' @export
##' @author David LeBauer
create.base.plot <- function() {
  base.plot <- ggplot()
  return(base.plot)
}
#==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Plots a prior density from a parameterized probability distribution  
##'
##' @name add.prior.density
##' @title Add Prior Density
##' @param prior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @param prior.color color of line to be plotted
##' @return plot with prior density added
##' @seealso \code{\link{pr.dens}}
##' @author David LeBauer
##' @export
##' @examples
##' add.prior.density(pr.dens('norm', 0, 1))
add.prior.density <- function(prior.density, base.plot = NULL, prior.color = 'black' ) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  new.plot <- base.plot +  geom_line(data = prior.density,
                                     aes(x = x, y = y),
                                     color = prior.color)
  return(new.plot)
}
#==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Returns a data frame from \link{stats::density} function 
##'
##' @name create.density.df
##' @title Create Density Data Frame from Sample
##' @param samps a vector of samples from a distribution
##' @param zero.bounded 
##' @param distribution list with elements distn, parama, paramb,
##' e.g. \code{list('norm', 0, 1)}
##' @author David LeBauer
##' @export
##' @return data frame with x and y = dens(x)
##' @examples
##' prior.df <- create.density.df(distribution = list('norm',0,1))
##' plot(prior.df)
##' samp.df <- create.density.df(samps = rnorm(100))
##' lines(samp.df)
create.density.df <- function(samps = NULL,
                              zero.bounded = FALSE,
                              distribution = NULL,
                              n = 1000, ...) {
  samp.exists <- !is.null(samps)
  dist.exists <- !is.null(distribution)
  if(identical(samp.exists, dist.exists)){
    stop('create.density.df requires one and only one of:
             samps: a vector of samples, e.g. MCMC chain,
               OR
             distribution: a named distribution supported by R')
  }
  if(samp.exists){
    if(zero.bounded) {
      new.density <- zero.bounded.density(samps, n = 1000, ...)
    } else {    
      new.density <- density(samps, n = 1000, ...)
    }
    density.df <- with(new.density,
                       data.frame(x = x,
                                  y = y))
  }
  
  if(dist.exists) {
    density.df <- do.call(pr.dens, c(distribution[1:3]))
  }
  return(density.df)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##'  Add posterior density to a plot
##'
##' @name add.posterior.density
##' @title Add posterior density. 
##' @param posterior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @return plot with posterior density line added
##' @export
##' @author David LeBauer
add.posterior.density <- function(posterior.density, base.plot = NULL) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  new.plot <- base.plot +  geom_line(data = posterior.density,
                                     aes(x = x, y = y))
  return(new.plot)  
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Add data to an existing plot or create a new one from \code{\link{create.base.plot}}
##'
##' Used to add raw data or summary statistics to the plot of a distribution.
##' The height of Y is arbitrary, and can be set to optimize visualization.
##' If SE estimates are available, tehse wil be plotted
##' @name add.data
##' @title Add data to plot 
##' @param trait.data data to be plotted
##' @param base.plot a ggplot object (grob),
##' created by \code{\link{create.base.plot}} if none provided
##' @param ymax maximum height of y
##' @seealso \code{\link{create.base.plot}}
##' @return updated plot object
##' @author David LeBauer
##' @export
##' @examples
##' \dontrun{add.data(data.frame(Y = c(1, 2), se = c(1,2)), base.plot = NULL, ymax = 10)}
add.data <- function(trait.data, base.plot = NULL, ymax, color = 'black') {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  n.pts <- nrow(trait.data)
  if(n.pts == 1){
    ymax <- ymax/8
  } else if (n.pts < 5) {
    ymax <- ymax / 4
  } else {
    ymax <- ymax / 2
  }
  y.pts <- seq(0, ymax, length.out = 1 + n.pts)[-1]
  plot.data <- data.frame(x = trait.data$Y,
                          y = y.pts,
                          se = trait.data$se,
                          control = !trait.data$trt == 1 & trait.data$ghs == 1)
  new.plot <- base.plot +
    geom_point(data = plot.data,
               aes(x = x, y = y,
               color = control)) +
                 geom_segment(data = plot.data,
                              aes(x = x - se, y = y, xend = x + se, yend = y,
                                  color = control)) +
                                    scale_color_manual(values = c('black', 'grey')) +
                                      opts(legend_position = "none")
  return(new.plot)
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
    base.plot <- add.prior.density(prior.density,
                                   base.plot = base.plot,
                                   prior.color = prior.color)
  }
  if(plot.posterior){
    keep <- (posterior.density$x > x.lim[1] &
             posterior.density$x < x.lim[2] &
             posterior.density$y > y.lim[1] &
             posterior.density$y < y.lim[2])
    posterior.density <- posterior.density[keep, ]
    base.plot <- add.posterior.density(posterior.density,
                                       base.plot = base.plot)
  }
  if(plot.data){
    base.plot <- add.data(trait.df,
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


#--------------------------------------------------------------------------------------------------#
##' Add borders to .. content for \description{} (no empty lines) ..
##'
##' Has ggplot2 display only specified borders, e.g. ("L"-shaped) borders, rather than a rectangle or no border. Note that the order can be significant; for example, if you specify the L border option and then a theme, the theme settings will override the border option, so you need to specify the theme (if any) before the border option, as above.
##' @name theme_border
##' @title Theme border for plot
##' @param type 
##' @param colour 
##' @param size 
##' @param linetype 
##' @return adds borders to ggplot as a side effect
##' @export
##' @author Rudolf Cardinal
##' @author \url{ggplot2 google group}{https://groups.google.com/forum/?fromgroups#!topic/ggplot2/-ZjRE2OL8lE}
##' @examples
##' \dontrun{
##' df = data.frame( x=c(1,2,3), y=c(4,5,6) )
##' ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() +
##'        opts(panel.border = theme_border(c("bottom","left")) )
##' ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() +
##'        opts(panel.border = theme_border(c("b","l")) )
##' }
theme_border <- function(type = c("left", "right", "bottom", "top", 
                           "none"), colour = "black", size = 1, linetype = 1) {
  type <- match.arg(type, several.ok=TRUE) 
  structure( 
            function(x = 0, y = 0, width = 1, height = 1, ...) { 
              xlist <- c() 
              ylist <- c() 
              idlist <- c() 
              if ("bottom" %in% type) { # bottom 
                xlist <- append(xlist, c(x, x+width)) 
                ylist <- append(ylist, c(y, y)) 
                idlist <- append(idlist, c(1,1)) 
              } 
              if ("top" %in% type) { # top 
                xlist <- append(xlist, c(x, x+width)) 
                ylist <- append(ylist, c(y+height, y+height)) 
                idlist <- append(idlist, c(2,2)) 
              } 
              if ("left" %in% type) { # left 
                xlist <- append(xlist, c(x, x)) 
                ylist <- append(ylist, c(y, y+height)) 
                idlist <- append(idlist, c(3,3)) 
              } 
              if ("right" %in% type) { # right 
                xlist <- append(xlist, c(x+width, x+width)) 
                ylist <- append(ylist, c(y, y+height)) 
                idlist <- append(idlist, c(4,4)) 
              } 
              polylineGrob( 
                           x=xlist, y=ylist, id=idlist, ..., default.units = "npc", 
                           gp=gpar(lwd=size, col=colour, lty=linetype), 
                           ) 
            }, 
            class = "theme", 
            type = "box", 
            call = match.call() 
            ) 
} 
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
