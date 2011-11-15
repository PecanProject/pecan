##' Plot univariate response of model output to a trait parameter.
##'
##' @title Sensitivity plot 
##' @param sa.sample trait quantiles used in sensitivity analysis 
##' @param sa.spline spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param y.range 
##' @param median.i index of median value in sa.sample; \code{median.i == which(as.numeric(rownames(sa.sample)) == 50) }
##' @param prior.sa.sample similar to sa.sample, but for prior distribution. If given, plots sensitivity for prior run
##' @param prior.sa.spline similar to sa.spline, but for prior trait distribution. 
##' @param fontsize (optional) list with three arguments that can be set to vary the fontsize of the title, axis labels, and axis title in the sensitivity plots
##' @return object of class ggplot
plot.sensitivity <- function(sa.sample, sa.spline, trait,
                             y.range = c(0,50), median.i = 4,
                             prior.sa.sample = NULL, prior.sa.spline = NULL,
                             fontsize = list(title = 18, axis = 14),
                             linesize = 1,
                             dotsize = 2) {
  LENGTH_OUT <- 1000
  
  units <- trait.dictionary(trait)$units
  saplot <- ggplot()

  post.x <- seq(from = min(sa.sample), to = max(sa.sample), length.out = LENGTH_OUT)

  
  saplot <- saplot + 
    ## plot spline function 
    geom_line(aes(x,y), data = data.frame(x = post.x, y = sa.spline(post.x)), size = linesize) + 
      ## plot points used to evaluate spline
      geom_point(aes(x,y), data = data.frame(x = sa.sample, y = sa.spline(sa.sample)), size = dotsize) +
                                        #indicate median with larger point
        geom_point(aes(x,y), data = data.frame(x = sa.sample[median.i], y = sa.spline(sa.sample[median.i])), size = dotsize * 1.3) + 
          scale_y_continuous(limits = range(pretty(y.range)), breaks = pretty(y.range, n = 3)[1:3]) +
            theme_bw() +
              opts(title= trait.dictionary(trait)$figid, 
                   axis.text.x = theme_text(size = fontsize$axis),
                   axis.text.y = theme_text(size = fontsize$axis),
                   axis.title.x = theme_text(size = fontsize$axis),
                   axis.title.y = theme_blank(),
                   plot.title = theme_text(size = fontsize$title),
                   panel.border = theme_blank())
  ## Following conditional can be removed to only plot posterior sa
  prior.x <- post.x
  if(!is.null(prior.sa.sample) & !is.null(prior.sa.spline)){
    prior.x <- seq(from = min(prior.sa.sample), to = max(prior.sa.sample), length.out = LENGTH_OUT)
    saplot <- saplot +
      ## plot spline
      geom_line(aes(x,y), data = data.frame(x = prior.x, y = prior.sa.spline(prior.x)),
                size = linesize, color = 'grey') +
                  ## plot points used to evaluate spline 
                  geom_point(aes(x,y), data = data.frame(x = prior.sa.sample, y = prior.sa.spline(prior.sa.sample)),
                             size = dotsize, color = 'grey') +
                               ## indicate location of medians
                               geom_point(aes(x,y), data = data.frame(x = prior.sa.sample[median.i], 
                                                      y = prior.sa.spline(prior.sa.sample[median.i])),
                                          size = dotsize * 1.5, color = 'grey') 
  }
  max.x <- max(prior.x)
  min.x <- 0
  x.breaks <- pretty(c(min.x, max.x), 4)

  saplot <- saplot + scale_x_continuous(units, limits = range(x.breaks),
                                        breaks = x.breaks)
                                        #  print(saplot)
  return(saplot)
}

plot.variance.decomposition <- function(plot.inputs, outdir,
                                        prior.plot.inputs = NULL,
                                        fontsize = list(title = 18, axis = 14),
                                        cv.xticks = pretty(c(0,150),4),
                                        el.xticks = pretty(c(-1.5,1.5),3),
                                        pv.xticks = pretty(c(0,50),4)) {
  traits    <- names(plot.inputs$partial.variances)
  units     <- trait.dictionary(traits)$units
  trait.labels <- merge(data.frame(id = traits), trait.dictionary(traits), by = 'id', sort = FALSE)$figid
  .plot.data <- data.frame(trait.labels        = trait.labels,
                           units               = units,
                           coef.vars           = plot.inputs$coef.vars * 100,
                           elasticities        = plot.inputs$elasticities,
                           partial.variances   = plot.inputs$partial.variances * 100)
                                        #  recover()
  if(!is.null(prior.plot.inputs)) {
    prior.plot.data <- data.frame(trait.labels              = trait.labels,
                                  units                     = units,
                                  prior.coef.vars           = prior.plot.inputs$coef.vars * 100,
                                  prior.elasticities        = prior.plot.inputs$elasticities,
                                  prior.partial.variances   = prior.plot.inputs$partial.variances * 100)
    .plot.data <- merge(.plot.data, prior.plot.data, by = 'trait.labels')
  }
  pv.order <- order(.plot.data$partial.variances, decreasing = FALSE)

  ## location of words and lollipops set by 'points'
  ##    these points can be moved up or down by adjusting the offset X in 1:length(traits) - X
  plot.data <- data.frame(.plot.data[pv.order, ], points = 1:length(traits) - 0.5)

  ## Notes on fine-tuning plots below
  ## axis lines and ticks drawn for each plot using geom_segment  
  ## size of x axis tick set by xend = ...
  ## vertical location of axis numbers set in base.plot using vjust
  
  base.plot <- ggplot(plot.data) +
    coord_flip() +
      theme_bw() +
        opts(axis.line.y = theme_blank(),
             axis.text.x = theme_text(size=fontsize$axis, vjust = -1),
             axis.text.y = theme_blank(),
             axis.title.x = theme_blank(), 
             axis.title.y = theme_blank(),
             axis.ticks = theme_blank(),
             panel.grid.major = theme_blank(),
             panel.grid.minor = theme_blank(),
             panel.border = theme_blank())

  if(!is.null(prior.plot.inputs)) {
    .cv.plot <-  base.plot +
      geom_pointrange(aes(x = points, y = prior.coef.vars,
                          ymin = 0, ymax = prior.coef.vars),
                      size = 1.25, color = 'grey')
    
    .el.plot <- base.plot +
      geom_pointrange(aes(x = points, prior.elasticities,
                          ymin = 0, ymax = prior.elasticities),
                      size = 1.25, color = 'grey') 

    .pv.plot <- base.plot +
      geom_pointrange(aes(x = points, y = prior.partial.variances,
                          ymin = 0, ymax = prior.partial.variances),
                      size = 1.25, color = 'grey') 
  } else {
    .cv.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$coef.vars, n = 4))
    .el.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$elasticities, n = 4))
    .pv.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$partial.variances, n = 4))
  }


  trait.plot <- base.plot + 
    opts(title = 'Parameter',
         plot.title = theme_text(hjust = 0.96, size = fontsize$title),
         axis.text.x = theme_text(colour='white'),
         axis.line.x = theme_blank()) +
           geom_text(aes(y = 1, x = points,
                         label=trait.labels, hjust = 1),
                     size = fontsize$axis/3) +
                       scale_y_continuous( breaks = c(0,0), limits = c(0,1)) +
                         ##  Add Invisible Axes to resize like other plots
                         geom_segment(aes(x = c(0,0), y = c(0,0),
                                          yend = c(0, max(cv.xticks)),
                                          xend = c(length(traits), 0)), colour = 'white')  + 
                                            ## Add invisible ticks
                                            geom_segment(aes(x = 0,
                                                             y = cv.xticks,
                                                             xend = -0.1,
                                                             yend = cv.xticks), colour = 'white') 

  cv.xticks <- pretty(range(plot.data[,grep('coef.var', colnames(plot.data))]), 4)  
  cv.plot <- .cv.plot +
    opts(title = 'CV (%)', plot.title = theme_text(size = fontsize$title)) +
      scale_y_continuous(breaks = cv.xticks, limits = range(cv.xticks)) +
        geom_pointrange(aes(x = points, y = coef.vars, ymin = 0, ymax = coef.vars),
                        size = 1.25) + 
                          ##  Add Axes
                          geom_segment(aes(x = c(0,0), y = c(0,0),
                                           yend = c(0, max(cv.xticks)),
                                           xend = c(length(traits), 0)))  + 
                                             ## Add Ticks
                                             geom_segment(aes(x = 0,
                                                              y = cv.xticks,
                                                              xend = -0.1,
                                                              yend = cv.xticks))


  el.range <-  range(abs(plot.data[,grep('elasticities', colnames(plot.data))]))
  el.xticks <- pretty(el.range, 3)   
  el.plot <- .el.plot + 
    opts(title = 'Elasticity', plot.title = theme_text(size = fontsize$title)) +
      scale_y_continuous(breaks = el.xticks, limits = range(el.xticks)) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities),
                        size = 1.25) +
                          ##  Add Axes
                          geom_segment(aes(x = c(0,0), y = c(0,min(el.xticks)),
                                           yend = c(0, max(el.xticks)),
                                           xend = c(length(traits), 0)))  + 
                                             ## Add Ticks
                                             geom_segment(aes(x = 0,
                                                              y = el.xticks,
                                                              xend = -0.1,
                                                              yend = el.xticks)) 

  pv.xticks <- pretty(range(plot.data[,grep('partial.variance', colnames(plot.data))]), 4)  
  pv.plot <- .pv.plot + 
    opts(title = 'Partial Variance (%)',
         plot.title = theme_text(size = fontsize$title)) +
           scale_y_continuous(breaks = pv.xticks, limits = range(pv.xticks)) +
             geom_pointrange(aes(x = points, partial.variances,
                                 ymin = 0, ymax = partial.variances), size = 1.25) +
                                   ##  Add Axes
                                   geom_segment(aes(x = c(0,0), y = c(0,0),
                                                    yend = c(0, max(pv.xticks)),
                                                    xend = c(length(traits), 0)))  + 
                                                      ## Add Ticks
                                                      geom_segment(aes(x = 0,
                                                                       y = pv.xticks,
                                                                       xend = -0.1,
                                                                       yend = pv.xticks)) 
  
  
  return(list(trait.plot = trait.plot, cv.plot = cv.plot, el.plot = el.plot, pv.plot = pv.plot))


}

##' Plot functions and quantiles used in sensitivity analysis
##'
##' @title Plot Sensitivities
##' @param sensitivity.results list containing sa.samples and sa.splines 
##' @param outdir 
##' @return outputs plots in outdir/sensitivity.analysis.pdf 
plot.sensitivities <- function(sensitivity.plot.inputs, outdir, prior.sensitivity.plot.inputs = NULL, ...){
  sa.samples <- sensitivity.plot.inputs$sa.samples
  sa.splines <- sensitivity.plot.inputs$sa.splines
  if(!is.null(prior.sensitivity.plot.inputs)) {
    prior.sa.samples <- prior.sensitivity.plot.inputs$sa.samples
    prior.sa.splines <- prior.sensitivity.plot.inputs$sa.splines
  }
  traits <- names(sa.samples)

  y.range <- c(0, max(mapply(do.call, sa.splines, lapply(sa.samples, list)), na.rm = TRUE))

  sensitivity.plots <- list()
  for(trait in traits) {
    if(!is.null(prior.sensitivity.plot.inputs)) {
      prior.sa.sample <- prior.sa.samples[,trait]
      prior.sa.spline <- prior.sa.splines[[trait]]
    } else {
      prior.sa.sample <- NULL
      prior.sa.spline <- NULL
    }
    sensitivity.plots[[trait]] <-plot.sensitivity(sa.sample =  sa.samples[,trait],
                                                  sa.spline = sa.splines[[trait]],
                                                  trait <- trait,
                                                  y.range = y.range,
                                                  median.i =  4,#which(as.numeric(rownames(sa.samples)) == 50),
                                                  prior.sa.sample = prior.sa.sample,
                                                  prior.sa.spline = prior.sa.spline,
                                                  ...)
  }
  return(sensitivity.plots)
}

##' Variable-width (dagonally cut) histogram
##'
##' 
##' When constructing a histogram, it is common to make all bars the same width.
##' One could also choose to make them all have the same area.
##' These two options have complementary strengths and weaknesses; the equal-width histogram oversmooths in regions of high density, and is poor at identifying sharp peaks; the equal-area histogram oversmooths in regions of low density, and so does not identify outliers.
##' We describe a compromise approach which avoids both of these defects. We regard the histogram as an exploratory device, rather than as an estimate of a density. 
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
dhist<-function(x, a=5*iqr(x),
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
        if (diff.xbr[i-1]<=spike&diff.xbr[i]<=spike&
            !is.na(diff.xbr[i])) {
          amt.spike<-amt.spike+diff.xbr[i-1]
          counts.new[i-1]<-counts.new[i-1]+counts.new[i]
          xbr.new[i]<-NA
          counts.new[i]<-NA
          flag.vec[i-1]<-T
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

##' Calculate interquartile range
##'
##' Calculates the 25th and 75th quantiles given a vector x; used in function \link{dhist}.
##' @title Interquartile range
##' @param x vector
##' @return numeric vector of length 2, with the 25th and 75th quantiles of input vector x. 
iqr<-function(x){
  return(diff(quantile(x, c(0.25, 0.75), na.rm = TRUE)))
}

create.base.plot <- function() {
  base.plot <- ggplot()
  return(base.plot)
}
##' Plots a prior density from a parameterized probability distribution  
##'
##' @title Add Prior Density
##' @param prior dataframe or list with density name and parameters
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @param prior.color color of line to be plotted
##' @return plot with prior density added
##' @seealso \code{\link{pr.dens}}
##' @examples
##' add.prior.density(c('norm', 0, 1))
add.prior.density <- function(prior, base.plot = NULL, prior.color = 'black') {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  prior.density <- do.call(pr.dens, prior)
  new.plot <- base.plot +  geom_line(data = prior.density,
                                     aes(x = x, y = y),
                                     color = prior.color)
  return(new.plot)
}

##' Returns a data frame from \link{stats::density} function 
##'
##' @title Create Density Data Frame from Sample
##' @param sample 
##' @param ... additional arguments to density 
##' @return data frame with x and y
create.density.df <- function(sample, zero.bounded = FALSE) {
  if(zero.bounded) {
    new.density <- zero.bounded.density(sample)
  } else {    
    new.density <- density(sample)
  }
  density.df <- with(new.density,
                     data.frame(x = x,
                                y = y))
  return(density.df)
}

##'  Add posterior density to a plot
##'
##' @title Add posterior density. 
##' @param posterior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @return plot with posterior density line added
add.posterior.density <- function(posterior.density, base.plot = NULL) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  new.plot <- base.plot +  geom_line(data = posterior.density,
                                     aes(x = x, y = y))
  return(new.plot)  
}

##' Add data to an existing plot or create a new one from \code{\link{create.base.plot}}
##'
##' Used to add raw data or summary statistics to the plot of a distribution.
##' The height of Y is arbitrary, and can be set to optimize visualization.
##' If SE estimates are available, tehse wil be plotted
##' @title Add data to plot 
##' @param trait.data data to be plotted
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @param ymax maximum height of y
##' @seealso \code{\link{create.base.plot}}
##' @return updated plot object
add.data <- function(trait.data, base.plot = NULL, ymax) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  plot.data <- with(trait.data,
                    data.frame(x = Y,
                               y = seq(0, ymax / 5, length.out = nrow(trait.data)),
                               se = se))
  new.plot <- base.plot +
    geom_point(data = plot.data, aes(x = x, y = y)) +
      geom_segment(data = plot.data, aes(x = x - se, y = y, xend = x + se, yend = y))
  return(new.plot)
}

##' Plot trait density and data
##'
##' @title Plot trait density
##' @param trait 
##' @param posterior.sample 
##' @param trait.df 
##' @return plot (grob) object
plot.trait <- function(trait,
                       prior,
                       posterior.sample = NULL,
                       trait.data = NULL,
                       fontsize = list(title = 18, axis = 14)) {
  
  ## Determine plot components
  plot.posterior <- !is.null(posterior.sample)
  plot.prior     <- !is.null(prior.density)
  plot.data      <- !is.null(trait.data)
  
  ## If no posterior, set prior density to black
  prior.color <- ifelse(plot.posterior, 'grey', 'black')

  ## get units for plot title
  units <- trait.dictionary(trait)$units

  base.plot <- create.base.plot() + theme_bw()

  prior.density   <- do.call(pr.dens, prior) 
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
    base.plot <- add.prior.density(prior, base.plot = base.plot, prior.color = prior.color)
  }
  if(plot.posterior){
    posterior.density <- create.density.df(posterior.sample)
    base.plot <- add.posterior.density(posterior.density, base.plot = base.plot)
  }
  if(plot.data){
    base.plot <- add.data(trait.data, base.plot = base.plot, ymax = ymax)
  }

  trait.plot <- base.plot +
    opts(title= paste(trait.dictionary(trait)$figid, " (", units, ")", sep = ''), 
         axis.text.x = theme_text(size = fontsize$axis, vjust = -2),
         axis.text.y = theme_blank(),
         axis.title.x = theme_blank(),
         axis.title.y = theme_blank(),
         axis.ticks = theme_blank(),
         axis.line = theme_blank(),
         plot.title = theme_text(size = fontsize$title),
         panel.grid.major = theme_blank(),
         panel.grid.minor = theme_blank(),
         panel.border = theme_blank())# +
                                        #           geom_segment(aes(x = 0, y = 0, yend = 0, xend = max(x.ticks))) +
                                        #             geom_segment(aes(x = x.ticks, y = 0,
                                        #                              xend = x.ticks, yend = -x.ticklength)) +
                                        #                                scale_x_continuous(breaks = x.ticks)
  print(trait.plot)
}


##' Plot probability density and data
##'
##' @title Plot Trait Probability Densities
##' @param sensitivity.results list containing sa.samples and sa.splines 
##' @param outdir 
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
