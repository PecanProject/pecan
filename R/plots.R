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

  post.x <- seq(from = min(sa.sample),
                to = max(sa.sample),
                length.out = LENGTH_OUT)
  
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

plot.variance.decomposition <- function(plot.inputs,
                                        prior.plot.inputs = NULL,
                                        fontsize = list(title = 18, axis = 14)){
  traits    <- names(plot.inputs$variances)
  units     <- trait.dictionary(traits)$units
  trait.labels <- merge(data.frame(id = traits), trait.dictionary(traits), by = 'id', sort = FALSE)$figid
  .plot.data <- data.frame(trait.labels  = trait.labels,
                           units         = units,
                           coef.vars     = plot.inputs$coef.vars * 100,
                           elasticities  = plot.inputs$elasticities,
                           variances     = plot.inputs$variances)
                                        #  recover()
  if(!is.null(prior.plot.inputs)) {
    prior.plot.data <- data.frame(trait.labels       = trait.labels,
                                  units              = units,
                                  prior.coef.vars    = prior.plot.inputs$coef.vars * 100,
                                  prior.elasticities = prior.plot.inputs$elasticities,
                                  prior.variances    = prior.plot.inputs$variances)
    .plot.data <- merge(.plot.data, prior.plot.data, by = 'trait.labels')
  }
  pv.order <- order(.plot.data$variances, decreasing = FALSE)

  ## location of words and lollipops set by 'points'
  ##    these points can be moved up or down by adjusting the offset X in 1:length(traits) - X
  plot.data <- data.frame(.plot.data[pv.order, ], points = 1:length(traits) - 0.5)
  cv.xticks <<- pretty(as.matrix(plot.data[,grep('coef.var', colnames(plot.data))]), 4)
  pv.xticks <<- pretty(as.matrix(plot.data[,grep('variances', colnames(plot.data))]), 4)  
  el.xticks <<- pretty(as.matrix(plot.data[,grep('elasticities', colnames(plot.data))]), 3)
  el.xrange <<- range(pretty(as.matrix(plot.data[,grep('elasticities', colnames(plot.data))]), 4))

  
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
      geom_pointrange(aes(x = points, y = prior.variances,
                          ymin = 0, ymax = prior.variances),
                      size = 1.25, color = 'grey') 
  } else {
    .cv.plot <- base.plot + scale_y_continuous(breaks = cv.xticks)
    .el.plot <- base.plot + scale_y_continuous(breaks = el.xrange)
    .pv.plot <- base.plot + scale_y_continuous(breaks = pv.xticks)
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


  if (diff(range(el.xticks)) < 4) el.xticks <- c(-1,0,1)
  el.plot <- .el.plot + 
    opts(title = 'Elasticity', plot.title = theme_text(size = fontsize$title)) +
      scale_y_continuous(breaks = el.xticks, limits = range(el.xrange)) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities),
                        size = 1.25) +
                          ##  Add Axes
                          geom_segment(aes(x = c(0,0), y = c(0, min(el.xrange)),
                                           yend = c(0, max(el.xrange)),
                                           xend = c(length(traits), 0)))  +
                                             ## Add Ticks
                                             geom_segment(aes(x = 0,
                                                              y = el.xticks,
                                                              xend = -0.1,
                                                              yend = el.xticks)) 

  pv.plot <- .pv.plot + 
    opts(title = 'Partial Variance',
         plot.title = theme_text(size = fontsize$title)) +
           scale_y_continuous(breaks = pv.xticks, limits = range(pv.xticks)) +
             geom_pointrange(aes(x = points, variances,
                                 ymin = 0, ymax = variances), size = 1.25) +
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
##' 
##' @title Plot Sensitivities
##' @param sensitivity.plot.inputs 
##' @param prior.sensitivity.plot.inputs 
##' @param ... arguments passed to \code{\link{plot.sensitivity}}
##' @param sensitivity.results list containing sa.samples and sa.splines 
##' @return list of plots, one per trait
plot.sensitivities <- function(sensitivity.plot.inputs, prior.sensitivity.plot.inputs = NULL, ...){
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
##' @param prior.density 
##' @param base.plot a ggplot object (grob), created by \code{\link{create.base.plot}} if none provided
##' @param prior.color color of line to be plotted
##' @return plot with prior density added
##' @seealso \code{\link{pr.dens}}
##' @examples
##' add.prior.density(c('norm', 0, 1))
add.prior.density <- function(prior.density, base.plot = NULL, prior.color = 'black' ) {
  if(is.null(base.plot)) base.plot <- create.base.plot()
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
create.density.df <- function(samps = NULL,
                              zero.bounded = FALSE,
                              distribution = NULL) {
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
      new.density <- zero.bounded.density(samps)
    } else {    
      new.density <- density(samps)
    }
    density.df <- with(new.density,
                       data.frame(x = x,
                                  y = y))
  }
  
  if(dist.exists) {
    density.df <- do.call(pr.dens, c(distribution[1:3], n = 1000))
  }
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
##' @examples
##' add.data(data.frame(Y = c(1, 2), se = c(1,2)), base.plot = NULL, ymax = 10)
add.data <- function(trait.data, base.plot = NULL, ymax, color = 'black') {
  if(is.null(base.plot)) base.plot <- create.base.plot()
  n.pts <- nrow(trait.data)
  ymax <- ymax * sqrt(n.pts + 1)/sqrt((n.pts + 1) * 64)
  y.pts <- seq(0, ymax, length.out = 1 + n.pts)[-1]
  plot.data <- data.frame(x = trait.data$Y,
                          y = y.pts,
                          se = trait.data$se)
  new.plot <- base.plot +
    geom_point(data = plot.data,
               aes(x = x, y = y),
               color = color) +
                 geom_segment(data = plot.data,
                              aes(x = x - se, y = y, xend = x + se, yend = y),
                              color = color)
  return(new.plot)
}

##' Plot trait density and data
##'
##' @title Plot trait density
##' @param trait character, name of trait to be plotted
##' @param prior named distribution with parameters
##' @param posterior.sample 
##' @param trait.data 
##' @param fontsize 
##' @return plot (grob) object
##' @examples
##' prior1 <- data.frame(distn = 'norm',
##'                      parama = 20,
##'                      paramb = 5)
##' data1  <- data.frame(Y = c(19, 21), se = c(1,1))
##' plot.trait(trait = 'Vcmax',
##'           prior = prior1,
##'           trait.data = data1)
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
  units <- trait.dictionary(trait)$units

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
    x.lim <- range(posterior.density$x, prior.density$x, na.rm = TRUE)
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
  if(logx == TRUE){
    ticks$x <- ticks$x[ticks$x > 0]
    base.plot <- base.plot + scale_x_log10(units,
                                           breaks = ticks$x,
                                           labels = ticks$x,
                                           limits = range(ticks$x))
  } else {
    base.plot <- base.plot + scale_x_continuous(units,
                                                breaks = ticks$x,
                                                labels = ticks$x,
                                                limits = range(ticks$x))
  }
  
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
    opts(title= trait.dictionary(trait)$figid, 
         axis.text.x = theme_text(size = fontsize$axis),
         axis.text.y = theme_blank(),
         axis.title.x = theme_text(size = fontsize$axis),
         axis.title.y = theme_blank(),
         axis.ticks = theme_blank(),
         axis.line = theme_blank(),
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


##' Calculate the density of a distribution for use in plotting
##'
##' @title Prior Density 
##' @param distribution one of R's supported distributions (character)
##' @param a first parameter of distribution (numeric)
##' @param b second parameter of distribution (numeric)
##' @return data frame with values of x, the density at each x and the probability at each x
##' @author David LeBauer
prior.density <- function(distribution = 'norm', a = 0, b = 1, xlim = NA){
  distribution <- gsub('lognormal', 'lnorm', distribution)
  if(distribution != 'beta'){
    if(isTRUE(is.na(xlim))){
      range.x <- range(pretty(do.call(paste('q', distribution, sep = ''), list(c(0.005, 0.995),a,b))))
    } else if (isTRUE(!is.na(xlim))) {
      range.x <- xlim
    }
    prior.x <- seq(from=range.x[1], to = range.x[2], length = 1000)
  } else {
    range.x <- c(0,1)
    prior.x <- seq(from=0, to = 1, length = 1000)  
  }
  dens.x  <- do.call(paste('d', distribution, sep=''),list(prior.x, a, b))
  prob.x  <- do.call(paste('p', distribution, sep=''),list(prior.x, a, b))
  return(data.frame(prior.x, dens.x, prob.x))
}

##' Plot prior density and data
##'
##' @title Prior Figure 
##' @param priordata observations to be plotted as points
##' @param priordensity density of prior distribution, calculated by \code{\link{prior.density}}
##' @param trait name of trait
##' @param xlim limits for x axis
##' @return plot / grob of prior distribution with data used to inform the distribution 
priorfig <- function(priordata = NA, priordensity = NA, trait = '', xlim = 'auto', fontsize = 18){
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
    scale_x_continuous(limits = xlim, breaks = x.breaks, trait.dictionary(trait)$units) +
      scale_y_continuous(breaks=NA)+
        opts(title = trait.dictionary(trait)$figid,
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
                           x = x + runif(length(x), -dx/2, dx/2))# this adds jitter to separate equal values 
    rug <- geom_rug(data = priordata, aes(x))

                                        #rug <- geom_point(data = priordata, aes(x=x, y = seq(0,max(priordensity$dens.x)/30, length = length(x))), size = 3, alpha = 3/sqrt(nrow(priordata)))
                                        #hist <-  geom_histogram(data = priordata, aes(x=x, y = ..density..),  alpha = 0.5, binwidth = diff(range(priordata))/sqrt(nrow(priordata)))
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


get.quantiles.from.density <- function(priordensity){
  qi <- c(which.min(abs(priordensity$prob.x - 0.025)),
          which.min(abs(priordensity$prob.x - 0.5)),
          which.min(abs(priordensity$prob.x - 0.975)))
  qs <- priordensity[qi,c('prior.x', 'dens.x')]
  colnames(qs) <- c('x', 'y')
  return(qs)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title panel.border
##' @param type 
##' @param colour 
##' @param size 
##' @param linetype 
##' @return 
##' @author Rudolf Cardinal http://goo.gl/YThRT
##' @example
##' ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts(
##' panel.border = theme_border(c("bottom","left")) )
##' ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts(
##' panel.border = theme_border(c("b","l")) )
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
