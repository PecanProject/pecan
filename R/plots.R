##' Plot univariate response of model output to a trait parameter.
##'
##' @title Sensitivity plot 
##' @param sa.sample trait quantiles used in sensitivity analysis 
##' @param sa.splinefun spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param y.range 
##' @param median.i index of median value in sa.sample; \code{median.i == which(as.numeric(rownames(sa.sample)) == 50) }
##' @param prior.sa.sample similar to sa.sample, but for prior distribution. If given, plots sensitivity for prior run
##' @param prior.sa.splinefun similar to sa.splinefun, but for prior trait distribution. 
##' @param fontsize (optional) list with three arguments that can be set to vary the fontsize of the title, axis labels, and axis title in the sensitivity plots
##' @return object of class ggplot
##' @author David LeBauer
plot.sensitivity <- function(sa.sample, sa.splinefun, trait,
                             y.range = c(0,50), median.i = 4,
                             prior.sa.sample = NA, prior.sa.splinefun = NA,
                             fontsize = list(title = 34, axis = 24)) {
  LENGTH_OUT <- 1000
  all.x <- c(sa.sample, prior.sa.sample[!is.na(prior.sa.sample)])
  x <- seq(from = min(sa.sample), to = max(sa.sample), length.out = LENGTH_OUT)
  x.ticks <- pretty(all.x)  
  xlim <- range(x.ticks)
  line.data <- data.frame(x = x, y = sa.splinefun(x))
  point.data <- data.frame(x = sa.sample, y = sa.splinefun(sa.sample))
  units <- gsub(get.units(trait)$units, '%', 'fraction')
  saplot <- ggplot() +
    ## plot spline function
    geom_line(data = line.data, aes(x, y), size = 2) +
      ## plot points used to evaluate spline
      geom_point(data = point.data, aes(x, y), size = 3.5) +
        #indicate median with larger point
        geom_point(data = point.data[median.i,], aes(x, y), size = 4.5) + 
          scale_y_continuous(limits = range(pretty(y.range)), breaks = pretty(y.range)) +
              scale_x_continuous(units, limits = range(pretty(x)), breaks = pretty(x)) +
                theme_bw() +
                  opts(title= paste(trait.dictionary(trait)$figid, " (", units, ")", sep = ''), 
                       axis.text.x = theme_text(size = fontsize$axis),
                       axis.text.y = theme_text(size = fontsize$axis),
                       axis.title.x = theme_blank(),
                       axis.title.y = theme_blank(),
                       plot.title = theme_text(size = fontsize$title),
                       panel.border = theme_blank())
  if(!is.na(prior.sa.sample) & !is.na(prior.sa.splinefun)){
    x <- seq(from = min(prior.sa.sample), to = max(prior.sa.sample), length.out = LENGTH_OUT)
    saplot <- saplot +
      ## plot spline
      geom_line(aes(x, sa.splinefun(x)), size = 2, color = 'grey') +
        ## plot points used to evaluate spline 
        geom_point(aes(sa.sample, sa.splinefun(sa.sample)), size = 3.5, color = 'grey') +
          ## indicate location of medians
          geom_point(data = point.data[median.i,], aes(x, y), size = 4.5, color = 'grey') 
  }
  return(saplot)
}

plot.variance.decomposition <- function(plot.inputs, outdir,
                                        prior.plot.inputs = NA,
                                        fontsize = list(title = 18, axis = 14)) {
  traits    <- names(plot.inputs$partial.variances)
  units     <- get.units(traits)$units
  trait.labels <- trait.dictionary(traits)[,'figid']
  plot.data <- data.frame(trait.labels        = trait.labels,
                          units               = units,
                          coef.vars           = plot.inputs$coef.vars * 100,
                          elasticities        = plot.inputs$elasticities,
                          partial.variances   = plot.inputs$partial.variances * 100,
                          points              = 1:length(traits))

  if(!is.na(prior.plot.inputs)) {
    prior.plot.data <- data.frame(trait.labels              = trait.labels,
                                  units                     = units,
                                  prior.coef.vars           = prior.plot.inputs$coef.vars,
                                  prior.elasticities        = prior.plot.inputs$elasticities,
                                  prior.partial.variances   = prior.plot.inputs$partial.variances)
    plot.data <- merge(plot.data, prior.plot.data, by = 'trait.labels')
  }
  
  base.plot <- ggplot(plot.data) +
    coord_flip() +
      theme_bw() +
        scale_y_continuous(expand = c(0,0)) + 
          opts(axis.line.y = theme_segment(),
               axis.line.x = theme_blank(),
               axis.text.x = theme_text(size=fontsize$axis),
               axis.text.y = theme_blank(),
               axis.title.x = theme_blank(), 
               axis.title.y = theme_blank(),
               axis.ticks = theme_blank(),
               panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank(),
               panel.border = theme_blank())

if(!is.na(prior.plot.inputs)) {
  .cv.plot <-  base.plot +
    geom_pointrange(aes(x = points, y = prior.coef.vars,
                        ymin = 0, ymax = prior.coef.vars),
                    size = 1.25, alpha = 0.25) +
                       scale_y_continuous(breaks =  pretty(plot.data$prior.coef.vars, n = 4))

  
  .el.plot <- base.plot +
    geom_pointrange(aes(x = points, prior.elasticities,
                        ymin = 0, ymax = prior.elasticities),
                    size = 1.25, color = 'grey') +
                      scale_y_continuous(breaks =
                                         pretty(c(plot.data[,grep('elasticities',
                                                                  colnames(plot.data))])[[1]], n = 4))


  .pv.plot <- base.plot +
    geom_pointrange(aes(x = points, y = prior.partial.variances,
                        ymin = 0, ymax = prior.partial.variances),
                    size = 1.25, color = 'grey') +
                      scale_y_continuous(breaks =
                                         pretty(c(plot.data[,grep('partial.var',
                                                                  colnames(plot.data))])[[1]], n = 4))
} else {
  .cv.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$coef.vars, n = 4))
  .el.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$elasticities, n = 4))
  .pv.plot <- base.plot + scale_y_continuous(breaks =  pretty(plot.data$prior.variances, n = 4))
 }

  
  trait.plot <- base.plot + 
    opts(title = 'Parameter',
         plot.title = theme_text(hjust = 0.96, size = fontsize$title),
         axis.text.x = theme_text(colour='white'),
         axis.line.x = theme_blank()) +
           geom_text(aes(y = 1, x = points,
                         label=trait.labels, hjust = 1),
                     size = fontsize$axis/3) +
                       scale_y_continuous( breaks = c(0,0), limits = c(0,1))
  
  cv.plot <- .cv.plot + 
    opts(title = 'CV (%)', plot.title = theme_text(size = fontsize$title)) +
      geom_pointrange(data = plot.data,
                      aes(x = points, y = coef.vars, ymin = 0, ymax = coef.vars),
                      size = 1.25)
  
  el.plot <- .el.plot + 
    opts(title = 'Elasticity', plot.title = theme_text(size = fontsize$title)) +
       geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities),
                       size = 1.25)

  pv.plot <- .pv.plot + 
    opts(title = 'Partial Variance (%)', plot.title = theme_text(size = fontsize$title)) +
      geom_pointrange(aes(x = points, partial.variances,
                          ymin = 0, ymax = partial.variances), size = 1.25)

  
 

  ## stand in to be replaced by plot used in publication
  pdf(paste(outdir, 'variance.decomposition.pdf', sep=''), width = 11, height = 8)
  grid.arrange(trait.plot,
               cv.plot,
               el.plot,
               pv.plot,
               ncol = 4)
  dev.off()
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param sensitivity.results list containing sa.samples and sa.splinefuns 
##' @param outdir 
##' @return 
##' @author David
plot.sensitivities <- function(sensitivity.plot.inputs, outdir, ...){
  sa.samples <- sensitivity.plot.inputs$sa.samples
  sa.splinefuns <- sensitivity.plot.inputs$sa.splinefuns
  traits <- names(sa.samples)
  pdf(paste(outdir, 'sensitivity.analysis.pdf', sep=''), height = 12, width = 20)

  y.range <- c(0, max(mapply(do.call, sa.splinefuns, lapply(sa.samples, list)), na.rm = TRUE))

  for(trait in traits) {
    sensitivity.plot <- plot.sensitivity(sa.sample =  sa.samples[,trait],
                                         sa.splinefun = sa.splinefuns[[trait]],
                                         trait <- trait,
                                         y.range = y.range,
                                         median.i =  which(as.numeric(rownames(sa.samples)) == 50),
                                         ...)
    print(sensitivity.plot)
  }
  dev.off()
}

##' .. content for \description{} (no empty lines) ..
##' Functions for creating variable width histogram
##' From Lorraine Denby, Colin Mallows. Journal of Computational and Graphical Statistics. March 1, 2009, 18(1): 21-31. doi:10.1198/jcgs.2009.0002. http://pubs.amstat.org/doi/pdf/10.1198/jcgs.2009.0002
##' .. content for \details{} ..
##' @title 
##' @param x is the data
##' @param a is the scaling factor, default is 5 * IQR
##' @param nbins is the number of bins, default is assigned by the Stuges method
##' @param rx  is the range used for the left of the left-most bin to the right of the right-most bin  
##' @param eps 
##' @param xlab is label for the x axis 
##' @param plot = T produces the plot, F returns the heights, breaks and counts
##' @param lab.spikes = T labels the % of data in the spikes
##' @return 
##' @author Lorraine Denby, Colin Mallows
dhist<-function(x, a=5*iqr(x),
                nbins=nclass.Sturges(x), rx = range(x,na.rm=T),
                eps=.15, xlab = "x", plot = T,lab.spikes=T)
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
                                ybr, left.include = T), cut(ylower, breaks = ybr),
                  cut(ylower, breaks = ybr, left.include = T))
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
    if(length(straddlers) > 0) counts <- table(c(1:nbins, cmtx[
                                                               - straddlers, 1])) else counts <- table(c(
                                                                                                         1:nbins, cmtx[, 1]))
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
                                        # N.B. argument "widths" in barplot must be xbr
    heights <- counts/widths
  }
  bin.size <- length(x)/nbins
  cut.pt <- unique(c(min(x) - abs(min(x))/1000, approx(seq(length(
                                                                  x)), x, (1:(nbins - 1)) * bin.size, rule = 2)$y, max(
                                                                                                        x)))
  aa <- hist(x, breaks = cut.pt, plot = F, probability = T)
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
            par(xpd=T)
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
##' Calculates the 25th and 75th quantiles given a vector x.
##' @title Interquartile range
##' @param x vector
##' @return numeric vector of length 2, with the 25th and 75th quantiles of input vector x. 
iqr<-function(x){ return(diff(quantile(x,c(.25,.75),na.rm=T))) }
