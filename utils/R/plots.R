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
##' (\code{\link{plot.data}}, \code{\link{plot.prior.density}},
##' \code{\link{plot.posterior.density}}) can be added.
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
##' Add data to an existing plot or create a new one from \code{\link{create.base.plot}}
##'
##' Used to add raw data or summary statistics to the plot of a distribution.
##' The height of Y is arbitrary, and can be set to optimize visualization.
##' If SE estimates are available, tehse wil be plotted
##' @name plot.data
##' @title Add data to plot 
##' @param trait.data data to be plotted
##' @param base.plot a ggplot object (grob),
##' created by \code{\link{create.base.plot}} if none provided
##' @param ymax maximum height of y
##' @seealso \code{\link{create.base.plot}}
##' @return updated plot object
##' @author David LeBauer
##' @export plot.data
##' @examples
##' \dontrun{plot.data(data.frame(Y = c(1, 2), se = c(1,2)), base.plot = NULL, ymax = 10)}
plot.data <- function(trait.data, base.plot = NULL, ymax, color = 'black') {

  if(is.null(base.plot)) base.plot <- create.base.plot()
  
  n.pts <- nrow(trait.data)
  if(n.pts == 1){
    ymax <- ymax/16
  } else if (n.pts < 5) {
    ymax <- ymax / 8
  } else {
    ymax <- ymax / 4
  }
  y.pts <- seq(0, ymax, length.out = 1 + n.pts)[-1]
  
  if(!'ghs' %in% names(trait.data)) trait.data$ghs <- 1
  
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
    theme(legend.position = "none")
  return(new.plot)
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
