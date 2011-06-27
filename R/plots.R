##' Plot univariate response of model output to a trait parameter.
##'
##' .. content for \details{} ..
##' @title Sensitivity plot 
##' @param x.values trait quantiles used in sensitivity analysis
##' @param fun spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param prior.x.values if given, plots sensitivity for prior run 
##' @param prior.fun if given, plots sensitivity for prior run
##' @return object of class ggplot
plot.sensitivity <- function(x.values, fun, trait, ylim = c(0,50), median.i = 4, prior.x.values = NA, prior.fun = NA) {
  LENGTH_OUT <- 100
  all.x <- c(x.values, prior.x.values[!is.na(prior.x.values)])
  x <- seq(from = min(x.values), to = max(x.values), length.out = LENGTH_OUT)
  y <- fun(x)
  xlim <- c(ifelse((max(all.x)-min(all.x))/4>min(all.x),0,0.9*min(all.x)), signif(max(all.x)+min(all.x),2))
  line.data <- data.frame(x = x, y = y)
  point.data <- data.frame(x = x.values, y = fun(x.values))
  median.i <- which(as.numeric(rownames(x.values)) == 50)
  saplot <- ggplot() +
      geom_line(data = line.data, aes(x, y), size = 2) +
        geom_point(data = point.data, aes(x, y), size = 3) +
             geom_point(data = point.data[median.i,], aes(x, y), size = 5) + #indicate location of medians
               scale_y_continuous(limits = c(0,ylim), breaks = seq(0, ylim, 0.5)) +
                 coord_cartesian(x=c(0, max(x)*1.1)) +
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
        geom_point(aes(x.values, fun(x.values)), size = 3, color = 'grey') + 
          geom_point(data = point.data[median.i,], aes(x, y), size = 5, color = 'grey') #indicate location of medians

  }
  return(saplot)
}

#        scale_x_continuous(limits = ifelse(diff(range(x))/min(x) < 0.25 ,
#                             c(0, max(x) * 1.1),
#                             c(0.9, 1.1) * range(x))) 
#                            
#                               


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
  ## pre-calculate the maximum value of all of the y-axis
  round.up <- function(x, place = 10) ceiling(x / place)*place
  max.y <- max(mapply(do.call, sa.splinefuns, lapply(sa.samples, list)), na.rm = TRUE)


  for(trait in traits) {
    print(plot.sensitivity(x.values = sa.samples[[trait]],
                           fun = sa.splinefuns[[trait]],
                           trait = trait,
                           ylim = c(0, roundup(max.y)),
                           median.i =  which(as.numeric(rownames(sa.samples)) == 50)))
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
