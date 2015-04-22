#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##'  Plot results of variance decomposition
##'
##' Plots variance decomposition tryptich
##' @name variance.decomposition.plot
##' @title Variance Decomposition Plots
##' @export
##' @author David LeBauer, Carl Davidson
##' @param ... Output from any number of sensitivity analyses. Output must be of the form 
##'          given by sensitivity.results$variance.decomposition.output in model output
##' @param all.plot.inputs Optional argument allowing output from sensitivity analyses to be specified in a list
##' @param exclude vector of strings specifying parameters to omit from the variance decomposition graph
##' @param convert.var function transforming variances to the value displayed in the graph
##' @param var.label label to displayed over variance column
##' @param order.plot.input Output from a sensitivity analysis that is to be used to order parameters.
##'          Parameters are ordered by variance. Defaults to the first sensitivity analysis output given
##' @param ticks.plot.input Output from a sensitivity analysis that is to be used.
##'          Defaults to the first sensitivity analysis output given
##' @param col Color of each sensitivity analysis. Equivalent to col parameter of the plot function.
##' @param pch Shape of each sensitivity analysis. Equivalent to pch parameter of the plot function.
##' @param main Plot title. Useful for multi-pft variance decompositions.
##' @param fontsize list specifying the font size of the titles and axes of the graph
##' @examples
##' x <- list(trait.labels = c("a", "b", "c"),
##'           coef.vars = c(a=1,b=0.5, c=0.1),
##'           elasticities = c(a=1,b=2,c=0.5),
##'           variances    = c(a = 20, b=30, c = 10))
##' do.call(grid.arrange, c(variance.decomposition.plot(x), ncol = 4))
variance.decomposition.plot <- function(plot.inputs,
                                        fontsize = list(title = 18, axis = 14)){
  require(ggmap)
  theme_set(theme_classic() +
    theme(axis.text.x = element_text(size=fontsize$axis, vjust = -1),
          axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()))
  
  traits       <- names(plot.inputs$variances)
  units        <- as.character(trait.lookup(traits)$units)
  trait.labels <- as.character(trait.lookup(traits)$figid)
  plot.data <- data.frame(trait.labels  = ifelse(!is.na(trait.labels),
                            trait.labels, traits),
                          units         = ifelse(!is.na(units), units, ""),
                          coef.vars     = plot.inputs$coef.vars * 100,
                          elasticities  = plot.inputs$elasticities,
                          variances     = plot.inputs$variances,
                          points = 1:length(traits) - 0.5)

  plot.data <- plot.data[order(plot.data$variances,
                               decreasing = FALSE), ]
  
  base.plot <- ggplot(plot.data) +
    coord_flip() 
    

  trait.plot <- base.plot + ggtitle("Parameter") +
    geom_text(aes(y = 1, x = points,
                  label=trait.labels, hjust = 1),
              size = fontsize$axis/3) + 
    scale_y_continuous( breaks = c(0,0), limits = c(0,1)) +
    theme(axis.text.x = element_blank())
  cv.plot <- base.plot + ggtitle("CV (%)") +
    geom_pointrange(aes(x = points, y = coef.vars, ymin = 0, ymax = coef.vars),
                    size = 1.25) +
    theme(plot.title = element_text(size = fontsize$title)) 


  el.plot <- base.plot + ggtitle("Elasticity") +
    theme(plot.title = element_text(size = fontsize$title)) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities),
                        size = 1.25) 

  pv.plot <- base.plot + ggtitle("Variance") +
    theme(plot.title = element_text(size = fontsize$title)) +
             geom_pointrange(aes(x = points, sqrt(variances),
                                 ymin = 0, ymax = sqrt(variances)), size = 1.25) 
    
  return(list(trait.plot = trait.plot, cv.plot = cv.plot, el.plot = el.plot, pv.plot = pv.plot))

}
##==================================================================================================#

format.plot.input <- function(plot.inputs, convert.var, trait.order=c()){
  traits <- row.names(as.data.frame(plot.inputs))
  if(length(trait.order) == 0){trait.order <- traits}
  plot.data <- data.frame(traits              = traits,
                          trait.labels        = trait.lookup(traits)$figid,
                          units               = trait.lookup(traits)$units,
                          coef.vars           = abs(plot.inputs$coef.vars * 100),
                          elasticities        = (plot.inputs$elasticities),
                          variances           = convert.var(abs(plot.inputs$variances)))
  plot.data <- merge(data.frame(traits=trait.order, points = seq(trait.order) - 0.5), plot.data)
  
  return(plot.data)
}


##--------------------------------------------------------------------------------------------------#
##' Plot univariate response of model output to a trait parameter.
##'
##' Plots for a single trait; called by \code{\link{plot.sensitivities}}
##' to plot sensitivity plots for multiple traits.
##' @name plot.sensitivity
##' @title Sensitivity plot 
##' @param sa.sample trait quantiles used in sensitivity analysis 
##' @param sa.spline spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param y.range 
##' @param median.i index of median value in sa.sample; \code{median.i == which(as.numeric(rownames(sa.sample)) == 50) }
##' @param prior.sa.sample similar to sa.sample, but for prior distribution. If given, plots sensitivity for prior run
##' @param prior.sa.spline similar to sa.spline, but for prior trait distribution. 
##' @param fontsize (optional) list with three arguments that can be set to vary the fontsize of the title, axis labels, and axis title in the sensitivity plots
##' @export
##' @return object of class ggplot
plot.sensitivity <- function(sa.sample, sa.spline, trait,
                             y.range = c(0,50), median.i = 4,
                             prior.sa.sample = NULL, prior.sa.spline = NULL,
                             fontsize = list(title = 12, axis = 8),
                             linesize = 1,
                             dotsize = 2) {
  LENGTH_OUT <- 1000
  
  units <- trait.lookup(trait)$units
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
            theme_bw() + ggtitle(trait.lookup(trait)$figid)
              theme(axis.text.x = element_text(size = fontsize$axis),
                   axis.text.y = element_text(size = fontsize$axis),
                   axis.title.x = element_text(size = fontsize$axis),
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = fontsize$title),
                   panel.border = element_blank())
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
  min.x <- min(prior.x)
  x.breaks <- pretty(c(min.x, max.x), 2)
  saplot <- saplot + scale_x_continuous(units, limits = range(x.breaks),
                                        breaks = x.breaks)
                                        #  print(saplot)
  return(saplot)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Plot functions and quantiles used in sensitivity analysis
##'
##' Generates a plot using \code{\link{plot.sensitivity}} for multiple traits.
##' @name plot.sensitivities 
##' @title Plot Sensitivities
##' @param sensitivity.plot.inputs inputs
##' @param prior.sensitivity.plot.inputs priors
##' @param ... arguments passed to \code{\link{plot.sensitivity}}
##' @param sensitivity.results list containing sa.samples and sa.splines
##' @export
##' @return list of plots, one per trait
plot.sensitivities <- function(sensitivity.plot.inputs,
                               prior.sensitivity.plot.inputs = NULL, ...){
  sa.samples <- sensitivity.plot.inputs$sa.samples
  sa.splines <- sensitivity.plot.inputs$sa.splines
  if(!is.null(prior.sensitivity.plot.inputs)) {
    prior.sa.samples <- prior.sensitivity.plot.inputs$sa.samples
    prior.sa.splines <- prior.sensitivity.plot.inputs$sa.splines
  }
  traits <- names(sa.samples)

#  y.range <- c(0, max(mapply(do.call, sa.splines, lapply(sa.samples, list)), na.rm = TRUE))
  y.range <- range(mapply(do.call, sa.splines, lapply(sa.samples, list)), na.rm = TRUE)

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
                                                  median.i =  which(as.numeric(rownames(sa.samples)) == 50),
                                                  prior.sa.sample = prior.sa.sample,
                                                  prior.sa.spline = prior.sa.spline,
                                                  ...)
  }
  return(sensitivity.plots)
}
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
