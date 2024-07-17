#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------------#
##' Variance Decomposition Plots
##'
##' Plots variance decomposition tryptich
##' @name plot_variance_decomposition
##' @export
##' @author David LeBauer, Carl Davidson
##' @param plot.inputs Output from a sensitivity analysis. Output must be of the form
##'          given by sensitivity.results$variance.decomposition.output in model output
##' @param fontsize list specifying the font size of the titles and axes of the graph
##' @examples
##' x <- list(trait.labels = c('a', 'b', 'c'),
##'           coef.vars = c(a=1,b=0.5, c=0.1),
##'           elasticities = c(a=1,b=2,c=0.5),
##'           variances    = c(a = 20, b=30, c = 10))
##' do.call(gridExtra::grid.arrange, c(plot_variance_decomposition(x), ncol = 4))
plot_variance_decomposition <- function(plot.inputs, 
                                        fontsize = list(title = 18, axis = 14)) {
  ggplot2::theme_set(ggplot2::theme_classic() + ggplot2::theme(axis.text.x = ggplot2::element_text(size = fontsize$axis, vjust = -1),
                                    axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
                                    axis.line = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), 
                                    axis.title.y = ggplot2::element_blank(), 
                                    panel.grid.minor = ggplot2::element_blank(), 
                                    panel.border = ggplot2::element_blank()))
  
  traits <- names(plot.inputs$variances)
  units <- as.character(PEcAn.utils::trait.lookup(traits)$units)
  trait.labels <- as.character(PEcAn.utils::trait.lookup(traits)$figid)
  plot.data <- data.frame(trait.labels = ifelse(!is.na(trait.labels), 
                                                trait.labels, 
                                                traits), 
                          units = ifelse(!is.na(units), units, ""), 
                          coef.vars = plot.inputs$coef.vars * 100,
                          elasticities = plot.inputs$elasticities, 
                          variances = plot.inputs$variances, 
                          points = seq_along(traits) - 0.5)
  
  plot.data <- plot.data[order(plot.data$variances, decreasing = FALSE), ]
  
  base.plot <- ggplot2::ggplot(plot.data) + ggplot2::coord_flip()
  
  
  trait.plot <- base.plot + ggplot2::ggtitle("Parameter") + 
    ggplot2::geom_text(ggplot2::aes(y = 1, x = .data$points, label = trait.labels, hjust = 1), size = fontsize$axis/3) + 
    ggplot2::scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  cv.plot <- base.plot + ggplot2::ggtitle("CV (%)") + 
    ggplot2::geom_pointrange(ggplot2::aes(x = .data$points, y = .data$coef.vars, ymin = 0, ymax = .data$coef.vars), size = 1.25) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = fontsize$title))
  
  el.plot <- base.plot + ggplot2::ggtitle("Elasticity") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = fontsize$title)) + 
    ggplot2::geom_pointrange(ggplot2::aes(x = .data$points, y = .data$elasticities, ymin = 0, ymax = .data$elasticities), size = 1.25)
  
  pv.plot <- base.plot + ggplot2::ggtitle("Variance") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = fontsize$title)) + 
    ggplot2::geom_pointrange(ggplot2::aes(x = .data$points, sqrt(.data$variances), ymin = 0, ymax = sqrt(.data$variances)), size = 1.25)
  
  return(list(trait.plot = trait.plot, cv.plot = cv.plot, el.plot = el.plot, pv.plot = pv.plot))
} # plot_variance_decomposition



##--------------------------------------------------------------------------------------------------#
##' Plot univariate response of model output to a trait parameter.
##'
##' Plots for a single trait; called by \code{\link{plot_sensitivities}}
##' to plot sensitivity plots for multiple traits.
##' @name plot_sensitivity
##' @title Sensitivity plot 
##' @param sa.sample trait quantiles used in sensitivity analysis 
##' @param sa.spline spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param y.range limits for y axis of plot
##' @param median.i index of median value in sa.sample; \code{median.i == which(as.numeric(rownames(sa.sample)) == 50) }
##' @param prior.sa.sample similar to sa.sample, but for prior distribution. If given, plots sensitivity for prior run
##' @param prior.sa.spline similar to sa.spline, but for prior trait distribution. 
##' @param fontsize (optional) list with three arguments that can be set to vary the fontsize of the title, axis labels, and axis title in the sensitivity plots
##' @param linesize passed to ggplot to set line thickness
##' @param dotsize passed to ggplot to set point size
##'
##' @export
##' @return object of class ggplot
plot_sensitivity <- function(sa.sample, sa.spline, trait, y.range = c(0, 50), median.i = 4, 
                             prior.sa.sample = NULL, prior.sa.spline = NULL, 
                             fontsize = list(title = 12, axis = 8), 
                             linesize = 1, dotsize = 2) {
  LENGTH_OUT <- 1000
  
  units <- PEcAn.utils::trait.lookup(trait)$units
  saplot <- ggplot2::ggplot()
  
  post.x <- seq(from = min(sa.sample), to = max(sa.sample), length.out = LENGTH_OUT)
  
  saplot <- saplot + ## plot spline function
    ggplot2::geom_line(ggplot2::aes(x=.data$x, y=.data$y), data = data.frame(x = post.x, y = sa.spline(post.x)), size = linesize) + 
    ## plot points used to evaluate spline
    ggplot2::geom_point(ggplot2::aes(x=.data$x, y=.data$y), data = data.frame(x = sa.sample, y = sa.spline(sa.sample)), 
               size = dotsize) + # indicate median with larger point
    ggplot2::geom_point(ggplot2::aes(x = .data$x, y=.data$y), data = data.frame(x = sa.sample[median.i], y = sa.spline(sa.sample[median.i])), 
               size = dotsize * 1.3) + 
    ggplot2::scale_y_continuous(limits = range(pretty(y.range)), breaks = pretty(y.range, n = 3)[1:3]) +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle(trait) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = fontsize$axis),
          axis.text.y = ggplot2::element_text(size = fontsize$axis), 
          axis.title.x = ggplot2::element_text(size = fontsize$axis),
          axis.title.y = ggplot2::element_blank(), 
          plot.title = ggplot2::element_text(size = fontsize$title), 
          panel.border = ggplot2::element_blank())
  
  ## Following conditional can be removed to only plot posterior sa
  prior.x <- post.x
  if (!is.null(prior.sa.sample) & !is.null(prior.sa.spline)) {
    prior.x <- seq(from = min(prior.sa.sample), to = max(prior.sa.sample), length.out = LENGTH_OUT)
    saplot <- saplot + ## plot spline
      ggplot2::geom_line(ggplot2::aes(x = .data$x, y= .data$y), data = data.frame(x = prior.x, y = prior.sa.spline(prior.x)), 
                         size = linesize, color = "grey") + ## plot points used to evaluate spline
      ggplot2::geom_point(ggplot2::aes(x= .data$x, y= .data$y), data = data.frame(x = prior.sa.sample, y = prior.sa.spline(prior.sa.sample)), 
                          size = dotsize, color = "grey") + ## indicate location of medians
      ggplot2::geom_point(ggplot2::aes(x = .data$x, y= .data$y), data = data.frame(x = prior.sa.sample[median.i], y = prior.sa.spline(prior.sa.sample[median.i])), 
                          size = dotsize * 1.5, color = "grey")
  }
  max.x <- max(prior.x)
  min.x <- min(prior.x)
  x.breaks <- pretty(c(min.x, max.x), 2)
  saplot <- saplot + ggplot2::scale_x_continuous(units, limits = range(x.breaks), breaks = x.breaks)
  # print(saplot)
  return(saplot)
} # plot_sensitivity


##--------------------------------------------------------------------------------------------------#
##' Plot functions and quantiles used in sensitivity analysis
##'
##' Generates a plot using \code{\link{plot_sensitivity}} for multiple traits.
##'
##' @param sensitivity.plot.inputs inputs
##' @param prior.sensitivity.plot.inputs priors
##' @param ... arguments passed to \code{\link{plot_sensitivity}}
##' @export
##' @return list of plots, one per trait
plot_sensitivities <- function(sensitivity.plot.inputs, 
                               prior.sensitivity.plot.inputs = NULL, ...) {
  sa.samples <- sensitivity.plot.inputs$sa.samples
  sa.splines <- sensitivity.plot.inputs$sa.splines
  if (!is.null(prior.sensitivity.plot.inputs)) {
    prior.sa.samples <- prior.sensitivity.plot.inputs$sa.samples
    prior.sa.splines <- prior.sensitivity.plot.inputs$sa.splines
  }
  traits <- names(sa.samples)
  
  # y.range <- c(0, max(mapply(do.call, sa.splines, lapply(sa.samples, list)),
  # na.rm = TRUE))
  y.range <- range(mapply(do.call, sa.splines, lapply(sa.samples, list)), na.rm = TRUE)
  
  sensitivity.plots <- list()
  for (trait in traits) {
    if (!is.null(prior.sensitivity.plot.inputs)) {
      prior.sa.sample <- prior.sa.samples[, trait]
      prior.sa.spline <- prior.sa.splines[[trait]]
    } else {
      prior.sa.sample <- NULL
      prior.sa.spline <- NULL
    }
    sensitivity.plots[[trait]] <- plot_sensitivity(sa.sample = sa.samples[, trait], 
                                                   sa.spline = sa.splines[[trait]], 
                                                   trait <- trait, 
                                                   y.range = y.range, 
                                                   median.i = which(as.numeric(rownames(sa.samples)) == 50),
                                                   prior.sa.sample = prior.sa.sample,
                                                   prior.sa.spline = prior.sa.spline, 
                                                   ...)
  }
  return(sensitivity.plots)
} # plot_sensitivities
