#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##'
##'
##' @name plot.variance.decomposition
##' @title Plot results of variance decomposition
##' @export
##' @author David LeBauer
plot.variance.decomposition <- function(plot.inputs, outdir,
                                        prior.plot.inputs = NULL,
                                        fontsize = list(title = 18, axis = 14),
                                        filter = TRUE,
                                        variance.scale=sqrt, 
                                        #ex: log, sqrt, identity 
                                        variance.prefix='Root'){
                                        #EX: Log, Root, Partial
  
  traits    <- names(plot.inputs$variances)
  .plot.data <- data.frame(trait.labels  = merge(data.frame(id = traits), trait.dictionary(traits), by = 'id', sort = FALSE)$figid,
                           units         = trait.dictionary(traits)$units,
                           coef.vars     = abs(plot.inputs$coef.vars * 100),
                           elasticities  = abs(plot.inputs$elasticities),
                           variances     = variance.scale(abs(plot.inputs$variances)))[filter,]
                                       #  recover()
  if(!is.null(prior.plot.inputs)) {
    prior.traits <- names(prior.plot.inputs$partial.variances)
    prior.matched <- prior.traits %in% traits
    post.matched <- traits %in% prior.traits
    
    prior.plot.data <- data.frame(trait.labels              = trait.dictionary(prior.traits)$figid,
                                  units                     = trait.dictionary(prior.traits)$units,
                                  prior.coef.vars           = abs(prior.plot.inputs$coef.vars * 100),
                                  prior.elasticities        = abs(prior.plot.inputs$elasticities),
                                  prior.variances           = variance.scale(abs(prior.plot.inputs$variances)))
    .plot.data <- merge(.plot.data, prior.plot.data, by = 'trait.labels')
    pv.order <- order(.plot.data$prior.variances, decreasing = FALSE)
  }
  else {
    pv.order <- order(.plot.data$variances, decreasing = FALSE)
  }

  ## location of words and lollipops set by 'points'
  ##    these points can be moved up or down by adjusting the offset X in 1:length(traits) - X
  plot.data <- data.frame(.plot.data[pv.order, ], points = 1:nrow(.plot.data) - 0.5)
  trait.labels <<- plot.data$trait.labels
  cv.xticks <<- pretty(plot.data[,grep('coef.var', colnames(plot.data))], 4)
  pv.xticks <<- pretty(plot.data[,grep('variance', colnames(plot.data))], 4)  
  el.xticks <<- pretty(plot.data[,grep('elasticities', colnames(plot.data))], 3)
  el.xrange <<- range(pretty(plot.data[,grep('elasticities', colnames(plot.data))], 4))
  
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
                                          xend = c(length(trait.labels), 0)), colour = 'white')  + 
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
                                           xend = c(length(trait.labels), 0)))  + 
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
                                           xend = c(length(trait.labels), 0)))  +
                                             ## Add Ticks
                                             geom_segment(aes(x = 0,
                                                              y = el.xticks,
                                                              xend = -0.1,
                                                              yend = el.xticks)) 

  pv.plot <- .pv.plot + 
    opts(title = paste(variance.prefix, 'Variance (Mg/ha)'),
         plot.title = theme_text(size = fontsize$title)) +
           scale_y_continuous(breaks = pv.xticks, limits = range(pv.xticks)) +
             geom_pointrange(aes(x = points, variances,
                                 ymin = 0, ymax = variances), size = 1.25) +
                                   ##  Add Axes
                                   geom_segment(aes(x = c(0,0), y = c(0,0),
                                                    yend = c(0, max(pv.xticks)),
                                                    xend = c(length(trait.labels), 0)))  + 
                                                      ## Add Ticks
                                                      geom_segment(aes(x = 0,
                                                                       y = pv.xticks,
                                                                       xend = -0.1,
                                                                       yend = pv.xticks))
  
  
  return(list(trait.plot = trait.plot, cv.plot = cv.plot, el.plot = el.plot, pv.plot = pv.plot))


}
##==================================================================================================#


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
