
#' Variance Decomposition Plots
#'
#' Plots variance decomposition tryptich: CV, elasticity, variance
#' @name plot_variance_decomposition
#' @export
#' @author David LeBauer, Carl Davidson, Chris Black
#'
#' @param plot.inputs Output from a sensitivity analysis. Output must be of the form
#'          given by sensitivity.results$variance.decomposition.output in model output
#' @param fontsize list specifying the font size of the titles and axes of the graph
#' @param order_by Result column to sort by, or "none" to retain input order
#'
#' @return ggplot object
#' @examples
#' x <- list(trait.labels = c('a', 'b', 'c'),
#'           coef.vars = c(a=1,b=0.5, c=0.1),
#'           elasticities = c(a=1,b=2,c=0.5),
#'           variances    = c(a = 20, b=30, c = 10))
#' plot_variance_decomposition(x)
#' plot_variance_decomposition(x, order_by = "rowname")
plot_variance_decomposition <- function(plot.inputs,
                                        fontsize = list(title = 18, axis = 14),
                                        order_by = c("Variance", "Elasticity",
                                                     "CV (%)", "rowname", "none")) {
  sort_cols <- match.arg(order_by, several.ok = TRUE)
  if (any(sort_cols != "none")) {
    # Can't retain original order when sorting by anything else
    sort_cols <- sort_cols[sort_cols != "none"]
  }

  dat <- as.data.frame(plot.inputs)
  dat$rowname <- rownames(dat)
  dat <- dat |>
    dplyr::mutate(
      rowname = dplyr::coalesce(PEcAn.utils::trait.lookup(.data$rowname)$figid,
                                .data$rowname),
      "CV (%)" = .data$coef.vars * 100,
      Variance = sqrt(.data$variances)
    ) |>
    dplyr::select(
      "rowname",
      "CV (%)",
      Elasticity = "elasticities",
      "Variance"
    )

  if ("none" %in% sort_cols) {
    dat$roworder <- seq_along(dat$rowname)
  } else {
    dat$roworder <- match(
      x = seq_len(nrow(dat)),
      table = do.call("order", dat[sort_cols])
    )
  }

  dat <- tidyr::pivot_longer(dat, c(-"rowname", -"roworder"))

  # Set point and line weight
  # (`element_geom` didn't exist before ggplot 4.0, `fatten` is deprecated after)
  lollipops <- function() {
    if (utils::packageVersion("ggplot2") >= "4.0.0") {

      # getExportedValue is to quiet an R CMD check "missing or unexported object"
      #   warning when checked on a system with ggplot2 < 4.0.
      # Never mind that the if skips this whole block on those systems...
      elem_geom = getExportedValue(ns = "ggplot2", name = "element_geom")

      return(list(
        ggplot2::geom_pointrange(),
        ggplot2::theme(
          geom = elem_geom(pointsize = ggplot2::rel(3),
                           linewidth = ggplot2::rel(1.25)))
      ))
    } else {
      return(ggplot2::geom_pointrange(fatten = 10))
    }
  }

  ggplot2::ggplot(dat) +
    ggplot2::aes(
      y = stats::reorder(.data$rowname, .data$roworder),
      x = .data$value,
      xmin = 0,
      xmax = .data$value
    ) +
    ggplot2::facet_wrap(~name, scales = "free_x") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = fontsize$axis, vjust = -1),
      axis.text.y = ggplot2::element_text(size = fontsize$axis, hjust = 1),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = fontsize$title)
    ) +
    lollipops()
}




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
    ggplot2::geom_line(ggplot2::aes(x=.data$x, y=.data$y), data = data.frame(x = post.x, y = sa.spline(post.x)), linewidth = linesize) + 
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
                         linewidth = linesize, color = "grey") + ## plot points used to evaluate spline
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
