##--------------------------------------------------------------------------------------------------#
##' Plots a prior density from a parameterized probability distribution
##'
##' @param prior.density data frame containing columns x and y
##' @param base.plot a ggplot object (grob), created if none provided
##' @param prior.color color of line to be plotted
##' @return plot with prior density added
##' @seealso \code{\link{pr.dens}}
##' @author David LeBauer
##' @export
##' @aliases plot.prior.density
##' @examples
##' \dontrun{
##' plot_prior.density(pr.dens('norm', 0, 1))
##' }
plot_prior.density <- function(prior.density, base.plot = NULL, prior.color = "black") {
  if (is.null(base.plot)) {
    base.plot <- ggplot2::ggplot()
  }
  new.plot <- base.plot + geom_line(data = prior.density, aes(x = x, y = y), color = prior.color)
  return(new.plot)
} # plot_prior.density


##--------------------------------------------------------------------------------------------------#
##'  Add posterior density to a plot
##'
##' @param posterior.density data frame containing columns x and y
##' @param base.plot a ggplot object (grob), created if none provided
##' @return plot with posterior density line added
##' @aliases plot.posterior.density
##' @export
##' @importFrom ggplot2 geom_line aes
##' @author David LeBauer
plot_posterior.density <- function(posterior.density, base.plot = NULL) {
  if (is.null(base.plot)) {
    base.plot <- ggplot2::ggplot()
  }
  new.plot <- base.plot + geom_line(data = posterior.density, aes(x = x, y = y))
  return(new.plot)
} # plot_posterior.density


##--------------------------------------------------------------------------------------------------#
##' Plot prior density and data
##'
##' @param priordata observations to be plotted as points
##' @param priordensity density of prior distribution, calculated by `prior.density`
##' @param trait dataframe with id, figid and units of the trait
##' @param xlim limits for x axis
##' @param fontsize passed to ggplot to set text size
##' @author David LeBauer
##' @return plot / grob of prior distribution with data used to inform the distribution
##' @md
##' @export
##' @importFrom ggplot2 ggplot aes theme_bw scale_x_continuous scale_y_continuous element_blank element_text geom_rug geom_line geom_point
priorfig <- function(priordata = NA, priordensity = NA, trait = NA, xlim = "auto", fontsize = 18) {
  if (is.data.frame(priordata)) {
    colnames(priordata) <- "x"
  }

  if (isTRUE(xlim == "auto")) {
    x.breaks <- pretty(c(signif(priordensity$x, 2)), 4)
    xlim     <- range(x.breaks)
  } else {
    x.breaks <- pretty(signif(xlim, 2), 4)
    xlim     <- range(c(x.breaks, xlim))
  }

  priorfigure <- ggplot() + theme_bw() +
    scale_x_continuous(limits = xlim, breaks = x.breaks, name = trait$units) +
    scale_y_continuous(breaks = NULL) +
    labs(title = trait$figid) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),    ## hide y axis label
          axis.text.x = element_text(size = fontsize),
          axis.title.y = element_blank(),   ## hide y axis label
          axis.title.x = element_text(size = fontsize * 0.9),
          plot.title = element_text(size = fontsize * 1.1))

  if (is.data.frame(priordata)) {
    priordata   <- subset(priordata, subset = !is.na(x))
    dx          <- with(priordata, min(abs(diff(x)[diff(x) != 0])))
    ## add jitter to separate equal values
    priordata$x <- priordata$x + stats::runif(length(priordata$x), -dx / 2, dx / 2)
    rug         <- geom_rug(data = priordata, aes(x))
    priorfigure <- priorfigure + rug
  }
  if (is.data.frame(priordensity[1])) {
    dens.line   <- geom_line(data = priordensity, aes(x, y))
    qpts        <- get.quantiles.from.density(priordensity)
    dens.ci     <- geom_point(data = qpts, aes(x, y))
    priorfigure <- priorfigure + dens.line + dens.ci
  }
  return(priorfigure)
} # priorfig


##--------------------------------------------------------------------------------------------------#
##' Plot trait density and data
##'
##' @param trait dataframe with id, figid and units of the trait to be plotted
##' @param prior named distribution with parameters
##' @param posterior.sample samples from posterior distribution
##'   whose density should be plotted
##' @param trait.df data to be plotted, in a format accepted by
##'   \code{\link[PEcAn.MA]{jagify}}
##' @param fontsize,x.lim,y.lim,logx passed on to ggplot
##' @return plot (grob) object
##' @author David LeBauer
##' @importFrom ggplot2 aes labs element_text element_blank geom_segment
##'    scale_x_continuous theme theme_bw
##' @export plot_trait
##' @aliases plot.trait
##' @examples
##' \dontrun{
##' prior1 <- data.frame(distn = 'norm',
##'                      parama = 20,
##'                      paramb = 5)
##' data1  <- data.frame(Y = c(19, 21), se = c(1,1))
##' trait1 <- data.frame(id = 'Vcmax', figid = 'Vcmax', units = 'umol CO2 m-2 s-1')
##' plot_trait(trait = trait1,
##'           prior = prior1,
##'           trait.df = data1)
##' }
plot_trait <- function(trait,
                       prior = NULL,
                       posterior.sample = NULL,
                       trait.df = NULL,
                       fontsize = list(title = 18, axis = 14),
                       x.lim = NULL,
                       y.lim = NULL,
                       logx = FALSE) {

  if (!requireNamespace("PEcAn.visualization", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "plot_trait requires package `PEcAn.visualization`,",
      "but it is not installed. Please install it and try again.")
  }

  ## Determine plot components
  plot_posterior <- !is.null(posterior.sample)
  plot_prior     <- !is.null(prior)
  plot_data      <- !is.null(trait.df)

  if(plot_data)  trait.df <- PEcAn.MA::jagify(trait.df)

  if (plot_prior) {
    prior.color   <- ifelse(plot_posterior, "grey", "black")
    prior.density <- create.density.df(distribution = prior)
    prior.density <- prior.density[prior.density$x > 0, ]
  } else {
    prior.density <- data.frame(x = NA, y = NA)
  }
  if (plot_posterior) {
    posterior.density <- create.density.df(samps = posterior.sample)
    posterior.density <- posterior.density[posterior.density$x > 0, ]
  } else {
    posterior.density <- data.frame(x = NA, y = NA)
  }

  if (is.null(x.lim)) {
    if (!is.null(trait.df)) {
      data.range <- max(c(trait.df$Y, trait.df$Y + trait.df$se), na.rm = TRUE)
    } else {
      data.range <- NA
    }
    x.lim <- range(c(prior.density$x, data.range), na.rm = TRUE)
  }
  if (is.null(y.lim)) {
    y.lim <- range(posterior.density$y, prior.density$y, na.rm = TRUE)
  }

  x.ticks <- pretty(c(0, x.lim[2]))

  base.plot <- ggplot2::ggplot() + theme_bw()
  if (plot_prior) {
    base.plot <- plot_prior.density(prior.density, base.plot = base.plot, prior.color = prior.color)
  }
  if (plot_posterior) {
    base.plot <- plot_posterior.density(posterior.density, base.plot = base.plot)
  }
  if (plot_data) {
    base.plot <- PEcAn.visualization::plot_data(trait.df, base.plot = base.plot, ymax = y.lim[2])
  }

  trait.plot <- base.plot +
    geom_segment(aes(x = min(x.ticks), xend = max(x.ticks), y = 0, yend = 0)) +
    scale_x_continuous(limits = range(x.ticks), breaks = x.ticks, name = trait$units) +
    labs(title = trait$figid) +
    theme(axis.text.x = element_text(size = fontsize$axis),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = fontsize$axis),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = fontsize$title),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  return(trait.plot)
} # plot_trait


##--------------------------------------------------------------------------------------------------#
##' Plot probability density and data
##'
##' @export
##' @aliases plot.densities
##' @param density.plot_inputs list containing trait.samples and trait.df
##' @param ... passed on to plot_density
##' @param outdir directory in which to generate figure as pdf
##' @author David LeBauer
##' @return outputs plots in outdir/sensitivity.analysis.pdf file
plot_densities <- function(density.plot_inputs, outdir, ...) {
  trait.samples       <- density.plot_inputs$trait.samples
  trait.df            <- density.plot_inputs$trait.df
  prior.trait.samples <- density.plot_inputs$trait.df

  traits <- names(trait.samples)
  grDevices::pdf(paste0(outdir, "trait.densities.pdf"), height = 12, width = 20)

  for (trait in traits) {
    density.plot <- plot_density(trait.sample = trait.samples[, trait],
                                 trait.df = trait.df[[trait]], ...)
    print(density.plot)
  }
  grDevices::dev.off()
} # plot_densities


##--------------------------------------------------------------------------------------------------#
#' Get the quantiles from prior density
#'
#' Finds quantiles on a density data frame
#'
#' @param density.df density dataframe generated by \code{\link{create.density.df}}
#' @param quantiles default is the median and 95\% CI;
#' @author David LeBauer
#' @export
#' @examples
#' prior.df <- create.density.df(distribution = list('norm',0,1))
#' get.quantiles.from.density(prior.df)
#' samp.df <- create.density.df(samps = rnorm(100))
#' get.quantiles.from.density(samp.df)
get.quantiles.from.density <- function(density.df, quantiles = c(0.025, 0.5, 0.975)) {
  colnames(density.df) <- c("prior.x", "dens.x")
  density.df$prob.x    <- density.df$dens.x / sum(density.df$dens.x)
  qi <- sapply(quantiles, function(x) which.min(abs(cumsum(density.df$prob.x) - x)))
  qs <- density.df[qi, c("prior.x", "dens.x")]
  colnames(qs) <- c("x", "y")
  return(qs)
} # get.quantiles.from.density
