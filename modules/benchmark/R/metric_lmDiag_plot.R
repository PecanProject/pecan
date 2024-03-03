##' @name metric_lmDiag_plot
##' @title Linear Regression Diagnostic Plot
##' @export
##' @param metric_dat data.frame
##' @param var ignored
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: return plot object?
##' 
##' @author Betsy Cowdery
metric_lmDiag_plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  PEcAn.logger::logger.info("Metric: Linear Regression Diagnostic Plot")
  
  fit <- stats::lm(metric_dat[, 1] ~ metric_dat[, 2])

  p1 <- ggplot2::ggplot(fit, ggplot2::aes(.data$.fitted, .data$.resid))
  p1 <- p1 + ggplot2::geom_point()
  p1 <- p1 + ggplot2::stat_smooth(method = "loess") 
  p1 <- p1 + ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed")
  p1 <- p1 + ggplot2::xlab("Fitted values") 
  p1 <- p1 + ggplot2::ylab("Residuals")
  p1 <- p1 + ggplot2::ggtitle("Residual vs Fitted Plot") 
  p1 <- p1 + ggplot2::theme_bw()
  
  p2 <- ggplot2::ggplot(fit, ggplot2::aes(stats::qqnorm(.data$.stdresid)[[1]], .data$.stdresid)) 
  p2 <- p2 + ggplot2::geom_point(na.rm = TRUE)
  p2 <- p2 + ggplot2::geom_abline(ggplot2::aes(stats::qqline(.data$.stdresid))) 
  p2 <- p2 + ggplot2::xlab("Theoretical Quantiles") 
  p2 <- p2 + ggplot2::ylab("Standardized Residuals")
  p2 <- p2 + ggplot2::ggtitle("Normal Q-Q") 
  p2 <- p2 + ggplot2::theme_bw()
  
  p3 <- ggplot2::ggplot(fit, ggplot2::aes(.data$.fitted, sqrt(abs(.data$.stdresid))))
  p3 <- p3 + ggplot2::geom_point(na.rm = TRUE)
  p3 <- p3 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE) 
  p3 <- p3 + ggplot2::xlab("Fitted Value")
  p3 <- p3 + ggplot2::ylab(expression(sqrt("|Standardized residuals|")))
  p3 <- p3 + ggplot2::ggtitle("Scale-Location") 
  p3 <- p3 + ggplot2::theme_bw()
  
  p4 <- ggplot2:: ggplot(fit, ggplot2::aes(seq_along(.data$.cooksd), .data$.cooksd)) 
  p4 <- p4 + ggplot2::geom_bar(stat = "identity", position = "identity")
  p4 <- p4 + ggplot2::xlab("Obs. Number") 
  p4 <- p4 + ggplot2::ylab("Cook's distance")
  p4 <- p4 + ggplot2::ggtitle("Cook's distance") 
  p4 <- p4 + ggplot2::theme_bw()
  
  p5 <- ggplot2::ggplot(fit, ggplot2::aes(.data$.hat, .data$.stdresid)) 
  p5 <- p5 + ggplot2::geom_point(ggplot2::aes(size = .data$.cooksd), na.rm = TRUE)
  p5 <- p5 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE)
  p5 <- p5 + ggplot2::xlab("Leverage") 
  p5 <- p5 + ggplot2::ylab("Standardized Residuals")
  p5 <- p5 + ggplot2::ggtitle("Residual vs Leverage Plot")
  p5 <- p5 + ggplot2::scale_size_continuous("Cook's Distance", range = c(1, 5))
  p5 <- p5 + ggplot2::theme_bw() 
  p5 <- p5 + ggplot2::theme(legend.position = "bottom")
  
  p6 <- ggplot2::ggplot(fit, ggplot2::aes(.data$.hat, .data$.cooksd)) 
  p6 <- p6 + ggplot2::geom_point(na.rm = TRUE) 
  p6 <- p6 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE)
  p6 <- p6 + ggplot2::xlab("Leverage hii") 
  p6 <- p6 + ggplot2::ylab("Cook's Distance")
  p6 <- p6 + ggplot2::ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6 <- p6 + ggplot2::geom_abline(slope = seq(0, 3, 0.5), color = "gray", linetype = "dashed")
  p6 <- p6 + ggplot2::theme_bw()
  
  p <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
  
    if (!is.na(filename)) {
      grDevices::pdf(filename, width = 10, height = 6)
      plot(p)
      grDevices::dev.off()
    }
  
  if (draw.plot) {
    plot(p)
  }
  
} # metric_lmDiag_plot
