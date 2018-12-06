##' @name metric_lmDiag_plot
##' @title Linear Regression Diagnostic Plot
##' @export
##' @param metric_dat data.frame
##' 
##' @author Betsy Cowdery
metric_lmDiag_plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  PEcAn.logger::logger.info("Metric: Linear Regression Diagnostic Plot")
  
  fit <- lm(metric_dat[, 1] ~ metric_dat[, 2])

  p1 <- ggplot2::ggplot(fit, aes(.fitted, .resid))
  p1 <- p1 + ggplot2::geom_point()
  p1 <- p1 + ggplot2::stat_smooth(method = "loess") 
  p1 <- p1 + ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed")
  p1 <- p1 + ggplot2::xlab("Fitted values") 
  p1 <- p1 + ggplot2::ylab("Residuals")
  p1 <- p1 + ggplot2::ggtitle("Residual vs Fitted Plot") 
  p1 <- p1 + ggplot2::theme_bw()
  
  p2 <- ggplot2::ggplot(fit, aes(qqnorm(.stdresid)[[1]], .stdresid)) 
  p2 <- p2 + ggplot2::geom_point(na.rm = TRUE)
  p2 <- p2 + ggplot2::geom_abline(aes(qqline(.stdresid))) 
  p2 <- p2 + ggplot2::xlab("Theoretical Quantiles") 
  p2 <- p2 + ggplot2::ylab("Standardized Residuals")
  p2 <- p2 + ggplot2::ggtitle("Normal Q-Q") 
  p2 <- p2 + ggplot2::theme_bw()
  
  p3 <- ggplot2::ggplot(fit, aes(.fitted, sqrt(abs(.stdresid)))) 
  p3 <- p3 + ggplot2::geom_point(na.rm = TRUE)
  p3 <- p3 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE) 
  p3 <- p3 + ggplot2::xlab("Fitted Value")
  p3 <- p3 + ggplot2::ylab(expression(sqrt("|Standardized residuals|")))
  p3 <- p3 + ggplot2::ggtitle("Scale-Location") 
  p3 <- p3 + ggplot2::theme_bw()
  
  p4 <- ggplot2:: ggplot(fit, aes(seq_along(.cooksd), .cooksd)) 
  p4 <- p4 + ggplot2::geom_bar(stat = "identity", position = "identity")
  p4 <- p4 + ggplot2::xlab("Obs. Number") 
  p4 <- p4 + ggplot2::ylab("Cook's distance")
  p4 <- p4 + ggplot2::ggtitle("Cook's distance") 
  p4 <- p4 + ggplot2::theme_bw()
  
  p5 <- ggplot2::ggplot(fit, aes(.hat, .stdresid)) 
  p5 <- p5 + ggplot2::geom_point(aes(size = .cooksd), na.rm = TRUE)
  p5 <- p5 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE)
  p5 <- p5 + ggplot2::xlab("Leverage") 
  p5 <- p5 + ggplot2::ylab("Standardized Residuals")
  p5 <- p5 + ggplot2::ggtitle("Residual vs Leverage Plot")
  p5 <- p5 + ggplot2::scale_size_continuous("Cook's Distance", range = c(1, 5))
  p5 <- p5 + ggplot2::theme_bw() 
  p5 <- p5 + ggplot2::theme(legend.position = "bottom")
  
  p6 <- ggplot2::ggplot(fit, aes(.hat, .cooksd)) 
  p6 <- p6 + ggplot2::geom_point(na.rm = TRUE) 
  p6 <- p6 + ggplot2::stat_smooth(method = "loess", na.rm = TRUE)
  p6 <- p6 + ggplot2::xlab("Leverage hii") 
  p6 <- p6 + ggplot2::ylab("Cook's Distance")
  p6 <- p6 + ggplot2::ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6 <- p6 + ggplot2::geom_abline(slope = seq(0, 3, 0.5), color = "gray", linetype = "dashed")
  p6 <- p6 + ggplot2::theme_bw()
  
  p <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
  
    if (!is.na(filename)) {
      pdf(filename, width = 10, height = 6)
      plot(p)
      dev.off()
    }
  
  if (draw.plot) {
    plot(p)
  }
  
} # metric_lmDiag_plot
