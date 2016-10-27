##' @name metric.scatter.plot
##' @title metric.scatter.plot
##' @export
##' @param metric_dat
##' @param var
##' 
##' @author Betsy Cowdery
metric.scatter.plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  
  library(ggplot2)
  
  p <- ggplot(data = metric_dat) 
  p <- p + geom_point(aes(x = model, y = obvs), size = 4) 
  p <- p + geom_abline(slope = 1, intercept = 0, colour = "#666666", 
                       size = 2, linetype = 2, lineend = "round")
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    plot(p)
  }

} # metric.scatter.plot
