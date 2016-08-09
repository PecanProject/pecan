##' @name metric.residual.plot
##' @title metric.residual.plot
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.residual.plot <- function(dat, var,filename = NA, draw.plot = FALSE){
  
  library(ggplot2)
  # 
  # ind <- intersect(which(!is.na(dat$obvs)),  which(!is.na(dat$model)))
  # 
  # dat <- dat[ind,]
  
  dat$time <- lubridate::year(as.Date(as.character(dat$time), format="%Y"))
  dat$diff <- abs(dat$model - dat$obvs)
  dat$zeros <- rep(0, length(time))
  
  p <- ggplot(data = dat, aes(x=time)) + 
    geom_path(aes(y=zeros), colour = "#666666", size=2, linetype = 2, lineend = "round") +
    geom_point(aes(y=diff), size=4,  colour = "#619CFF") + labs(title=var, x= "years", y="abs(model - observation)")
  
  if(!is.na(filename)){
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }  
  
  if(draw.plot){
    plot(p)
  }

}

# ind <- intersect(which(!is.na(dat$obvs)),  which(!is.na(dat$model)))
# plot(dat$model[ind]-dat$obvs[ind], ylim = c(-max(dat$model[ind]-dat$obvs[ind]),max(dat$model[ind]-dat$obvs[ind])))
# abline(h=0)
