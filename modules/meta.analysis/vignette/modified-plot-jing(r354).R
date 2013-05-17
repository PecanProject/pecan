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
    theme_bw() +
    theme(title= trait.lookup(trait)$figid, 
         axis.text.x = element_text(size = fontsize$axis),
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