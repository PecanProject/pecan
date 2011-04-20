##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x.values trait quantiles used in sensitivity analysis
##' @param fun spline function estimated from sensitivity analysis
##' @param trait trait name for title
##' @param prior.x.values if given, plots sensitivity for prior run 
##' @param prior.fun if given, plots sensitivity for prior run
##' @return object of class ggplot
sensitivity.plot <- function(x.values, fun, trait, prior.x.values = NA, prior.fun = NA) {
  print(x.values)
  x <- seq(from = min(x.values), to = max(x.values), length.out = 100)
  line.data <- data.frame(x = x, y = fun(x))
  point.data <- data.frame(x = x.values, y = fun(x.values))
  range(x)
  range(x.values)
  print(line.data)
  saplot <- ggplot() +
    geom_line(data = line.data, aes(x, y), size = 2) +
      geom_point(data = point.data, aes(x, y), size = 3) +
        scale_y_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30)) +
          theme_bw() +
            opts(title=trait.dictionary(trait)$figid,
                 axis.text.x = theme_text(size=14),
                 axis.text.y = theme_text(size=14),
                 axis.title.x = theme_blank(), 
                 axis.title.y = theme_blank(),
                 plot.title = theme_text(size = 20),
                 panel.border = theme_blank())
  if(!is.na(prior.x.values) & !is.na(prior.fun)){
    x <- seq(from = min(prior.x.values), to = max(prior.x.values), length.out = 100)
    saplot <- saplot +
      geom_line(aes(x, fun(x)), size = 2, color = 'grey') +
        geom_point(aes(x.values, fun(x.values)), size = 3, color = 'grey')
  }
  return(saplot)
}

#        scale_x_continuous(limits = ifelse(diff(range(x))/min(x) < 0.25 ,
#                             c(0, max(x) * 1.1),
#                             c(0.9, 1.1) * range(x))) 
#                            
#                               coord_cartesian(x=c(0, max(x)*1.1))


## geom_line(aes(x, y),
##           color = 'grey',
##           size = 2,
##           data = lpr ) +
##             geom_point(aes(x,y),
##                        data = dpr,
##                        color = 'grey',
##                        size = 3) +
##                          geom_line(aes(x, y),                                                       
##                                    color = 'black',
##                                    size = 2,
##                                    data = lpo) +
##                                      geom_point(aes(x,y),
##                                                 data=dpo,
##                                                 color = 'black',
##                                                 size = 3) +
##                                                   geom_point(aes(x,y), data= dpr[4,], color = 'grey', size = 5) +
##                                                     geom_point(aes(x,y), data= dpo[4,], color = 'black', size = 5) + scale_shape(solid=FALSE) +

