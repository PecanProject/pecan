plot.sa <- function (satables, outvar) {
  ## Set up dataframes for figs
  pr <- satables[['prior']][[outvar]]
  po <- satables[['post']][[outvar]]
  data = data.frame(
    trait = pr$figid,
    pr.cv = pr$cv.theta, #prior coef. var
    po.cv = po$cv.theta,  #post  " "
    pr.el = pr$elast,     #prior elasticity
    po.el = po$elast,     #post  "
    pr.ev = pr$per.var,  #prior, explained variance by parameter i
    po.ev = po$rel.var,  #post   "         "        "  " 
    null  = pr$null)     #dummy for label plot
  ## fig. parameters
  cv.ymax <- max(abs(data[,c('pr.cv', 'po.cv')])) * 1.1 #po.cv =< pr.cv by definition
  el.ymax <- max(abs(data[,c('pr.el', 'po.el')])) * 1.1
  el.ymin <- -el.ymax
  ev.ymax <- max(data[,c('pr.ev', 'po.ev')]) * 1.1
  fontsize = 14 

  base.plot <- ggplot(data) +
    coord_flip() +
      theme_bw() +
        scale_y_continuous(expand = c(0,0)) + 
          opts(axis.line = theme_segment(),
               axis.text.x = theme_text(size=fontsize),
               axis.text.y = theme_text(size=0.01),
               axis.title.x = theme_blank(), 
               axis.title.y = theme_blank(),
               ## theme_text(size = 20),
               ## axis.ticks = theme_blank(),
               panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank(),
               panel.border = theme_blank())
         
  trait.plot <- base.plot +
    opts( title = outvar) +
         geom_text(aes(y = 1, x = seq(nrow(data)), label=trait), data=data, hjust = 1) +
         scale_y_continuous( breaks = c(0,0),
                            limits = c(0,1))
 
  cv.plot <- base.plot +
    opts( title = 'CV') +
      geom_pointrange(aes(seq(nrow(data)), pr.cv, ymin = 0, ymax = pr.cv ), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.cv, ymin = 0, ymax = po.cv), size = 1.25) +
          scale_y_continuous(breaks =  seq(0, cv.ymax, by=0.2), 
                             limits = c(0, cv.ymax)) 
  

  el.plot <- base.plot +
    opts( title = 'Elasticity') +
      geom_pointrange(aes(seq(nrow(data)), pr.el, ymin = 0, ymax = pr.el), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.el, ymin = 0, ymax = po.el), size = 1.25) +
          scale_y_continuous(#breaks =  seq(el.ymin, el.ymax, by=???), 
                             limits = c(el.ymin, el.ymax)) 


  ev.plot <- base.plot +
    opts( title = 'Explained Variance') +
      geom_pointrange(aes(seq(nrow(data)), pr.ev, ymin = 0, ymax = pr.ev), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.ev, ymin = 0, ymax = po.ev), size = 1.25) +
          scale_y_continuous(#breaks =  seq(0, ev.ymax, by=ev.ymax/5), 
                             limits = c(0, ev.ymax)) 

  
  return(grid.arrange(trait.plot, cv.plot, el.plot, ev.plot, ncol=4))
}
