plot.sa <- function (satables, runtype, outvar) {

  plot.tab <- satables[[runtype]][[outvar]]
  name <- factor(rownames(plot.tab))
  fontsize = 14
  pct <- plot.tab$cv.theta
  a = data.frame(name, pct)
  yaxmax <- ceiling(max(pct)*10)/10 + 0.1
  plot1 <- ggplot(a, aes(name, pct)) + 
    geom_pointrange(aes(ymin = 0, ymax = pct), size = 1.25) +
      scale_y_continuous(breaks =  seq(0,yaxmax, by=0.2), 
                         limits = c(0, yaxmax), 
                         expand = c(0, 0)) + 
                           coord_flip() + theme_bw() +
                             opts(axis.line = theme_segment(),
                                  axis.text.x = theme_text(size=fontsize),
                                  axis.text.y = theme_text(size=0.001),
                                  axis.title.x = theme_blank(), 
                                  axis.title.y = theme_blank(),
                                  panel.grid.major = theme_blank(),
                                  panel.grid.minor = theme_blank(),
                                  panel.background = theme_blank(),
                                  panel.border = theme_blank(),
                                  title = 'CV')
  

  pct<-plot.tab$elas
  a = data.frame(name, pct)
  plot2 <- ggplot(a, aes(name, pct)) + 
    geom_pointrange(aes(ymin = 0, ymax = pct), size = 1.25) +
      scale_y_continuous(breaks =  c( -5, 0, 5), 
                         limits = c(-10,10), 
                         expand = c(0, 0)) + 
                           coord_flip() + theme_bw() +
                             opts(axis.line = theme_segment(),
                                  axis.text.x = theme_text(size=fontsize),
                                  axis.text.y = theme_text(size=0.001),
                                  axis.title.x = theme_blank(), 
                                  axis.title.y = theme_blank(),
                                        #     theme_text(size = 20),
                                        #	 axis.ticks = theme_blank(),
                                  panel.grid.major = theme_blank(),
                                  panel.grid.minor = theme_blank(),
                                  panel.background = theme_blank(),
                                  panel.border = theme_blank(),
                                  title = 'Elasticity')

  pct<-plot.tab$rel.var
  a = data.frame(name, pct)
  plot3 <- ggplot(a, aes(name, pct)) + 
    geom_pointrange(aes(ymin = 0, ymax = pct), size = 1.25) +
      scale_y_continuous(breaks = c(0,0.2, 0.4, 0.6, 0.8), 
                         limits = c(0,1), 
                         expand = c(0, 0),
                         formatter = "percent") + 
                           coord_flip() + theme_bw() +
                             opts(axis.line = theme_segment(),
                                  axis.text.x = theme_text(size=fontsize),
                                  axis.text.y = theme_text(size=0.01),
                                  axis.title.x = theme_blank(), 
                                  axis.title.y = theme_blank(),
                                        #     theme_text(size = 20),
                                        #	 axis.ticks = theme_blank(),
                                  panel.grid.major = theme_blank(),
                                  panel.grid.minor = theme_blank(),
                                  axis.line.x = theme_blank(),
                                  panel.border = theme_blank(),
                                  title = '% Explained Variance')

  pdf(paste(runtype, outvar, 'SAplot.pdf', sep=""), width = 11, height = 7)
  arrange(plot0,plot1,plot2,plot3,ncol=4)
  dev.off()
}

#for (i in 1:2) {
#  for (j in 1:2) {
#    plot.sa(i,j,outvar)
#  }
#}
