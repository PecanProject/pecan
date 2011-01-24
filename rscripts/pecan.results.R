fontsize = 14 


######AGB ENSEMBLE PLOTS

load('out/edout.ens.Rdata')
ens <- edout.ens[edout.ens$agb<100 & edout.ens$agb > 0 & !is.na(edout.ens$agb),]
pr.q <- quantile(ens$agb[ens$runtype=='prior'], c(0.05, 0.5, 0.95))
po.q <- quantile(ens$agb[ens$runtype=='post'], c(0.05, 0.5, 0.95))
pr.dens <- density(ens$agb[ens$runtype=='prior'], from = 0)
po.dens <- density(ens$agb[ens$runtype=='post'], from = 0)

pr.densdf <- data.frame(x = c(0, pr.dens$x), y = c(0, pr.dens$y))
po.densdf <- data.frame(x = c(0, po.dens$x), y = c(0, po.dens$y))

pdf('out/ensemble_density.pdf')

ggplot() +
  theme_bw() +
  opts(title = "Aboveground Biomass \n prior (grey) \n post (dark grey)",
       axis.text.y = theme_blank(),
       axis.text.x = theme_text(size=fontsize),
       axis.line = theme_blank(),
       axis.title.x = theme_blank(), 
       axis.title.y = theme_blank(),
       theme_text(size = 20),
       axis.ticks.x = theme_blank(),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       panel.border = theme_blank(),
       axis.color.y = 'white',
       legend.position=c(-10,0)) +
  geom_area(data = subset(pr.densdf, x >= pr.q[1] & x<= pr.q[3]),
            aes(x=x, y=y), fill = 'grey50', alpha = 0.5) +
  geom_area(data = subset(po.densdf, x >= po.q[1] & x<= po.q[3]),
            aes(x=x, y=y), fill = 'grey20') +
  geom_line(data = pr.densdf, aes(x=x, y = y) , color = 'grey50') +
  geom_line(data = po.densdf, aes(x=x, y = y) , color = 'grey20') +
  scale_x_continuous(limits = c(0, 50))#, breaks = c(0, 20, 40, 60, 80, 100))

dev.off()

## Trait Pdfs
load('out/pecan.samps.Rdata')
load('madata.Rdata')
load('out/trait.defs.Rdata')
traits <- colnames(prior.samps)#trait.defs$id

trait.plots <- list()
for (i in seq(traits)) {
  tri<-traits[i]
  data <- data.frame(prior = prior.samps[,tri], post = post.samps[,tri])

  plot <- ggplot(data=data) +
    geom_density(aes(x=prior), color='grey') +
      theme_bw() +
        scale_x_continuous('units') +
          scale_y_continuous('Density') +
            opts(title = as.character(trait.defs[trait.defs$id == tri, 'figid']))

  if(tri == 'Vm0') {
    tr <- trait.data[['Vcmax']]
  } else {
    tr <- trait.data[[tri]]
  }

  if(!is.null(nrow(tr))) {
    plot <- plot + geom_density(aes(x = post))
    plot <- plot + geom_point(aes(x=Y, y=0), data=tr) 
  }
  trait.plots[[tri]] <- plot
}

pdf('out/traitpdfs.pdf')
trait.plots
dev.off()

## Diagnostic Plots
load('out/satables.Rdata')
postagb <- satables[['post']][['agb']]
prioragb <- satables[['prior']][['agb']]
means <- data.frame(names = rownames(postagb), postmean=postagb$mean.theta, priormean=prioragb$mean.theta)

ggplot(data=means) +
  theme_bw() +
  coord_trans(xtrans='log10') +
  scale_x_continuous(breaks = c(0.01, 0.1, 1, 10, 100, 1000), limits = c(0.005, 1000)) +
  geom_point(aes(x = priormean,y = names),colour = 'grey', size = 5) +
  geom_point(aes(x = postmean,y = names), size = 5) +
  opts(axis.line = theme_segment(),
       axis.text.x = theme_text(size = 14),
       axis.text.y = theme_text(size = 14, hjust=1),
       axis.title.x = theme_blank(),
       axis.title.y = theme_blank())
   
