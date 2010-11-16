load('out/edout.Rdata')
library(ggplot2)
pr.samp <- which(edout[['runtype']]=='priorsamp')
po.samp <- which(edout[['runtype']]=='postsamp')
agb <-edout[['agb']][['output']]

data <-data.frame(prior = agb[pr.samp,3],post = agb[po.samp,3]) 

## Manuscript Figures
agbens <- ggplot(data=data) +
  opts(title = 'Switchgrass Yield (grey = prior, black = posterior)') +
  geom_density(aes(x = post)) +
  geom_density(aes(x = prior),colour = 'grey') +
  scale_x_continuous('November Aboveground Biomass (Mg ha-1 y-1) average over years 5-10') +
  scale_y_continuous('Density') +
  theme_bw()

pdf('out/agbens.pdf')
agbens
dev.off()

## Trait Pdfs
load('out/pecan.samps.Rdata')
load('out/pecan.MA.Rdata')
traits <- colnames(prior.samps)

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

  tr <- trait.data[[tri]]
  if(!is.null(nrow(tr))) {
    plot <- plot + geom_density(aes(x = post))
    tri <- ifelse(tri == 'Vm0', 'Vcmax', tri)
    plot <- plot + geom_point(aes(x=Y, y=0), data=tr) 
  }
  trait.plots[[tri]] <- plot
}

pdf('traitpdfs.pdf')
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
   
