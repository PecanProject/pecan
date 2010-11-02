load('out/edout.Rdata')
library(ggplot2)
pr.samp <- which(edout[['runtype']]=='priorsamp')
po.samp <- which(edout[['runtype']]=='postsamp')
agb <-edout[['agb']][['output']]

data <-data.frame(prior = agb[pr.samp,3],post = agb[po.samp,3]) 


ggplot(data=data) +
  opts(title = 'Switchgrass Yield (grey = prior, black = posterior)') +
  geom_density(aes(x = post)) +
  geom_density(aes(x = prior),colour = 'grey') +
  scale_x_continuous('Aboveground Biomass Nov 2002 Mg ha-1') +
  scale_y_continuous('Density') +
  theme_bw()
  
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
   
