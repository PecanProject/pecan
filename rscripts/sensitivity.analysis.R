library(XML)
if(interactive()){
   user <- system('echo $USER', intern = TRUE)
  if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

##TODO get code to handle > 1 pft
pft <- settings$pfts$pft$name

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))
outdir <- settings$outdir

load('tests/trait.samples.Rdata')
load('tests/sa.test.Rdata')
#load('tests/ensemble.output.Rdata')

traits <- names(trait.samples[[pft]])

sa.splines <- sapply(traits, function(x) sa.spline(sa.trait[[x]], sa.agb[[x]]))
##TODO need to do this for each pft
spline.estimates <-  sapply(traits, function(x) spline.estimate(sa.splines[[x]], trait.samples[[pft]][[x]]))
spline.estimates.trunc <-  zero.truncate(spline.estimates)
trait.means  <- unlist(lapply(trait.samples[[pft]], mean))
trait.variance <- unlist(lapply(trait.samples[[pft]], var))
output.means <- colMeans(spline.estimates.trunc)
output.variance <- apply(spline.estimates.trunc, 2, var)
sensitivities <- sapply(traits,
                        function(x) sa.splines[[x]](trait.means[[x]], 1)) 

saplots <-  lapply(traits, function(x) sensitivity.plot(sa.trait[[x]], sa.splines[[x]], x))

pdf('sensitivity.analysis.pdf', height = 12, width = 20)
saplots#left='Aboveground Biomass', main='Parameter Sensitivity', nrow=3,ncol=5) 
dev.off()

########Variance Decomposition
elasticities <- sensitivities / (output.means / trait.means)

coef.vars    <- ifelse(traits != 'Vm_low_temp',
                       sqrt(trait.variance) / trait.means,
                       sqrt(trait.variance) / (trait.means + 273.15))

total.variance <- sum(output.variance)
explained.variance <- output.variance / total.variance

## stand in to be replaced by plot used in publication
pdf('variancedecomposition.pdf', width = 12, height = 8)
grid.arrange(qplot(names(explained.variance), coef.vars) + coord_flip(),
             qplot(1:length(explained.variance), elasticities) + coord_flip(),
             qplot(1:length(explained.variance), explained.variance) + coord_flip(),
             ncol = 3)
dev.off()
