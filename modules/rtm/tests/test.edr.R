#install.packages("~/dietzelab/pecan/models/ed", repos=NULL)
#install.packages("~/dietzelab/pecan/modules/rtm", repos=NULL)
#library(PEcAn.ED2)
library(PEcAnRTM)
#print(write.config.xml.ED2)

analysis.path <- '~/dietzelab/ED2/EDR/run/'
setwd(analysis.path)
ed2in.path <- '/projectnb/dietzelab/pecan.data/output/ashiklom/1000001295/run/1000336885/ED2IN'
history.path <- '/projectnb/dietzelab/pecan.data/output/ashiklom/1000001295/out/1000336885/'
paths <- list(ed2in.path = ed2in.path, history.path = history.path)
par.wl <- 400:750
nir.wl <- 751:2500
prospect.param <- c(1.4, 40, 5, 0.01, 0.01)
prospect.version <- 5
datetime <- ISOdatetime(2004, 07, 01, 12, 00, 00)
lenout <- 10
trait.seq <- seq(0.1, 0.9, length.out = lenout)
albedo.mat <- matrix(NA, 2101, lenout)
pft.names <- c('Optics.Temperate_Early_Hardwood', 
               'Optics.Temperate_Mid_Hardwood',
               'Optics.Temperate_Late_Hardwood',
               'Optics.Temperate_Late_Conifer')
trait.values <- lapply(pft.names, function(x) list(name = x))
names(trait.values) <- pft.names
for(i in 1:length(trait.seq)){
    val <- trait.seq[i]
    print(val)
    for(p in seq_along(pft.names)){
        trait.values[[p]]$clumping_factor <- val
    }
    trait.name <- names(trait.values[[p]])[2]
    #xml <- write.config.xml.ED2(defaults = list(),
                                #settings = list(model = list(revision = "git")),
                                #trait.values = trait.values)
    #print(xml)
    albedo <- EDR.prospect(prospect.param = prospect.param,
                           prospect.version = prospect.version,
                           paths=paths, 
                           par.wl = par.wl, nir.wl = nir.wl,
                           datetime = datetime, 
                           trait.values = trait.values,
                           history.prefix = 'history', change.history.time = TRUE,
                           output.path = getwd())
    print(head(albedo))
    albedo.mat[,i] <- albedo
}

#pdf(sprintf("%s.pdf", trait.name))
plot.title <- sprintf("%s: %.3f to %.3f", trait.name, min(trait.seq), max(trait.seq))
wl <- 400:2500
matplot(wl, albedo.mat[wl-399,], type='l', main=plot.title)
#dev.off()
