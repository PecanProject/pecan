#install.packages("~/dietzelab/pecan/models/ed", repos=NULL)
#install.packages("~/dietzelab/pecan/modules/rtm", repos=NULL)
#library(PEcAn.ED2)
library(PEcAnRTM)
#print(write.config.xml.ED2)



analysis.path <- '~/dietzelab/ED2/EDR/run/'
setwd(analysis.path)
#pecan.id <- "1000001494"
#run.id <- "1000443342"
pecan.id <- "1000001502"
run.id <- "1000443579"
base.path <- '/projectnb/dietzelab/pecan.data/output/ashiklom'
ed2in.path <- file.path(base.path, pecan.id, 'run', run.id, 'ED2IN')
history.path <- file.path(base.path, pecan.id, 'out', run.id)
print(list.files(history.path))
paths <- list(ed2in.path = ed2in.path, history.path = history.path)
par.wl <- 400:2499
nir.wl <- 2500
prospect.param <- c(1.4, 40, 5, 0.01, 0.01)
prospect.version <- 5
datetime <- ISOdatetime(2004, 07, 01, 12, 00, 00)
lenout <- 10
trait.seq <- seq(0.1, 0.4, length.out = lenout)
albedo.mat <- matrix(NA, 2101, lenout)
pft.names <- c('Optics.Temperate_Early_Hardwood', 
               'Optics.Temperate_Mid_Hardwood',
               'Optics.Temperate_Late_Hardwood',
               'Optics.Temperate_Late_Conifer')
trait.values <- lapply(pft.names, function(x) list(name = x))
names(trait.values) <- pft.names
leaf.refl <- prospect(prospect.param, 5, FALSE)[,1]
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
                           history.prefix = 'history',
                           change.history.time = TRUE,
                           output.path = getwd())
#    print(head(albedo))
    albedo.diff <- c(0,diff(albedo))
    threshold <- 0.01
    albedo.na <- which(abs(albedo.diff) > threshold)
    albedo[albedo.na] <- NA
    plot(albedo, type='l')
    albedo.mat[,i] <- albedo
}

#png(sprintf("%s.png", trait.name))
plot.title <- sprintf("%s: %.3f to %.3f", trait.name, min(trait.seq), max(trait.seq))
wl <- 400:2500
matplot(wl, albedo.mat, type='l', main=plot.title)
#dev.off()
