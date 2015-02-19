##' Trait analysis
library(ggplot2)
library(reshape2)
library(nimble)

source("trait_load.R")

FFT.all <- load.data()
FFT.np <- subset(FFT.all, Group.1 != "PIRE")
PATH.results <- "~/Documents/Dropbox/PROSPECT_results/"

### Correlation raster plot
corplot <- function(dat) {
        dat.c <- cor(as.matrix(dat[,-1]),
                     use="pairwise.complete.obs")[-(1:10),1:10]
        dat.cm <- melt(dat.c)
        plt <- ggplot(dat.cm) + 
                aes(x=Var1, y=Var2, fill=value) +
                geom_raster() +
                scale_fill_gradient2(low="red", mid="white", high="green",
                                     limits=c(-1,1))
        return(plt)
}

pairsplot <- function(dat, size=1000) {
        m1 <- melt(dat, id.vars=colnames(dat)[1:11], value.name="tval",
                   variable.name="trait")
        m2 <- melt(m1, measure.vars=colnames(dat)[2:11], value.name="pval",
                   variable.name="prospect")
        plt <- ggplot(m2) + aes(x=tval, y=pval) + geom_point() +
                facet_grid(prospect ~ trait, scale="free")
        return(plt)
}
        
########################################
### Bayesian optical trait analysis
########################################

mvlinear <- function(dat, variable){
        ## Subset data - complete cases only (no NA)
        dat.s <- dat[complete.cases(dat), c("Group.1", variable,
                                            "N", "Cab", "Cw", "Cm")]
        n.species <- dim(dat.s)[1]
        
        ## Model specification
        mvCode <- nimbleCode({
                Nc ~ dnorm(Nm, Np)
                Cabc ~ dnorm(Cabm, Cabp)
                Cwc ~ dnorm(Cwm, Cwp)
                Cmc ~ dnorm(Cmm, Cmp)
                Res ~ dnorm(0, Resp)
                
                for (i in 1:nsp){
                        ym[i] <- Nc*N[i] + Cabc*Cab[i] + 
                                Cwc*Cw[i] + Cmc*Cm[i] + Res
                        y[i] ~ dnorm(ym[i], SDp)
                }
                
                # Priors
                Nm ~ dnorm(0, 0.01)
                Np ~ dgamma(0.01, 0.01)
                Nv <- 1/Np
                Cabm ~ dnorm(0, 0.01)
                Cabp ~ dnorm(0.01, 0.01)
                Cabv <- 1/Cabp
                Cwm ~ dnorm(0, 0.01)
                Cwp ~ dnorm(0.01, 0.01)
                Cwv <- 1/Cwp
                Cmm ~ dnorm(0, 0.01)
                Cmp ~ dnorm(0.01, 0.01)
                Cmv <- 1/Cmp
                Resp ~ dgamma(0.01, 0.01)
                Resv <- 1/Resp
                SDp ~ dgamma(0.01, 0.01)
                SD <- 1/SDp
        })
        
        ## Model constants
        mvConsts <- list(nsp = n.species)
        
        ## Model data
        mvData <- list(N = dat.s$N,
                       Cab = dat.s$Cab,
                       Cw = dat.s$Cw,
                       Cm = dat.s$Cm,
                       y = dat.s[,variable])
        
        ## Initial conditions
#         mvInits <- list(Nm = 0, Np = 0.01,
#                         Cabm = 0, Cabp = 0.01,
#                         Cwm = 0, Cwp = 0.01,
#                         Cmm = 0, Cmp = 0.01,
#                         Resp = 0.01)

        mvModel <- nimbleModel(code = mvCode, name="mv_linear",
                               constants = mvConsts, data=mvData,
                               inits = NULL)      

        calculate(mvModel, mvModel$getDependencies("ym"))
        
        mvComp <- compileNimble(mvModel)
        mvSpec <- configureMCMC(mvComp, print=TRUE)
}

