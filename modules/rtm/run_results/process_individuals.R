### Diagnostics for plot
library(coda)
library(data.table)
library(MASS)
f.path <- "../run_results/FFT_individuals_0203/"
f.list <- list.files(f.path)
spec.list <- unique(gsub("(.*)_[0-9]{2}\\.dat", "\\1", f.list))
nspec <- length(spec.list)
blank <- double(nspec)


load.spec <- function(specname, burnin=5000, thin=5){
    in.files <- list.files(f.path, specname, full.names=TRUE)
    r.list <- mcmc.list()
    for(i in 1:length(in.files)){
        in.list <- mcmc(fread(in.files[i], header=TRUE))
        r.list[[i]] <- window(in.list, start=burnin, thin=thin)
    }
    return(r.list)
}

### Raw samples to all.samples
### Raw summary tables to all.summary
### Fitted lognormal coefficients to all.table
vars <- c("N", "Cab", "Cw", "Cm")
all.samples <- list()
all.summary <- list()
all.table <- data.table(leaf = spec.list,
                        N.mul = blank, N.sdl = blank,
                        Cab.mul = blank, Cab.sdl = blank,
                        Cw.mul = blank, Cw.sdl = blank,
                        Cm.mul = blank, Cm.sdl = blank)
tab.names <- colnames(all.table)[-1]
setkey(all.table, leaf)
for(i in 1:length(spec.list)){
    print(i)
    specname <- spec.list[i]
    in.spec <- load.spec(specname, burnin=5000, thin=5)
    all.samples[[specname]] <- in.spec
    all.summary[[specname]] <- summary(in.spec)
    fitvec <- c(sapply(vars,function(x) {
        fitdistr(c(do.call(rbind, in.spec[,x])),
                 "lognormal")$estimate
    }))
    all.table[specname,tab.names := as.list(fitvec), with=FALSE]
}

save(all.samples, file = "samples_FFT0203.Rdata")
save(all.summary, file = "summary_FFT0203.Rdata")
save(all.table, file = "table_FFT0203.Rdata")

### Merge Mean and SD from all.summary with
###   fitted values from all.table into "results"
as2 <- data.table(t(sapply(all.summary,
                  function(l) l$statistics[,c("Mean", "SD")])),
                  keep.rownames=TRUE)
summary.vars <- c("N.m", "Cab.m", "Cw.m", "Cm.m", "resp",
                  "N.sd", "Cab.sd", "Cw.sd", "Cm.sd", "resp.sd")
setnames(as2, c("leaf", summary.vars))
results <- merge(as2, all.table, by="leaf")
save(results, file = "results_FFT0203.Rdata")
