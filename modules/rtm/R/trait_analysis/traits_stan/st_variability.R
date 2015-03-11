## Variability model, implemented in Stan
library(rstan)
library(data.table)
load("../data/FFT_full.Rdata")

fft.spec <- fftdat[!is.na(Spectra) & !is.na(PFT)]
design.reg <- model.matrix(N.mul ~ Height + PFT, data=fft.spec)

vdata <- list(X = design.reg,
              nfe = ncol(design.reg),
              n_all = nrow(fft.spec))
vdata$y_m <- fft.spec[,N.mul]
vdata$y_s <- fft.spec[,N.sdl]

vc <- stanc("trait_analysis/traits_stan/variability.stan", model_name = "vari")
vm <- stan_model(stanc_ret = vc, verbose=FALSE)
vfit <- sampling(vm, data=vdata, iter=5000, chains=1)       
