# Test slow inversion
library(PEcAnRTM)
data(sensor.rsr)
params <- c(1.4, 40, 8, 0.01, 0.01)
obs.raw <- prospect(params, 5)[,1] + generate.noise()
sensor <- "chris.proba"
obs <- spectral.response(obs.raw, sensor)
Rprof()
samples <- default.invert.prospect(obs, sensor, ngibbs = 2000, quiet=FALSE)
Rprof(NULL)
print(summaryRprof())
#par(mfrow=c(3,2))
#for(i in 1:6) {
    #plot(samples[,i], type='l')
    #abline(h=params[i])
#}
