library(devtools)
#install('~/Projects/pecan/pecan/modules/rtm')
load_all('~/Projects/pecan/pecan/modules/rtm')
load_all('~/Projects/pecan/pecan/modules/assim.batch/')

pl <- list(N = c(2.5, 0.03),
           Cab = c(70, 0.1),
           Car = c(15, 0.1),
           Cw = c(0.02, 0.001),
           Cm = c(0.004, 0.0001))

#distplot(dnorm, pl$N, 1, 4) 
#distplot(dnorm, pl$Cab, 30, 50)
#distplot(dnorm, pl$Car, 5, 10)
#distplot(dnorm, pl$Cw, 0.005, 0.015)
#distplot(dnorm, pl$Cm, 0.005, 0.015)

pdp <- prior.defaultvals.prospect()

#distplot(dlnorm, c(pdp$mu[1], pdp$sigma[1]))
#distplot(dlnorm, c(pdp$mu[2], pdp$sigma[2]))
#distplot(dlnorm, c(pdp$mu[3], pdp$sigma[3]))
#distplot(dlnorm, c(pdp$mu[4], pdp$sigma[4]))
#distplot(dlnorm, c(pdp$mu[5], pdp$sigma[5]))

true <- c("N" = 1.5,
          "Cab" = 40,
          "Car" = 8,
          "Cw" = 0.01,
          "Cm" = 0.009)

data(sensor.rsr)
sensor <- "identity"
obs_true <- prospect(true, 5)[,1]
obs_noise <- obs_true + generate.noise()
obs <- spectral.response(obs_noise, sensor)

invert.options <- default.settings.prospect
invert.options$model <- function(param) {
  spectral.response(prospect(param, 5)[,1], sensor)
}
invert.options$inits <- invert.options$inits.function()
invert.options$do.lsq <- TRUE
#invert.options$ngibbs <- 100
#invert.options$prior.function <- function(p) {
  #den <- dnorm(p, sapply(pl, "[", 1), sapply(pl, "[", 2), TRUE)
  #return(sum(den))
#}

#Rprof()

#samp <- invert.custom(observed = obs, 
                      #invert.options = invert.options)
samp <- invert.auto(observed = obs,
                    invert.options = invert.options)

#Rprof(NULL)
#sp <- summaryRprof()
#head(sp$by.self)

#smcmc <- coda::as.mcmc(samp$results)

#sbt <- window(samp$samples, start = 48200)

#samp <- invert.auto(observed

#plot(smcmc)

#pdf(sprintf("~/Pictures/%s.inversion_posteriors.pdf", sensor))
#for (i in 1:5) {
  #coda::densplot(smcmc[,i], main = names(true)[i])
  #lines(distplot(dlnorm, c(pdp$mu[i], pdp$sigma[i]), plot = FALSE),
        #col = "red")
  #abline(v = true[i])
#}
#dev.off()

