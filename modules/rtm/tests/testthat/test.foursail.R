context("Testing standalone SAIL RTM")

# get soil reflectance spectra
rsoil <- system.file("extdata", "soil_reflect_par.dat", package="PEcAnRTM")
rsoil <- read.table(rsoil,header = F)
rsoil <- as.vector(unlist(rsoil[1,]))
rsoil <- c(rsoil,rsoil[2100]) # make soil reflectance the correct length
if (interactive()) {
  plot(seq(400,2500,1), rsoil, type = "l")
}

# define sail parameters
LIDFa <- -0.35 
LIDFb <- -0.15
TypeLIDF <- 1
LAI <- 4
q <- 0.1
tts <- 48
tto <- 0 
psi <- 234
params <- c(LIDFa,LIDFb,TypeLIDF,LAI,q,tts,tto,psi)
param <- params

# get leaf reflectance and transmittance
LRT <- PEcAnRTM::prospect(c(2,55,10,3,0.1,0.007,0.007), 'D')
if (interactive()) {
  plot(LRT[,1])
}
refl <- LRT[,1]
length(refl)
tran <- LRT[,2]

test_that("standalone SAIL RTM", {
  # generate 4SAIL canopy spectra
  sail_spec <- foursail(refl, tran, rsoil, params)

  expect_true(is_spectra(sail_spec))
  expect_true(nrow(sail_spec) == 2101)
  expect_true(ncol(sail_spec) == 4)
  expect_true(all(is.finite(sail_spec)))
  expect_true(all(sail_spec > 0))
})

# plot results
if (interactive()) {
  matplot(sail_spec)
}
