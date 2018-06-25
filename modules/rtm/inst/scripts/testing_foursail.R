library(PEcAnRTM)

# get soil
rsoil <- system.file("extdata", "soil_reflect_par.dat", package="PEcAnRTM")
rsoil <- read.table(rsoil,header = F)
#str(rsoil)
rsoil <- as.vector(unlist(rsoil[1,]))
length(rsoil)
rsoil <- c(rsoil,rsoil[2100]) # make soil reflectance the correct length
plot(seq(400,2500,1), rsoil, type = "l")

# define some sail params
LIDFa <- -0.35 
LIDFb <- -0.15
TypeLIDF <- 1
LAI <- 4
q <- 0.1
tts <- 48
tto <- 0 
psi <- 234
params <- c(LIDFa,LIDFb,TypeLIDF,TypeLIDF,LAI,q,tts,tto,psi)

# get leaf refl/trans
LRT <- PEcAnRTM::prospect(c(2,55,10,3,0.1,0.007,0.007), 'D')
plot(LRT[,1])
refl <- LRT[,1]
length(refl)
tran <- LRT[,2]


sail_sepc <- foursail(refl, tran, rsoil, params)


