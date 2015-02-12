### PROSPECT code and inversion, implemented in Nimble
library(nimble)
# setwd("R/")
# PATH.pd <- "../data/dataSpec_p4.RData"
# PATH.sm <- "input_matrix.R"
# 
# source(PATH.sm)
# 
# load(PATH.pd)
# dat.p <- dataSpec_p4
# rm(dataSpec_p4)
# 
# obs.spec <- specmatrix("ACRU", spectype="FFT")

load("pinv_nimble_testdat.Rdata")
n.iter <- 10

prospect_LL <- nimbleFunction(
        run = function(){
#                 N = double(0),
#                        Cab = double(0),
#                        Cw = double(0),
#                        Cm = double(0),
#                        nr = double(1),
#                        Cab_abs = double(1),
#                        Cw_abs = double(1),
#                        Cm_abs = double(1)){

                wl <- 1 + 2500 - 400

# PROSPECT Model ----------------------------------------------------------
                theta <- rep(0, wl)
                Refl <- rep(0, wl)

                k = (1.0/N) * (Cab * Cab_abs + Cw * Cw_abs + Cm * Cm_abs)
                n = nr

                k1 <- k <= 0
                k2 <- k > 0 & k <=4
                k3 <- k > 4 & k <=85
                k4 <- k > 85

                tau <- rep(0, wl)

                ### Exponential integral
                tau[k1] <- 1
                ## Condition 2
                xx = 0.5 * k[k2] - 1.0
                yy=(((((((((((((((-3.60311230482612224e-13
                                  *xx+3.46348526554087424e-12)*xx-2.99627399604128973e-11)
                                *xx+2.57747807106988589e-10)*xx-2.09330568435488303e-9)
                              *xx+1.59501329936987818e-8)*xx-1.13717900285428895e-7)
                            *xx+7.55292885309152956e-7)*xx-4.64980751480619431e-6)
                          *xx+2.63830365675408129e-5)*xx-1.37089870978830576e-4)
                        *xx+6.47686503728103400e-4)*xx-2.76060141343627983e-3)
                      *xx+1.05306034687449505e-2)*xx-3.57191348753631956e-2)
                    *xx+1.07774527938978692e-1)*xx-2.96997075145080963e-1;
                yy=(yy*xx+8.64664716763387311e-1)*xx+7.42047691268006429e-1
                yy=yy-log(k[k2])
                tau[k2] = (1.0-k[k2])*exp(-k[k2])+k[k2]^2*yy
                ## Condition 3
                xx=14.5/(k[k3]+3.25)-1.0
                yy=(((((((((((((((-1.62806570868460749e-12
                                  *xx-8.95400579318284288e-13)*xx-4.08352702838151578e-12)
                                *xx-1.45132988248537498e-11)*xx-8.35086918940757852e-11)
                              *xx-2.13638678953766289e-10)*xx-1.10302431467069770e-9)
                            *xx-3.67128915633455484e-9)*xx-1.66980544304104726e-8)
                          *xx-6.11774386401295125e-8)*xx-2.70306163610271497e-7)
                        *xx-1.05565006992891261e-6)*xx-4.72090467203711484e-6)
                      *xx-1.95076375089955937e-5)*xx-9.16450482931221453e-5)
                    *xx-4.05892130452128677e-4)*xx-2.14213055000334718e-3;
                yy=((yy*xx-1.06374875116569657e-2)*xx-8.50699154984571871e-2)*xx+9.23755307807784058e-1
                yy=exp(-k[k3])*yy/k[k3]
                tau[k3] = (1.0-k[k3])*exp(-k[k3])+k[k3]^2*yy
                ## Condition 4
                tau[k4] = 0
                theta = tau

                ### Transmissivity of elementary layer at nadir (t90)
                sa = 1   # sin(pi/2)
                np = n*n + 1
                nm = n*n - 1
                a = (n + 1)*(n+1) / 2
                k = (-((n*n - 1)*(n*n - 1))) / 4
                b1 = 0
                b2 = sa*sa - np/2
                b = b1 - b2
                ts = (k*k/(6.0*b*b*b) + k/b - b/2.0) - (k*k/(6.0*a*a*a) + k/a - a/2.0)
                tp1 = -2.0*n*n * (b - a) / (np*np)
                tp2 = -2.0*n*n * np * log(b/a) / (nm*nm)
                tp3 = n*n * (1.0/b - 1.0/a) / 2.0
                tp4 = 16.0*n*n*n*n * (n*n*n*n + 1.0) * log((2.0*np*b - nm*nm)/(2.0*np*a - nm*nm)) / (np*np*np * nm*nm)
                tp5 = 16.0*n*n*n*n*n*n * (1.0/(2.0*np*b - nm*nm) - 1.0/(2.0*np*a - nm*nm)) / (np*np*np)
                tp = tp1 + tp2 + tp3 + tp4 + tp5
                out = (ts + tp) / (2.0*sa*sa)
                t90 <- out

                ### Transmissivity of elementary layer at alpha (40 deg)
                alpha <- 0.69813170079  # 40 * pi/180
                sa = sin(alpha)
                np = n*n + 1
                nm = n*n - 1
                a = (n + 1)*(n+1) / 2
                k = (-((n*n - 1)*(n*n - 1))) / 4
                b1 = sqrt((sa*sa - np/2) * (sa*sa - np/2) + k)
                b2 = sa*sa - np/2
                b = b1 - b2
                ts = (k*k/(6.0*b*b*b) + k/b - b/2.0) - (k*k/(6.0*a*a*a) + k/a - a/2.0)
                tp1 = -2.0*n*n * (b - a) / (np*np)
                tp2 = -2.0*n*n * np * log(b/a) / (nm*nm)
                tp3 = n*n * (1.0/b - 1.0/a) / 2.0
                tp4 = 16.0*n*n*n*n * (n*n*n*n + 1.0) * log((2.0*np*b - nm*nm)/(2.0*np*a - nm*nm)) / (np*np*np * nm*nm)
                tp5 = 16.0*n*n*n*n*n*n * (1.0/(2.0*np*b - nm*nm) - 1.0/(2.0*np*a - nm*nm)) / (np*np*np)
                tp = tp1 + tp2 + tp3 + tp4 + tp5
                out = (ts + tp) / (2.0*sa*sa)
                tav = out

                # "x" and "y" simplifications from original PROSPECT model (Jacquemoud & Baret 1990)
                x = tav / t90
                y = x * (t90 - 1.0) + 1.0 - tav

                # Reflectance and transmittance of first layer (N=1)
                tao1 = tav
                tao2 = t90 / (n*n)
                rho1 = 1 - tao1
                rho2 = 1 - tao2
                rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta)
                taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta)
                rho90 = (rhoa - y) / x
                tao90 = taoa / x

                # Reflectance and transmittance of N layers (Stokes coefficients)
                d90 = sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90)
                a90 = (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90)
                b90 = (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90)
                nmR = taoa * tao90 * (pow(b90,(N-1.0)) - pow(b90,(1.0-N)))
                #nmT = taoa * (a90 - 1/a90)    # Transmittance calcs
                dmRT = a90*pow(b90, (N-1.0)) - pow(b90, (1.0-N))/a90 - rho90 * (pow(b90, (N-1.0)) - pow(b90,(1.0-N)))
                RNa = rhoa + nmR / dmRT
                #TNa = nmT / dmRT   # Transmittance calcs

# Likelihood calculation --------------------------------------------------

                Refl <- RNa
                specerror <- matrix(nrow = wl, ncol = nspec)
                for (i in 1:nspec){
                        specerror[,i] <- Refl - observed[,i]
                }
                logL <- sum(dnorm(specerror, 0, resp))
                return(logL)
                returnType(double(0))
        })

### Bayesian inversion
prospectCode <- nimbleCode({
        ### Priors
        Ni ~ dlnorm(-0.916, 2.2)
        N <- Ni + 1
        Cab ~ dlnorm(3.4, 0.9)
        Cw ~ dlnorm(-6.377, 0.5)
        Cm ~ dlnorm(-5.116, 0.9)
        resp ~ dgamma(0.001, 0.001)
        resv <- 1/resp
})

prospectConstants <- list(nr = dat.p[,2],
                          Cab_abs = dat.p[,3],
                          Cw_abs = dat.p[,5],
                          Cm_abs = dat.p[,6],
                          observed = obs.spec,
                          nspec = ncol(obs.spec))

#prospectData <- list(observed = obs.spec)

prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants)
                        #data = prospectData)

prospectSpec <- configureMCMC(prospect, print=TRUE)
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "N",
                                       llFunction = prospect_LL,
                                       includesTarget = FALSE))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cab",
                                       llFunction = prospect_LL,
                                       includesTarget = FALSE))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cw",
                                       llFunction = prospect_LL,
                                       includesTarget = FALSE))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cm",
                                       llFunction = prospect_LL,
                                       includesTarget = FALSE))
prospectSpec$addMonitors(c("N", "Cab", "Cw", "Cm", "resv"))
print("Building MCMC...")
prospectMCMC <- buildMCMC(prospectSpec, project = prospect)
prospectMCMC$run(n.iter)
samples1 <- as.matrix(prospectMCMC$mvSamples)
plot(samples1[,"N"], type='l')


# print("Compiling...")
# Cprospect <- compileNimble(prospect)
# CprospectMCMC <- compileNimble(prospectMCMC)
# print("Compiled successfully! Starting run...")
# 
# CprospectMCMC$run(n.iter)
# print("Complete! Post processing...")
# 
# samples <- as.matrix(CprospectMCMC$mvSamples)
# plot(samples[,"N"], type='l')



