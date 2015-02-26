sampler_resp <- nimbleFunction(
                               contains = sampler_BASE,
                               setup = function(model, mvSaved, control) {
                                       targetNode <- control$targetNode
                                       calcNodes <- model$getDependencies(targetNode)
                                       constants <- prospectConstants

                                       ## PROSPECT Absorption features
                                       Cab_abs <- constants$Cab_abs
                                       Cw_abs <- constants$Cw_abs
                                       Cm_abs <- constants$Cm_abs

                                       ## Elementary layer transmittances
                                       tao1 <- constants$tao1
                                       tao2 <- constants$tao2
                                       rho1 <- constants$rho1
                                       rho2 <- constants$rho2
                                       x <- constants$x
                                       y <- constants$y

                                       ## Exponential integral constants
                                       e1 <- constants$e1
                                       e2 <- constants$e2

                                       ## Data 
                                       observed <- constants$observed
                                       nspec <- constants$nspec
                                       wl <- constants$wl
                                       zeroswl <- rep(0, wl)
                               },
                               run = function(){
                                       declare(specerror, double(2, c(wl, nspec)))
                                       theta <- zeroswl
                                       Refl <- zeroswl
                                       k <- (1.0/model$N) * (model$Cab * Cab_abs + model$Cw * Cw_abs + model$Cm * Cm_abs)
                                       tau <- zeroswl

                                       ### Exponential integral
                                       for(i in 1:wl) {
                                               ## Condition 1
                                               if(k[i] <= 0)
                                                       tau[i] <- 1
                                               ## Condition 2
                                               if(k[i] > 0 & k[i] <= 4) {
                                                       xx = 0.5 * k[i] - 1.0
                                                       yy=((((((((((((((((((
                                                        e1[1]*xx)+e1[2])*xx+e1[3])*xx+e1[4])*xx+
                                                        e1[5])*xx+e1[6])*xx+e1[7])*xx+e1[8])*xx+
                                                        e1[9])*xx+e1[10])*xx+e1[11])*xx+
                                                        e1[12])*xx+e1[13])*xx+e1[14])*xx+
                                                        e1[15])*xx+e1[16])*xx+e1[17]*xx+
                                                        e1[18])*xx+e1[19])
                                                       yy=yy-log(k[i])
                                                       tau[i] <- (1.0-k[i])*exp(-k[i])+k[i]^2*yy
                                               }
                                               ## Condition 3
                                               if(k[i] > 4 & k[i] <= 85) {
                                                       xx=14.5/(k[i]+3.25)-1.0
                                                       yy=((((((((((((((((((
                                                        e2[1]*xx)+e2[2])*xx+e2[3])*xx+e2[4])*xx+
                                                        e2[5])*xx+e2[6])*xx+e2[7])*xx+e2[8])*xx+
                                                        e2[9])*xx+e2[10])*xx+e2[11])*xx+
                                                        e2[12])*xx+e2[13])*xx+e2[14])*xx+
                                                        e2[15])*xx+e2[16])*xx+e2[17]*xx+
                                                        e2[18])*xx+e2[19])*xx+e2[20]
                                                       yy=exp(-k[i])*yy/k[i]
                                                       tau[i] <- (1.0-k[i])*exp(-k[i])+k[i]^2*yy
                                               }
                                               ## Condition 4
                                               if(k[i] > 85) {
                                                       tau[i] <- 0
                                               }
                                       }
                                       theta <- tau

                                       # Reflectance and transmittance of first layer (N=1)
                                       rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta)
                                       taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta)
                                       rho90 = (rhoa - y) / x
                                       tao90 = taoa / x

                                       # Reflectance and transmittance of N layers (Stokes coefficients)
                                       d90 = sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90)
                                       a90 = (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90)
                                       b90 = (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90)
                                       nmR = taoa * tao90 * (b90^(model$N-1.0) - b90^(1.0-model$N))
                                       dmRT = a90*b90^(model$N-1.0) - b90^(1.0-model$N)/a90 - rho90 * (b90^(model$N-1.0) - b90^(1.0-model$N))
                                       Refl = rhoa + nmR / dmRT

                                       ## Gibbs Sampling
                                       for (i in 1:nspec){
                                               specerror[,i] <- Refl - observed[,i]
                                       }
                                       rp1 <- nspec * wl / 2
                                       rp2 <- (nspec * wl - 1) * var(specerror)
                                       rp <- rgamma(1, rp1, rp2)

                                       model[[targetNode]] <<- rp
                                       calculate(model, calcNodes)
                                       copy(from = model, to = mvSaved, nodes = calcNodes, row = 1, logProb = TRUE)
                               },

                               methods = list(
                                              reset = function() {})
                               )

