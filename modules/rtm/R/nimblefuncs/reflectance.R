### Core prospect model
### Returns vector of modeled reflectance

prospect_refl <- nimbleFunction(
        setup = function(model, constants) {
                ### Load constants
                n <- constants$nr
                Cab_abs <- constants$Cab_abs
                Cw_abs <- constants$Cw_abs
                Cm_abs <- constants$Cm_abs
                observed <- constants$observed
                nspec <- constants$nspec
                wl <- constants$wl

                ### Setup wavelength vector
                zeroswl <- rep(0, wl)

                ### t90 pre-calculation
                np <- n^2 + 1
                nm <- n^2 - 1
                a <- (n + 1)^2 / 2
                kt <- (-(nm^2)) / 4
                b <- np/2 - 1

                ### tav pre-calculation
                alpha <- 0.69813170079  # 40 * pi/180
                sa <- sin(alpha)
                bV1 <- sqrt((sa^2 - np/2)^2 + kt)
                bV2 <- sa^2 - np/2
                bV <- bV1 - bV2
        },
        run = function(){
                ### Initializing variables
                theta <- zeroswl
                Refl <- zeroswl
                k <- (1.0/model$N) * (model$Cab * Cab_abs +
                                              model$Cw * Cw_abs +
                                              model$Cm * Cm_abs)
                tau <- zeroswl

                ### Exponential integral
                for(i in 1:wl) {
                        ## Condition 1
                        if(k[i] <= 0){
                                tau[i] <- 1
                        }
                        ## Condition 2
                        if(k[i] > 0 & k[i] <= 4) {
                                xx = 0.5 * k[i] - 1.0
                                yy=(((((((((((((((-3.60311230482612224e-13
                                                  *xx+3.46348526554087424e-12)
                                *xx-2.99627399604128973e-11)
                                *xx+2.57747807106988589e-10)*xx-2.09330568435488303e-9)
                                *xx+1.59501329936987818e-8)*xx-1.13717900285428895e-7)
                                *xx+7.55292885309152956e-7)*xx-4.64980751480619431e-6)
                                *xx+2.63830365675408129e-5)*xx-1.37089870978830576e-4)
                                *xx+6.47686503728103400e-4)*xx-2.76060141343627983e-3)
                                *xx+1.05306034687449505e-2)*xx-3.57191348753631956e-2)
                                *xx+1.07774527938978692e-1)*xx-2.96997075145080963e-1
                                yy=(yy*xx+8.64664716763387311e-1)*xx+7.42047691268006429e-1
                                yy=yy-log(k[i])
                                tau[i] <- (1.0-k[i])*exp(-k[i])+k[i]^2*yy
                        }
                        ## Condition 3
                        if(k[i] > 4 & k[i] <= 85) {

                                xx=14.5/(k[i]+3.25)-1.0
                                yy=(((((((((((((((-1.62806570868460749e-12
                                                  *xx-8.95400579318284288e-13)
                                *xx-4.08352702838151578e-12)
                                *xx-1.45132988248537498e-11)*xx-8.35086918940757852e-11)
                                *xx-2.13638678953766289e-10)*xx-1.10302431467069770e-9)
                                *xx-3.67128915633455484e-9)*xx-1.66980544304104726e-8)
                                *xx-6.11774386401295125e-8)*xx-2.70306163610271497e-7)
                                *xx-1.05565006992891261e-6)*xx-4.72090467203711484e-6)
                                *xx-1.95076375089955937e-5)*xx-9.16450482931221453e-5)
                                *xx-4.05892130452128677e-4)*xx-2.14213055000334718e-3;
                                yy=((yy*xx-1.06374875116569657e-2)*xx-8.50699154984571871e-2)*xx+9.23755307807784058e-1
                                yy=exp(-k[i])*yy/k[i]
                                tau[i] <- (1.0-k[i])*exp(-k[i])+k[i]^2*yy
                        }
                        ## Condition 4
                        if(k[i] > 85) {
                                tau[i] <- 0
                        }
                }
                theta <- tau

                ### Transmissivity of elementary layer at nadir (t90)
                ts = (kt^2/(6.0*b^3) + kt/b - b/2.0) - (kt^2/(6.0*a^3) + kt/a - a/2.0)
                tp1 = -2.0*n^2 * (b - a) / (np^2)
                tp2 = -2.0*n^2 * np * log(b/a) / (nm^2)
                tp3 = n^2 * (1.0/b - 1.0/a) / 2.0
                tp4 = 16.0*n^4 * (n^4 + 1.0) * log((2.0*np*b - nm^2)/(2.0*np*a - nm^2)) / (np^3 * nm^2)
                tp5 = 16.0*n^6 * (1.0/(2.0*np*b - nm^2) - 1.0/(2.0*np*a - nm^2)) / (np^3)
                tp = tp1 + tp2 + tp3 + tp4 + tp5
                t90 = (ts + tp) / (2.0)

                ### Transmissivity of elementary layer at alpha (40 deg)
                ts = (kt^2/(6.0*bV^3) + kt/bV - bV/2.0) - (kt^2/(6.0*a^3) + kt/a - a/2.0)
                tp1 = -2.0*n^2 * (bV - a) / (np^2)
                tp2 = -2.0*n^2 * np * log(bV/a) / (nm^2)
                tp3 = n^2 * (1.0/bV - 1.0/a) / 2.0
                tp4 = 16.0*n^4 * (n^4 + 1.0) * log((2.0*np*bV - nm^2)/(2.0*np*a - nm^2)) / (np^3 * nm^2)
                tp5 = 16.0*n^6 * (1.0/(2.0*np*bV - nm^2) - 1.0/(2.0*np*a - nm^2)) / (np^3)
                tp = tp1 + tp2 + tp3 + tp4 + tp5
                tav = (ts + tp) / (2.0*sa^2)

                # "x" and "y" simplifications from original PROSPECT model (Jacquemoud & Baret 1990)
                x = tav / t90
                y = x * (t90 - 1.0) + 1.0 - tav

                # Reflectance and transmittance of first layer (N=1)
                tao1 = tav
                tao2 = t90 / (n^2)
                rho1 = 1 - tao1
                rho2 = 1 - tao2
                rhoa = rho1 + (tao1 * tao2 * rho2 * theta^2) / (1 - rho2^2 * theta^2)
                taoa = tao1 * tao2 * theta / (1 - rho2^2 * theta^2)
                rho90 = (rhoa - y) / x
                tao90 = taoa / x


                # Reflectance and transmittance of N layers (Stokes coefficients)
                d90 = sqrt((tao90^2 - rho90^2 - 1.0)*(tao90^2 - rho90^2 - 1.0) - 4.0*rho90^2)
                a90 = (1.0 + rho90^2 - tao90^2 + d90) / (2.0*rho90)
                b90 = (1.0 - rho90^2 + tao90^2 + d90) / (2.0*tao90)
                #nmR = taoa * tao90 * (pow(b90,(model$N-1.0)) - pow(b90,(1.0-model$N)))
                nmR = taoa * tao90 * b90^(model$N-1.0) - b90^(1.0-model$N)
                dmRT = a90*b90^(model$N-1.0) - b90^(1.0-model$N)/a90 - rho90 * (b90^(model$N-1.0) - b90^(1.0-model$N))
                Refl = rhoa + nmR / dmRT

                #Tmodel$Na = nmT / dmRT   # Transmittance calcs
                #nmT = taoa * (a90 - 1/a90)    # Transmittance calcs

                returnType(double(1, wl))
                return(Refl)
        })

