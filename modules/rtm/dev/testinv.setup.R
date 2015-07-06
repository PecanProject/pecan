library(PEcAnRTM)

## Sail parameters
#param.order <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm",
#                 "LIDFa", "LIDFb", "LIDFtype", "LAI", "q",
#                 "tts", "tto", "psi", "psoil")
# Parameter values
# N         1 - 4
# Cab       10 - 120
# Car       5 - 30
# Cbrown    0
# Cw        0.005 - 0.04
# Cm        0.002 - 0.014 
# LAI       0 - 10
# LIDFa     10 - 80 (degrees)
# q         0.2
# psoil     0 - 1
# tts       30
# tto       0
# psi       0

## Priors
lai.mu <- log(1.5)
lai.sd <- log(4)
Cab.mu <- log(30)
Cab.sd <- log(10)
Car.mu <- log(20)
Car.sd <- log(10)
Cw.mu <- -6.377
Cw.sd <- 0.5
inv.priors <- list()
inv.priors[[1]] <- function(LAI) dnorm(log(LAI), lai.mu, lai.sd, 1)
inv.priors[[2]] <- function(Cab) dnorm(log(Cab), Cab.mu, Cab.sd, 1)
inv.priors[[3]] <- function(Car) dnorm(log(Car), Car.mu, Car.sd, 1)
inv.priors[[4]] <- function(Cw) dnorm(log(Cw), Cw.mu, Cw.sd, 1)
inv.pmin <- c("LAI" = 0, "Cab" = 0, "Car" = 0, "Cw" = 0)
initial <- function(){
        i <- numeric()
        i["LAI"] <- min(rlnorm(1, lai.mu, lai.sd), 10)
        i["Cab"] <- min(rlnorm(1, Cab.mu, Cab.sd), 120)
        i["Car"] <- min(rlnorm(1, Car.mu, Car.sd), 30)
        i["Cw"] <- min(rlnorm(1, Cw.mu, Cw.sd), 0.04)
        return(i)
}

inv.s.low <- c("LAI" = 1, "Cab" = 10, "Car" = 5, "Cw" = 0.005)
inv.s.mid <- c("LAI" = 5, "Cab" = 40, "Car" = 20, "Cw" = 0.017)
inv.s.high <- c("LAI" = 10, "Cab" = 120, "Car" = 30, "Cw" = 0.04)

par.def <- sail.def[c("LAI", "Cab", "Car", "Cw")]
cons <- sail.constants(par.def)

## Basic AVIRIS inversion
pars.av1 <- c("LAI" = 2, "Cab" = 35, "Car" = 15, "Cw" = 0.009)
cons <- sail.constants(pars.av1)
obs <- ps.aviris(pars.av1, cons)

