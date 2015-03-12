##' Trait analysis
library(ggplot2)
library(reshape2)
library(data.table)
load("../data/FFT_full.Rdata")


# Basic histograms --------------------------------------------------------

N.h <- ggplot(fft.spec) + geom_histogram(aes(x=N.m))
Cab.h <- ggplot(fft.spec) + geom_histogram(aes(x=Cab.m))
Cw.h <- ggplot(fft.spec) + geom_histogram(aes(x=Cw.m))
Cm.h <- ggplot(fft.spec) + geom_histogram(aes(x=Cm.m))

grid.arrange(N.h, Cab.h, Cw.h, Cm.h)

# Posterior vs Prior ------------------------------------------------------

N.prior <- 1 + rlnorm(1e5, -0.916, 2.2)
Cab.prior <- rlnorm(1e5, 3.4, 0.9)
Cw.prior <- rlnorm(1e5, -6.377, 0.5)
Cm.prior <- rlnorm(1e5, -5.116, 0.9)

N.pp <- ggplot() + geom_density(aes(x=N.prior), color="green") +
        geom_density(aes(x=fft.spec[,N.m]), color="black") +
        xlim(c(fft.spec[,c(min(N.m), max(N.m))]))
Cab.pp <- ggplot() + geom_density(aes(x=Cab.prior), color="green") +
        geom_density(aes(x=fft.spec[,Cab.m]), color="black") +
        xlim(c(fft.spec[,c(min(Cab.m), max(Cab.m))]))
Cw.pp <- ggplot() + geom_density(aes(x=Cw.prior), color="green") +
        geom_density(aes(x=fft.spec[,Cw.m]), color="black") +
        xlim(c(fft.spec[,c(min(Cw.m), max(Cw.m))]))
Cm.pp <- ggplot() + geom_density(aes(x=Cm.prior), color="green") +
        geom_density(aes(x=fft.spec[,Cm.m]), color="black") +
        xlim(c(fft.spec[,c(min(Cm.m), max(Cm.m))]))

grid.arrange(N.pp, Cab.pp, Cw.pp, Cm.pp)


# Violin plots by PFT ------------------------------------------------

N.pft <- ggplot(fft.spec) + aes(x = PFT, y = N.m) + geom_violin()
Cab.pft <- ggplot(fft.spec) + aes(x = PFT, y = Cab.m) + geom_violin()
Cw.pft <- ggplot(fft.spec) + aes(x = PFT, y = Cw.m) + geom_violin()
Cm.pft <- ggplot(fft.spec) + aes(x = PFT, y = Cm.m) + geom_violin()

grid.arrange(N.pft, Cab.pft, Cw.pft, Cm.pft)


# Violin plots by conifer vs. broadleaf -----------------------------------

N.cb <- ggplot(fft.spec) + aes(x = Con_vs_BL, y = N.m) + geom_violin()
Cab.cb <- ggplot(fft.spec) + aes(x = Con_vs_BL, y = Cab.m) + geom_violin()
Cw.cb <- ggplot(fft.spec) + aes(x = Con_vs_BL, y = Cw.m) + geom_violin()
Cm.cb <- ggplot(fft.spec) + aes(x = Con_vs_BL, y = Cm.m) + geom_violin()

grid.arrange(N.cb, Cab.cb, Cw.cb, Cm.cb)

