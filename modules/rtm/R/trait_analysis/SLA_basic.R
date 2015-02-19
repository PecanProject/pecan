##' SLA formula analysis

source("trait_load.R")

d2 <- FFT.all

### Jacquemoud & Feret 1990
SLA.jf <- function(N) (0.1 * N + 0.025) / (N - 0.9)   # Original function
d2$SLA.jf <- SLA.jf(FFT.all$N)*1000
plot(d2$SLA_cm2_g_DW, d2$SLA.jf, 
     xlim=c(0,700), ylim=c(0,700),
     xlab="Observed SLA (cm2/mg)",
     ylab="Model SLA [f(N)] (cm2/mg)",
     main="Jacquemoud & Feret (1990) SLA = f(N)")
abline(0,1)
abline(lm(d2$SLA.jf ~ d2$SLA_cm2_g_DW), col=2)

N.jf <- function(SLA) (0.9*SLA + 0.025) / (SLA - 0.1)   # Inverted
d2$N.jf <- N.jf(d2$SLA_cm2_g_DW/1000)
d2$N.jf[d2$N.jf > 5 | d2$N.jf < 0] <- NA
plot(d2$N, d2$N.jf, xlim=c(1,4), ylim=c(1,4),
     xlab="Observed N", ylab="Modeled N as f(SLA)",
     main="Jacquemoud & Feret (1990), N = f(SLA)")
abline(0,1)
abline(lm(d2$N.jf ~ d2$N), col=2)


### Veroustraete and Gond (pc)
N.vg <- function(SLA) (SLA - 0.1)^-0.25
d2$N.vg <- N.vg(d2$SLA_cm2_g_DW/1000)
plot(d2$N, d2$N.vg, xlim=c(1,4), ylim=c(1,4),
     xlab="Observed N", ylab="Modeled N as f(SLA)",
     main="Veroustraete and Gond (pc), N = f(SLA)")
abline(0,1)
abline(lm(d2$N.vg ~ d2$N), col=2)
