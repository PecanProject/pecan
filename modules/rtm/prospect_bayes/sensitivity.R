##` PROSPECT Sensitivity Analysis
##'
##' Generates plots showing the contributions of PROSPECT parameters to
##' resulting spectra.

source("prospect.R")

LENGTH <- 10

N.vec <- seq(1, 4, length=LENGTH)
Cab.vec <- seq(20, 80, length=LENGTH)
Cw.vec <- seq(0.01, 0.04, length=LENGTH)
Cm.vec <- seq(0.0001, 0.01, length=LENGTH)

wl.vec <- 400:2500

ps.N <- sapply(N.vec, function(x) prospect4(x, guess.inits["Cab"],
                                            guess.inits["Cw"],
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
)

ps.Cab <- sapply(Cab.vec, function(x) prospect4(guess.inits["N"],
                                                x,
                                            guess.inits["Cw"],
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
)

ps.Cw <- sapply(Cw.vec, function(x) prospect4(guess.inits["N"],
                                              guess.inits["Cab"],
                                            x,
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
)

ps.Cm <- sapply(Cm.vec, function(x) prospect4(guess.inits["N"],
                                              guess.inits["Cab"],
                                            guess.inits["Cw"],
                                            x,
                                            n.a, cab.a, w.a, m.a)
)

plot.sens <- function(mat, TITLE, xlim=c(400,2500)) {
  plot(wl.vec, mat[,1], type='l', col=1,
       xlim=xlim,
       ylim=c(0, max(mat) + 0.05),
       xlab="Wavelength (nm)",
       ylab="Reflectance")
  for (i in 2:LENGTH){
    lines(wl.vec, mat[,i], col=i)
  }
  title(TITLE)
}

plot.sens(ps.N, sprintf("N: %.1f to %.1f", min(N.vec), max(N.vec)))
plot.sens(ps.Cab, sprintf("Cab: %.0f to %.0f", min(Cab.vec), max(Cab.vec)))
plot.sens(ps.Cw, sprintf("Cw: %.2f to %.2f", min(Cw.vec), max(Cw.vec)))
plot.sens(ps.Cm, sprintf("Cm: %.4f to %.4f", min(Cm.vec), max(Cm.vec)))


